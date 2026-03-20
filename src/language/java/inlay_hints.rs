use std::ops::Range;
use std::sync::Arc;
use std::{cell::RefCell, collections::HashMap};

use ropey::Rope;
use tree_sitter::Node;
use tree_sitter_utils::{Handler, HandlerExt, Input};

use crate::index::{IndexView, NameTable};
use crate::language::java::editor_semantics::{
    JavaInvocationSite, intersects_range, render_type_for_ui, resolve_invocation,
    semantic_context_at_offset,
};
use crate::language::java::type_ctx::SourceTypeCtx;
use crate::semantic::SemanticContext;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JavaInlayHintKind {
    Type,
    Parameter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JavaInlayHint {
    pub offset: usize,
    pub label: String,
    pub kind: JavaInlayHintKind,
}

pub fn collect_java_inlay_hints(
    source: &str,
    rope: &Rope,
    root: Node,
    name_table: Option<Arc<NameTable>>,
    view: &IndexView,
    byte_range: Range<usize>,
) -> Vec<JavaInlayHint> {
    if byte_range.is_empty() {
        return Vec::new();
    }

    let Some(scan_root) = smallest_relevant_root(root, byte_range.clone()) else {
        return Vec::new();
    };
    let candidates = collect_hint_candidates(source, scan_root, &byte_range);
    if candidates.var_decls.is_empty() && candidates.invocations.is_empty() {
        return Vec::new();
    }

    let semantic_cache = SemanticContextCache::default();
    let mut hints = Vec::new();
    collect_var_hints(
        source,
        rope,
        root,
        name_table.clone(),
        view,
        &candidates.var_decls,
        &semantic_cache,
        &mut hints,
    );
    collect_parameter_hints(
        source,
        rope,
        root,
        name_table,
        view,
        &candidates.invocations,
        &semantic_cache,
        &mut hints,
    );
    hints.sort_by_key(|hint| hint.offset);
    hints
}

pub fn has_java_inlay_hint_candidates(source: &str, root: Node, byte_range: Range<usize>) -> bool {
    if byte_range.is_empty() {
        return false;
    }

    let Some(scan_root) = smallest_relevant_root(root, byte_range.clone()) else {
        return false;
    };
    let candidates = collect_hint_candidates(source, scan_root, &byte_range);
    !candidates.var_decls.is_empty() || !candidates.invocations.is_empty()
}

#[derive(Default)]
struct HintCandidates<'a> {
    var_decls: Vec<Node<'a>>,
    invocations: Vec<Node<'a>>,
}

#[derive(Default)]
struct SemanticContextCache {
    entries: RefCell<HashMap<usize, Arc<SemanticContext>>>,
}

impl SemanticContextCache {
    fn get_or_compute(
        &self,
        source: &str,
        rope: &Rope,
        root: Node,
        offset: usize,
        name_table: Option<Arc<NameTable>>,
        view: &IndexView,
    ) -> Option<Arc<SemanticContext>> {
        if let Some(ctx) = self.entries.borrow().get(&offset).cloned() {
            return Some(ctx);
        }

        let ctx = Arc::new(semantic_context_at_offset(
            source, rope, root, offset, name_table, view,
        )?);
        self.entries.borrow_mut().insert(offset, Arc::clone(&ctx));
        Some(ctx)
    }
}

fn smallest_relevant_root<'a>(root: Node<'a>, byte_range: Range<usize>) -> Option<Node<'a>> {
    let start = byte_range.start.min(root.end_byte());
    let end = byte_range
        .end
        .max(start.saturating_add(1))
        .min(root.end_byte());
    let descendant = root.named_descendant_for_byte_range(start, end)?;

    let mut current = descendant;
    while let Some(parent) = current.parent() {
        if !intersects_range(parent, &byte_range) {
            break;
        }
        if parent.start_byte() < byte_range.start || parent.end_byte() > byte_range.end {
            return Some(parent);
        }
        current = parent;
    }

    Some(current)
}

fn collect_hint_candidates<'a>(
    source: &str,
    node: Node<'a>,
    byte_range: &Range<usize>,
) -> HintCandidates<'a> {
    let mut out = HintCandidates::default();
    collect_hint_candidates_into(source, node, byte_range, &mut out);
    out
}

fn collect_hint_candidates_into<'a>(
    source: &str,
    node: Node<'a>,
    byte_range: &Range<usize>,
    out: &mut HintCandidates<'a>,
) {
    if !intersects_range(node, byte_range) {
        return;
    }

    if is_var_hint_candidate(source, node) {
        out.var_decls.push(node);
    }
    if is_parameter_hint_candidate(node) {
        out.invocations.push(node);
    }

    let mut walker = node.walk();
    for child in node.children(&mut walker) {
        collect_hint_candidates_into(source, child, byte_range, out);
    }
}

fn is_var_hint_candidate(source: &str, node: Node) -> bool {
    node.kind() == "local_variable_declaration"
        && node
            .child_by_field_name("type")
            .and_then(|type_node| type_node.utf8_text(source.as_bytes()).ok())
            == Some("var")
}

fn is_parameter_hint_candidate(node: Node) -> bool {
    invocation_arguments(node)
        .map(named_argument_nodes)
        .is_some_and(|args| !args.is_empty())
}

#[allow(clippy::too_many_arguments)]
fn collect_var_hints(
    source: &str,
    rope: &Rope,
    root: Node,
    name_table: Option<Arc<NameTable>>,
    view: &IndexView,
    candidates: &[Node],
    semantic_cache: &SemanticContextCache,
    out: &mut Vec<JavaInlayHint>,
) {
    for &node in candidates {
        let mut walker = node.walk();
        for declarator in node.named_children(&mut walker) {
            if declarator.kind() != "variable_declarator" {
                continue;
            }
            let Some(name_node) = declarator.child_by_field_name("name") else {
                continue;
            };
            let Some(ctx) = semantic_cache.get_or_compute(
                source,
                rope,
                root,
                name_node.end_byte(),
                name_table.clone(),
                view,
            ) else {
                continue;
            };
            let Some(local) = name_node
                .utf8_text(source.as_bytes())
                .ok()
                .and_then(|name| {
                    ctx.local_variables
                        .iter()
                        .rev()
                        .find(|lv| lv.name.as_ref() == name)
                })
            else {
                continue;
            };
            if local.type_internal.erased_internal() == "var"
                || local.type_internal.erased_internal() == "unknown"
            {
                continue;
            }
            out.push(JavaInlayHint {
                offset: name_node.end_byte(),
                label: format!(": {}", render_type_for_ui(&local.type_internal, view, &ctx)),
                kind: JavaInlayHintKind::Type,
            });
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_parameter_hints(
    source: &str,
    rope: &Rope,
    root: Node,
    name_table: Option<Arc<NameTable>>,
    view: &IndexView,
    candidates: &[Node],
    semantic_cache: &SemanticContextCache,
    out: &mut Vec<JavaInlayHint>,
) {
    for &node in candidates {
        let Some(site) = invocation_site_for_node(source, node) else {
            continue;
        };
        let Some(arguments) = invocation_arguments(node) else {
            continue;
        };
        let arg_nodes = named_argument_nodes(arguments);
        if arg_nodes.is_empty() {
            continue;
        }

        let ctx_offset = arguments.start_byte().saturating_add(1);
        let Some(ctx) =
            semantic_cache.get_or_compute(source, rope, root, ctx_offset, name_table.clone(), view)
        else {
            continue;
        };
        let Some(type_ctx) = ctx.extension::<SourceTypeCtx>() else {
            continue;
        };
        let Some(call) = resolve_invocation(&ctx, view, type_ctx, &site, None) else {
            continue;
        };

        for (arg_index, arg_node) in arg_nodes.into_iter().enumerate() {
            let Some(param_name) = call.parameter_name_for_argument(arg_index) else {
                continue;
            };
            if should_skip_parameter_hint(param_name.as_ref(), arg_node, source) {
                continue;
            }
            out.push(JavaInlayHint {
                offset: arg_node.start_byte(),
                label: format!("{param_name}:"),
                kind: JavaInlayHintKind::Parameter,
            });
        }
    }
}

fn invocation_site_for_node(source: &str, node: Node) -> Option<JavaInvocationSite> {
    // Dispatch on node kind to build the appropriate invocation site.
    let handler = (|inp: Input<&&[u8]>| -> Option<JavaInvocationSite> {
        let bytes = *inp.ctx;
        let method_name = inp
            .node
            .child_by_field_name("name")?
            .utf8_text(bytes)
            .ok()?
            .to_owned();
        let receiver_expr = inp
            .node
            .child_by_field_name("object")
            .and_then(|r| r.utf8_text(bytes).ok())
            .unwrap_or("this")
            .to_owned();
        let arg_texts = invocation_arguments(inp.node)
            .map(named_argument_nodes)
            .unwrap_or_default()
            .into_iter()
            .filter_map(|arg| arg.utf8_text(bytes).ok().map(ToOwned::to_owned))
            .collect();
        Some(JavaInvocationSite::Method {
            receiver_expr,
            method_name,
            arg_texts,
        })
    })
    .for_kinds(&["method_invocation"])
    .or((|inp: Input<&&[u8]>| -> Option<JavaInvocationSite> {
        let bytes = *inp.ctx;
        let call_text = inp.node.utf8_text(bytes).ok()?.to_owned();
        let arg_texts = invocation_arguments(inp.node)
            .map(named_argument_nodes)
            .unwrap_or_default()
            .into_iter()
            .filter_map(|arg| arg.utf8_text(bytes).ok().map(ToOwned::to_owned))
            .collect();
        Some(JavaInvocationSite::Constructor {
            call_text,
            arg_texts,
        })
    })
    .for_kinds(&["object_creation_expression"]));

    let bytes_ref: &[u8] = source.as_bytes();
    handler.handle(Input::new(node, &bytes_ref, None))
}

fn invocation_arguments(node: Node) -> Option<Node> {
    node.child_by_field_name("arguments")
}

fn named_argument_nodes(arguments: Node) -> Vec<Node> {
    let mut walker = arguments.walk();
    arguments.named_children(&mut walker).collect()
}

fn should_skip_parameter_hint(param_name: &str, arg_node: Node, source: &str) -> bool {
    match arg_node.kind() {
        "identifier" => arg_node
            .utf8_text(source.as_bytes())
            .ok()
            .is_some_and(|text| text == param_name),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{collect_java_inlay_hints, has_java_inlay_hint_candidates};
    use crate::index::{
        ClassMetadata, ClassOrigin, IndexScope, MethodParams, MethodSummary, ModuleId,
        WorkspaceIndex,
    };
    use crate::language::java::make_java_parser;
    use ropey::Rope;
    use rust_asm::constants::ACC_PUBLIC;
    use std::sync::Arc;

    fn make_view() -> crate::index::IndexView {
        let idx = Box::leak(Box::new(WorkspaceIndex::new()));
        idx.add_jar_classes(
            IndexScope {
                module: ModuleId::ROOT,
            },
            vec![ClassMetadata {
                package: Some(Arc::from("java/util")),
                name: Arc::from("Objects"),
                internal_name: Arc::from("java/util/Objects"),
                super_name: Some(Arc::from("java/lang/Object")),
                interfaces: vec![],
                annotations: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("requireNonNull"),
                    params: MethodParams::from([("Ljava/lang/Object;", "obj")]),
                    annotations: vec![],
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("Ljava/lang/Object;")),
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                generic_signature: None,
                origin: ClassOrigin::Jar(Arc::from("rt.jar")),
            }],
        );
        idx.view(IndexScope {
            module: ModuleId::ROOT,
        })
    }

    fn parse_root(source: &str) -> (Rope, tree_sitter::Tree) {
        let rope = Rope::from_str(source);
        let mut parser = make_java_parser();
        let tree = parser.parse(source, None).expect("failed to parse Java");
        (rope, tree)
    }

    #[test]
    fn candidate_scan_skips_non_relevant_range() {
        let source = "class Test {\n  void demo() {\n    var value = 1;\n  }\n}\n";
        let (_rope, tree) = parse_root(source);
        let root = tree.root_node();
        let byte_range = source.find("class").unwrap()..source.find("void").unwrap();

        assert!(!has_java_inlay_hint_candidates(source, root, byte_range));
    }

    #[test]
    fn collect_java_inlay_hints_limits_results_to_requested_range() {
        let source = concat!(
            "import java.util.Objects;\n",
            "class Test {\n",
            "  void demo() {\n",
            "    var first = 1;\n",
            "    Objects.requireNonNull(first);\n",
            "    var second = 2;\n",
            "    Objects.requireNonNull(second);\n",
            "  }\n",
            "}\n"
        );
        let view = make_view();
        let (rope, tree) = parse_root(source);
        let root = tree.root_node();

        let range_start = source.find("var second").unwrap();
        let range_end = source.find("  }\n}").unwrap();
        let hints =
            collect_java_inlay_hints(source, &rope, root, None, &view, range_start..range_end);

        let labels: Vec<&str> = hints.iter().map(|hint| hint.label.as_str()).collect();
        assert_eq!(labels, vec![": int"]);
        assert!(
            hints
                .iter()
                .all(|hint| hint.offset >= range_start && hint.offset < range_end)
        );
    }

    #[test]
    fn candidate_scan_detects_parameter_hint_range() {
        let source = concat!(
            "import java.util.Objects;\n",
            "class Test {\n",
            "  void demo(String value) {\n",
            "    Objects.requireNonNull(value);\n",
            "  }\n",
            "}\n"
        );
        let (_rope, tree) = parse_root(source);
        let root = tree.root_node();
        let start = source.find("Objects.requireNonNull").unwrap();
        let end = source.find(");\n").unwrap() + 1;

        assert!(has_java_inlay_hint_candidates(source, root, start..end));
    }
}
