use crate::language::java::utils::{
    find_ancestor, is_comment_kind, is_in_name_position, is_in_type_position,
};
use tree_sitter::Node;

use crate::{
    completion::CursorLocation,
    language::java::{JavaContextExtractor, utils::strip_sentinel},
};

pub(crate) fn determine_location(
    ctx: &JavaContextExtractor,
    cursor_node: Option<Node>,
    trigger_char: Option<char>,
) -> (CursorLocation, String) {
    let (loc, query) = determine_location_impl(ctx, cursor_node, trigger_char);
    if location_has_newline(&loc) {
        return (CursorLocation::Unknown, String::new());
    }
    (loc, query)
}

fn determine_location_impl(
    ctx: &JavaContextExtractor,
    cursor_node: Option<Node>,
    trigger_char: Option<char>,
) -> (CursorLocation, String) {
    let node = match cursor_node {
        Some(n) => n,
        None => return (CursorLocation::Unknown, String::new()),
    };
    let mut current = node;
    loop {
        match current.kind() {
            "marker_annotation" | "annotation" => {
                let name_node = current.child_by_field_name("name");
                let prefix = name_node
                    .map(|n| cursor_truncated_text(ctx, n))
                    .unwrap_or_default();
                return (
                    CursorLocation::Annotation {
                        prefix: prefix.clone(),
                    },
                    prefix,
                );
            }
            "import_declaration" => return handle_import(ctx, current),
            "method_invocation" => return handle_member_access(ctx, current),
            "field_access" => return handle_member_access(ctx, current),
            "object_creation_expression" => return handle_constructor(ctx, current),
            "argument_list" => return handle_argument_list(ctx, current),
            "identifier" | "type_identifier" => {
                return handle_identifier(ctx, current, trigger_char);
            }
            _ => {}
        }
        match current.parent() {
            Some(p) => current = p,
            None => break,
        }
    }
    (CursorLocation::Unknown, String::new())
}

fn handle_import(ctx: &JavaContextExtractor, node: Node) -> (CursorLocation, String) {
    let mut raw_prefix = String::new();
    collect_import_text(ctx, node, &mut raw_prefix);
    let prefix = strip_sentinel(&raw_prefix);
    let query = prefix.rsplit('.').next().unwrap_or("").to_string();

    (CursorLocation::Import { prefix }, query)
}

/// Recursively collect valid text (identifiers, dots, asterisks) in import declarations
/// Skip import keywords, static keywords, semicolons, and all types of comments
fn collect_import_text(ctx: &JavaContextExtractor, node: Node, out: &mut String) {
    // If the node's starting position is already after the cursor, skip it (because we need the part before the cursor).
    if node.start_byte() >= ctx.offset {
        return;
    }

    // If there are no child nodes, it means it is a leaf node (Token)
    if node.child_count() == 0 {
        let kind = node.kind();
        // 过滤掉不需要的 Token
        if kind == "import" || kind == "static" || kind == ";" || is_comment_kind(kind) {
            return;
        }

        // Get the text truncated to the cursor position
        // Note: cursor_truncated_text internally processes node.end_byte().min(ctx.offset)
        let text = cursor_truncated_text(ctx, node);
        out.push_str(&text);
    } else {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            collect_import_text(ctx, child, out);
        }
    }
}

fn handle_member_access(ctx: &JavaContextExtractor, node: Node) -> (CursorLocation, String) {
    let mut walker = node.walk();
    let children: Vec<Node> = node.children(&mut walker).collect();
    let dot_pos = match children.iter().position(|n| n.kind() == ".") {
        Some(p) => p,
        None => {
            let name_node = children
                .iter()
                .find(|n| n.kind() == "identifier" || n.kind() == "type_identifier");
            let text = name_node
                .map(|n| cursor_truncated_text(ctx, *n))
                .unwrap_or_default();
            return (
                CursorLocation::Expression {
                    prefix: text.clone(),
                },
                text,
            );
        }
    };

    let member_node = children[dot_pos + 1..]
        .iter()
        .find(|n| n.kind() == "identifier" || n.kind() == "type_identifier");

    let member_prefix = match member_node {
        Some(mn) => {
            let s = mn.start_byte();
            let e = mn.end_byte();
            let raw = if ctx.offset <= s {
                String::new()
            } else if ctx.offset < e {
                ctx.source[s..ctx.offset].to_string()
            } else {
                ctx.node_text(*mn).to_string()
            };
            strip_sentinel(&raw)
        }
        // This path won't be reached if the path is injected; in non-injection paths, there's nothing after `dot`.
        None => String::new(),
    };

    let receiver_node = if dot_pos > 0 {
        Some(children[dot_pos - 1])
    } else {
        None
    };
    let receiver_expr = receiver_node
        .map(|n| ctx.node_text(n).to_string())
        .unwrap_or_default();

    (
        CursorLocation::MemberAccess {
            receiver_type: None,
            member_prefix: member_prefix.clone(),
            receiver_expr,
        },
        member_prefix,
    )
}

fn handle_constructor(ctx: &JavaContextExtractor, node: Node) -> (CursorLocation, String) {
    let type_node = node.child_by_field_name("type");

    // If the type node starts after the cursor and is separated by a newline,
    // it means tree-sitter consumed the next line as the type.
    // We reject this and return Unknown to trigger the injection path.
    if let Some(ty) = type_node
        && ctx.offset < ty.start_byte()
    {
        let gap = &ctx.source[ctx.offset..ty.start_byte()];
        if gap.contains('\n') {
            return (CursorLocation::Unknown, String::new());
        }
    }

    let class_prefix = type_node
        .map(|n| {
            // Use cursor_truncated_text so we don't capture text *after* the cursor
            let raw = cursor_truncated_text(ctx, n);
            strip_sentinel(&raw)
        })
        .unwrap_or_default();

    let expected_type = infer_expected_type_from_lhs(ctx, node);
    (
        CursorLocation::ConstructorCall {
            class_prefix: class_prefix.clone(),
            expected_type,
        },
        class_prefix,
    )
}

fn handle_identifier(
    ctx: &JavaContextExtractor,
    node: Node,
    _trigger_char: Option<char>,
) -> (CursorLocation, String) {
    let mut ancestor = node;
    loop {
        ancestor = match ancestor.parent() {
            Some(p) => p,
            None => break,
        };
        match ancestor.kind() {
            "field_access" | "method_invocation" => {
                return handle_member_access(ctx, ancestor);
            }
            "import_declaration" => return handle_import(ctx, ancestor),
            "object_creation_expression" => return handle_constructor(ctx, ancestor),
            "local_variable_declaration" => {
                // Detect misreading: If the next sibling begins with `(`,
                // this indicates that `str\nfunc(...)` was misread as local_variable_declaration,
                // the "type" of the cursor is actually an expression.
                if let Some(next) = ancestor.next_sibling() {
                    let next_text = &ctx.source[next.start_byte()..next.end_byte()];
                    if next_text.trim_start().starts_with('(') {
                        let text = cursor_truncated_text(ctx, node);
                        let clean = strip_sentinel(&text);
                        return (
                            CursorLocation::Expression {
                                prefix: clean.clone(),
                            },
                            clean,
                        );
                    }
                }

                if is_in_type_position(node, ancestor) {
                    let text = cursor_truncated_text(ctx, node);
                    return (
                        CursorLocation::TypeAnnotation {
                            prefix: text.clone(),
                        },
                        text,
                    );
                }

                if is_in_name_position(node, ancestor) {
                    let type_name = extract_type_from_decl(ctx, ancestor);
                    return (CursorLocation::VariableName { type_name }, String::new());
                }

                if is_in_type_subtree(node, ancestor) {
                    // 取光标前的文本作为 prefix，但只取最后一段（点后面的部分）
                    // let text = cursor_truncated_text(ctx, node);
                    // let clean = strip_sentinel(&text);
                    // 触发注入路径来正确识别
                    return (CursorLocation::Unknown, String::new());
                }
                let text = cursor_truncated_text(ctx, node);
                let clean = strip_sentinel(&text);
                return (
                    CursorLocation::Expression {
                        prefix: clean.clone(),
                    },
                    clean,
                );
            }
            "formal_parameter" => {
                if is_in_formal_param_name_position(node, ancestor) {
                    let type_name = ancestor
                        .child_by_field_name("type")
                        .map(|n| {
                            ctx.node_text(n)
                                .split('<')
                                .next()
                                .unwrap_or("")
                                .trim()
                                .to_string()
                        })
                        .unwrap_or_default();
                    return (CursorLocation::VariableName { type_name }, String::new());
                }

                // type position
                let text = cursor_truncated_text(ctx, node);
                return (
                    CursorLocation::TypeAnnotation {
                        prefix: text.clone(),
                    },
                    text,
                );
            }
            "argument_list" => {
                if let Some(parent) = node.parent()
                    && parent.kind() == "method_invocation"
                    && ctx.offset <= ancestor.start_byte()
                {
                    return handle_member_access(ctx, parent);
                }
                return handle_argument_list(ctx, ancestor);
            }
            // If inside ERROR node, return Unknown to trigger injection path
            "ERROR" => return (CursorLocation::Unknown, String::new()),
            "block" | "class_body" | "program" => break,
            _ => {}
        }
    }
    let text = cursor_truncated_text(ctx, node);
    let clean = strip_sentinel(&text);
    (
        CursorLocation::Expression {
            prefix: clean.clone(),
        },
        clean,
    )
}

fn is_in_formal_param_name_position(id_node: Node, param_node: Node) -> bool {
    param_node
        .child_by_field_name("name")
        .is_some_and(|n| n.id() == id_node.id())
}

fn handle_argument_list(ctx: &JavaContextExtractor, node: Node) -> (CursorLocation, String) {
    // Find the nearest identifier node before the cursor in the argument_list as the prefix
    let prefix = find_prefix_in_argument_list(ctx, node);
    (
        CursorLocation::MethodArgument {
            prefix: prefix.clone(),
        },
        prefix,
    )
}

fn find_prefix_in_argument_list(ctx: &JavaContextExtractor, arg_list: Node) -> String {
    // Traverse the child nodes of argument_list and find the one containing the cursor
    let mut cursor = arg_list.walk();
    for child in arg_list.named_children(&mut cursor) {
        if child.start_byte() <= ctx.offset && child.end_byte() >= ctx.offset.saturating_sub(1) {
            let text = cursor_truncated_text(ctx, child);
            let clean = strip_sentinel(&text);
            return clean;
        }
    }
    String::new()
}

fn location_has_newline(loc: &CursorLocation) -> bool {
    match loc {
        CursorLocation::ConstructorCall { class_prefix, .. } => class_prefix.contains('\n'),
        CursorLocation::MemberAccess { member_prefix, .. } => member_prefix.contains('\n'),
        CursorLocation::Expression { prefix } => prefix.contains('\n'),
        CursorLocation::MethodArgument { prefix } => prefix.contains('\n'),
        CursorLocation::TypeAnnotation { prefix } => prefix.contains('\n'),
        CursorLocation::Annotation { prefix } => prefix.contains('\n'),
        CursorLocation::Import { prefix } => prefix.contains('\n'),
        _ => false,
    }
}

fn cursor_truncated_text(ctx: &JavaContextExtractor, node: Node) -> String {
    let start = node.start_byte();
    let end = node.end_byte().min(ctx.offset);
    if end <= start {
        return String::new();
    }
    ctx.source[start..end].to_string()
}

fn extract_type_from_decl(ctx: &JavaContextExtractor, decl_node: Node) -> String {
    let mut walker = decl_node.walk();
    for child in decl_node.named_children(&mut walker) {
        if child.kind() == "modifiers" {
            continue;
        }
        return ctx
            .node_text(child)
            .split('<')
            .next()
            .unwrap_or("")
            .trim()
            .to_string();
    }
    String::new()
}

fn is_in_type_subtree(id_node: Node, decl_node: Node) -> bool {
    // Find the type child node of decl_node and check if id_node is in its descendants.
    let mut walker = decl_node.walk();
    for child in decl_node.named_children(&mut walker) {
        if child.kind() == "modifiers" {
            continue;
        }
        // The first non-modifiers child node is a type node
        return is_descendant_of(id_node, child);
    }
    false
}

fn is_descendant_of(node: Node, ancestor: Node) -> bool {
    let mut cur = node;
    loop {
        if cur.id() == ancestor.id() {
            return true;
        }
        match cur.parent() {
            Some(p) => cur = p,
            None => return false,
        }
    }
}

fn infer_expected_type_from_lhs(ctx: &JavaContextExtractor, node: Node) -> Option<String> {
    let decl = find_ancestor(node, "local_variable_declaration")?;
    let mut walker = decl.walk();
    for child in decl.named_children(&mut walker) {
        if child.kind() == "modifiers" {
            continue;
        }
        let ty = ctx.node_text(child);
        let simple = ty.split('<').next()?.trim();
        if !simple.is_empty() {
            return Some(simple.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use ropey::Rope;
    use tree_sitter::Parser;

    use crate::{
        completion::CursorLocation,
        language::java::{JavaContextExtractor, location::determine_location},
    };

    fn setup_with(source: &'_ str, offset: usize) -> (JavaContextExtractor<'_>, tree_sitter::Tree) {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        let tree = parser.parse(source, None).unwrap();
        let ctx = JavaContextExtractor {
            source,
            bytes: source.as_bytes(),
            offset,
            rope: Rope::from_str(source),
        };
        (ctx, tree)
    }

    #[test]
    fn test_misread_type_as_expression() {
        // str 缺分号，TS 把 `str\nfunc(...)` 误读为
        // local_variable_declaration(type=str, declarator=func)
        // cursor 在 str 末尾时应该得到 Expression 而非 TypeAnnotation
        let src = indoc::indoc! {r#"
    class A {
        public static String str = "1234";
        public static void func() {
            str
            func(func("1234", 5678));
        }
    }
    "#};
        // offset 紧贴 str 末尾（方法体内那个 str，不是字段声明里的）
        let marker = "func() {\n        str";
        let offset = src.find(marker).unwrap() + marker.len();
        let (ctx, tree) = setup_with(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let (loc, query) = determine_location(&ctx, cursor_node, None);

        assert!(
            matches!(loc, CursorLocation::Expression { .. }),
            "Expected Expression (str is an expression, not a type annotation), got {:?}",
            loc
        );
        assert_eq!(query, "str");
    }

    #[test]
    fn test_genuine_type_annotation_in_local_decl() {
        // 正常的 local_variable_declaration，cursor 在 type 上，不能被误读检测误伤
        // 用 `HashM` 作为类型前缀，后面跟 `map =` 确保 TS 解析为 local_variable_declaration
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            HashM map = null;
        }
    }
    "#};
        let offset = src.find("HashM").unwrap() + 5; // 紧贴 HashM 末尾
        let (ctx, tree) = setup_with(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let (loc, query) = determine_location(&ctx, cursor_node, None);

        assert!(
            matches!(loc, CursorLocation::TypeAnnotation { .. }),
            "Expected TypeAnnotation for genuine type position, got {:?}",
            loc
        );
        assert_eq!(query, "HashM");
    }

    #[test]
    fn test_misread_not_triggered_when_next_sibling_is_normal_statement() {
        // next sibling 不以 `(` 开头时，误读检测不应触发
        // 保证普通 local_variable_declaration 里 type 位置的补全正常
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            ArrayL list = new ArrayList<>();
            int x = 1;
        }
    }
    "#};
        let offset = src.find("ArrayL").unwrap() + 6;
        let (ctx, tree) = setup_with(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let (loc, _) = determine_location(&ctx, cursor_node, None);

        assert!(
            matches!(loc, CursorLocation::TypeAnnotation { .. }),
            "Expected TypeAnnotation, got {:?}",
            loc
        );
    }
}
