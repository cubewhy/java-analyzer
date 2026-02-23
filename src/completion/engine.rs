use std::sync::Arc;

use super::context::CursorLocation;
use super::type_resolver::TypeResolver;
use super::{
    candidate::CompletionCandidate,
    context::CompletionContext,
    providers::{
        CompletionProvider, constructor::ConstructorProvider, import::ImportProvider,
        keyword::KeywordProvider, local_var::LocalVarProvider, member::MemberProvider,
        static_member::StaticMemberProvider,
    },
    scorer::Scorer,
};
use crate::completion::LocalVar;
use crate::completion::import_utils::resolve_simple_to_internal;
use crate::completion::providers::annotation::AnnotationProvider;
use crate::completion::providers::expression::ExpressionProvider;
use crate::completion::providers::package::PackageProvider;
use crate::completion::providers::snippet::SnippetProvider;
use crate::completion::providers::this_member::ThisMemberProvider;
use crate::completion::type_resolver::ChainSegment;
use crate::index::GlobalIndex;

pub struct CompletionEngine {
    providers: Vec<Box<dyn CompletionProvider>>,
}

impl CompletionEngine {
    pub fn new() -> Self {
        Self {
            providers: vec![
                Box::new(LocalVarProvider), // Highest priority: local variables
                Box::new(ThisMemberProvider),
                Box::new(MemberProvider),       // obj.xxx
                Box::new(StaticMemberProvider), // Cls.xxx
                Box::new(ConstructorProvider),  // new Xxx
                Box::new(PackageProvider),
                Box::new(ExpressionProvider), // expression/type position: class name
                Box::new(ImportProvider),     // import statement
                Box::new(KeywordProvider),    // Keyword (triggered only upon input)
                Box::new(AnnotationProvider),
                Box::new(SnippetProvider), // Snippets
            ],
        }
    }

    pub fn register_provider(&mut self, provider: Box<dyn CompletionProvider>) {
        self.providers.push(provider);
    }

    pub fn complete(
        &self,
        mut ctx: CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        // infer type
        self.enrich_context(&mut ctx, index);

        let mut candidates: Vec<CompletionCandidate> = self
            .providers
            .iter()
            .flat_map(|p| p.provide(&ctx, index))
            .collect();

        // score
        let scorer = Scorer::new(&ctx.query);
        for c in &mut candidates {
            c.score += scorer.score(c);
        }

        candidates = dedup(candidates);

        // sort results
        candidates.sort_unstable_by(|a, b| {
            b.score
                .partial_cmp(&a.score)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| a.label.cmp(&b.label))
        });

        candidates
    }

    fn enrich_context(&self, ctx: &mut CompletionContext, index: &GlobalIndex) {
        if let CursorLocation::MemberAccess {
            receiver_type,
            receiver_expr,
            ..
        } = &mut ctx.location
            && receiver_type.is_none()
            && !receiver_expr.is_empty()
        {
            let resolver = TypeResolver::new(index);
            let chain = parse_chain_from_expr(receiver_expr);
            let resolved = if chain.is_empty() {
                resolver.resolve(
                    receiver_expr,
                    &ctx.local_variables,
                    ctx.enclosing_internal_name.as_ref(),
                )
            } else {
                resolver.resolve_chain(
                    &chain,
                    &ctx.local_variables,
                    ctx.enclosing_internal_name.as_ref(),
                )
            };

            // If the result is a simple name (without '/'), it needs to be further parsed into an internal name.
            *receiver_type = match resolved.as_deref() {
                None => None,
                Some(ty) if ty.contains('/') => resolved,
                Some(ty) => resolve_simple_to_internal(
                    ty,
                    &ctx.existing_imports,
                    ctx.enclosing_package.as_deref(),
                    index,
                ),
            };
        }

        // Resolve `var` local variables
        {
            let resolver = TypeResolver::new(index);

            // First pass: collect indices and init_exprs that need resolution
            let to_resolve: Vec<(usize, String)> = ctx
                .local_variables
                .iter()
                .enumerate()
                .filter_map(|(i, lv)| {
                    if lv.type_internal.as_ref() == "var" {
                        lv.init_expr.as_deref().map(|e| (i, e.to_string()))
                    } else {
                        None
                    }
                })
                .collect();

            // Second pass: resolve each one using a snapshot of locals
            let locals_snapshot: Vec<LocalVar> = ctx.local_variables.clone();
            for (idx_in_vec, init_expr) in to_resolve {
                if let Some(resolved) = resolve_var_init_expr(
                    &init_expr,
                    &locals_snapshot,
                    ctx.enclosing_internal_name.as_ref(),
                    &resolver,
                    &ctx.existing_imports,
                    ctx.enclosing_package.as_deref(),
                    index,
                ) {
                    ctx.local_variables[idx_in_vec].type_internal = resolved;
                }
            }
        }
    }
}

fn resolve_var_init_expr(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    existing_imports: &[String],
    enclosing_package: Option<&str>,
    index: &GlobalIndex,
) -> Option<Arc<str>> {
    let expr = expr.trim();

    // Constructor: "new Foo(...)" → type is Foo
    if let Some(rest) = expr.strip_prefix("new ") {
        let class_name = rest.split('(').next()?.split('<').next()?.trim();
        return crate::completion::import_utils::resolve_simple_to_internal(
            class_name,
            existing_imports,
            enclosing_package,
            index,
        );
    }

    // Method call chain: "receiver.method(args)" or "method(args)"
    // Parse using existing chain parser
    let chain = crate::completion::engine::parse_chain_from_expr(expr);
    if !chain.is_empty() {
        // resolve_chain needs the receiver type first
        let mut current: Option<Arc<str>> = None;
        for (i, seg) in chain.iter().enumerate() {
            if i == 0 {
                if seg.arg_count.is_some() {
                    let recv_internal = enclosing_internal?;
                    let arg_types: Vec<Arc<str>> = seg
                        .arg_texts
                        .iter()
                        .filter_map(|t| resolver.resolve(t.trim(), locals, enclosing_internal))
                        .collect();
                    let arg_types_ref: &[Arc<str>] = if arg_types.len() == seg.arg_texts.len() {
                        &arg_types
                    } else {
                        &[]
                    };
                    current = resolver.resolve_method_return(
                        recv_internal.as_ref(),
                        &seg.name,
                        seg.arg_count.unwrap_or(-1),
                        arg_types_ref,
                    );
                } else {
                    // variable or class name lookup — unchanged
                    current = resolver.resolve(&seg.name, locals, enclosing_internal);
                    if current.is_none() {
                        current = resolve_simple_to_internal(
                            &seg.name,
                            existing_imports,
                            enclosing_package,
                            index,
                        );
                    }
                }
            } else {
                let recv = current.as_deref()?;
                let recv_full = if recv.contains('/') || index.get_class(recv).is_some() {
                    Arc::from(recv)
                } else {
                    crate::completion::import_utils::resolve_simple_to_internal(
                        recv,
                        existing_imports,
                        enclosing_package,
                        index,
                    )?
                };

                // Resolve arg_texts → arg_types for overload disambiguation
                let arg_types: Vec<Arc<str>> = seg
                    .arg_texts
                    .iter()
                    .filter_map(|t| resolver.resolve(t.trim(), locals, enclosing_internal))
                    .collect();
                // Only use type matching if all args were successfully resolved
                let arg_types_ref: &[Arc<str>] = if arg_types.len() == seg.arg_texts.len() {
                    &arg_types
                } else {
                    &[]
                };

                current = resolver.resolve_method_return(
                    &recv_full,
                    &seg.name,
                    seg.arg_count.unwrap_or(-1),
                    arg_types_ref,
                );
            }
        }
        if current.is_some() {
            return current;
        }
    }

    None
}

impl Default for CompletionEngine {
    fn default() -> Self {
        Self::new()
    }
}

fn dedup(mut candidates: Vec<CompletionCandidate>) -> Vec<CompletionCandidate> {
    candidates.sort_unstable_by(|a, b| {
        a.label
            .cmp(&b.label)
            .then_with(|| a.source.cmp(b.source))
            .then_with(|| {
                a.required_import
                    .is_some()
                    .cmp(&b.required_import.is_some())
            })
            .then_with(|| {
                b.score
                    .partial_cmp(&a.score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    });

    let mut result: Vec<CompletionCandidate> = Vec::with_capacity(candidates.len());
    for c in candidates {
        let duplicate = result.last().is_some_and(|last| {
            last.label == c.label
                && std::mem::discriminant(&last.kind) == std::mem::discriminant(&c.kind)
        });
        if !duplicate {
            result.push(c);
        }
    }
    result
}

/// Roughly parse the expression string into a call chain
/// "list.stream().filter" → [variable("list"), method("stream", 0), variable("filter")]
pub(crate) fn parse_chain_from_expr(expr: &str) -> Vec<ChainSegment> {
    let mut segments = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    let mut in_method = false;
    let mut arg_start = 0usize; // byte position of current arg start
    let mut arg_texts: Vec<String> = Vec::new();

    for (char_pos, ch) in expr.char_indices() {
        match ch {
            '(' => {
                depth += 1;
                if depth == 1 {
                    in_method = true;
                    arg_start = char_pos + 1;
                    arg_texts = Vec::new();
                }
            }
            ')' => {
                depth -= 1;
                if depth == 0 && in_method {
                    // Collect last arg
                    let arg = expr[arg_start..char_pos].trim();
                    let has_any = !arg.is_empty();
                    if has_any {
                        arg_texts.push(arg.to_string());
                    }
                    let arg_count = if arg_texts.is_empty() {
                        0
                    } else {
                        arg_texts.len() as i32
                    };
                    segments.push(ChainSegment::method_with_types(
                        current.trim(),
                        arg_count,
                        vec![], // arg_types filled later by resolver
                        arg_texts.clone(),
                    ));
                    current = String::new();
                    arg_texts = Vec::new();
                    in_method = false;
                }
            }
            ',' if depth == 1 => {
                let arg = expr[arg_start..char_pos].trim();
                arg_texts.push(arg.to_string());
                arg_start = char_pos + 1;
            }
            '.' if depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() && !in_method {
                    segments.push(ChainSegment::variable(trimmed));
                }
                current = String::new();
            }
            c => {
                if depth == 0 {
                    current.push(c);
                }
            }
        }
    }
    segments
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use rust_asm::constants::ACC_PUBLIC;

    use crate::{
        completion::LocalVar,
        index::{ClassMetadata, ClassOrigin, MethodSummary},
    };

    use super::*;

    fn seg_names(expr: &str) -> Vec<(String, Option<i32>)> {
        parse_chain_from_expr(expr)
            .into_iter()
            .map(|s| (s.name, s.arg_count))
            .collect()
    }

    #[test]
    fn test_chain_simple_variable() {
        // "list.ge" → [variable("list")]，"ge" is member_prefix which should be dropped
        assert_eq!(seg_names("list.ge"), vec![("list".into(), None)]);
    }

    #[test]
    fn test_chain_method_call() {
        // "list.stream().fi" → [variable("list"), method("stream", 0)]
        assert_eq!(
            seg_names("list.stream().fi"),
            vec![("list".into(), None), ("stream".into(), Some(0)),]
        );
    }

    #[test]
    fn test_chain_method_with_args() {
        // "map.get(key).toStr" → [variable("map"), method("get", 1)]
        assert_eq!(
            seg_names("map.get(key).toStr"),
            vec![("map".into(), None), ("get".into(), Some(1)),]
        );
    }

    #[test]
    fn test_chain_multiple_methods() {
        // "a.b().c(x, y).d" → [var("a"), method("b",0), method("c",2)]
        assert_eq!(
            seg_names("a.b().c(x, y).d"),
            vec![
                ("a".into(), None),
                ("b".into(), Some(0)),
                ("c".into(), Some(2)),
            ]
        );
    }

    #[test]
    fn test_chain_no_dot() {
        // "someVar" -> [], the whole thing is member_prefix
        assert_eq!(seg_names("someVar"), vec![]);
    }

    #[test]
    fn test_chain_nested_parens() {
        // "list.get(map.size()).toStr" → [var("list"), method("get", 1)]
        // The comma inside nested parentheses should not be counted in the outer arg_count.
        assert_eq!(
            seg_names("list.get(map.size()).toStr"),
            vec![("list".into(), None), ("get".into(), Some(1)),]
        );
    }

    fn make_index_with_random_class() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy")),
            name: Arc::from("RandomClass"),
            internal_name: Arc::from("org/cubewhy/RandomClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("f"),
                descriptor: Arc::from("()V"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: None,
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        idx
    }

    #[test]
    fn test_enrich_context_resolves_simple_name_via_import() {
        let idx = make_index_with_random_class();
        let engine = CompletionEngine::new();

        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "f".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "f",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: Arc::from("RandomClass"), // simple name
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".to_string()],
        );

        engine.enrich_context(&mut ctx, &idx);

        // After enriching, receiver_type should be the fully qualified internal name.
        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(
                receiver_type.as_deref(),
                Some("org/cubewhy/RandomClass"),
                "receiver_type should be fully qualified after enrich"
            );
        }
    }

    #[test]
    fn test_enrich_context_resolves_simple_name_via_wildcard_import() {
        let idx = make_index_with_random_class();
        let engine = CompletionEngine::new();

        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: Arc::from("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.*".to_string()],
        );

        engine.enrich_context(&mut ctx, &idx);

        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(receiver_type.as_deref(), Some("org/cubewhy/RandomClass"),);
        }
    }

    #[test]
    fn test_complete_returns_f_method() {
        let mut idx = make_index_with_random_class();
        let engine = CompletionEngine::new();

        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "f".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "f",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: Arc::from("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".to_string()],
        );

        let results = engine.complete(ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "f"),
            "should find method f(): {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_dedup_prefers_no_import_candidate() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;

        let with_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        )
        .with_import("org.cubewhy.RandomClass");

        let without_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        );

        // Two candidates, one with import and one without.
        let result = dedup(vec![with_import, without_import]);

        assert_eq!(result.len(), 1, "should dedup to one candidate");
        assert!(
            result[0].required_import.is_none(),
            "dedup should prefer the candidate without required_import, got: {:?}",
            result[0].required_import
        );
    }

    #[test]
    fn test_expected_type_ranks_first_in_constructor_completion() {
        use crate::completion::context::CursorLocation;
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        // Add Main, Main2, RandomClass
        for (pkg, name) in [
            ("org/cubewhy/a", "Main"),
            ("org/cubewhy/a", "Main2"),
            ("org/cubewhy", "RandomClass"),
        ] {
            idx.add_classes(vec![ClassMetadata {
                package: Some(Arc::from(pkg)),
                name: Arc::from(name),
                internal_name: Arc::from(format!("{}/{}", pkg, name)),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("<init>"),
                    descriptor: Arc::from("()V"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: None,
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            }]);
        }

        let engine = CompletionEngine::new();
        let ctx = CompletionContext::new(
            CursorLocation::ConstructorCall {
                class_prefix: String::new(),
                expected_type: Some("RandomClass".to_string()),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".to_string()],
        );

        let results = engine.complete(ctx, &mut idx);

        assert!(!results.is_empty(), "should have candidates");
        assert_eq!(
            results[0].label.as_ref(),
            "RandomClass",
            "RandomClass should rank first when it matches expected_type, got: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_var_method_return_type_resolved() {
        use crate::completion::context::{CursorLocation, LocalVar};
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        // NestedClass with randomFunction() returning NestedClass
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("NestedClass"),
            internal_name: Arc::from("NestedClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("randomFunction"),
                descriptor: Arc::from("(Ljava/lang/String;)LNestedClass;"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: Some(Arc::from("NestedClass")),
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("nc"),
                    type_internal: Arc::from("NestedClass"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("str"),
                    type_internal: Arc::from("var"),
                    init_expr: Some("nc.randomFunction()".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );

        engine.enrich_context(&mut ctx, &idx);

        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(
            str_var.type_internal.as_ref(),
            "NestedClass",
            "var str should be resolved to NestedClass via method return type"
        );
    }

    #[test]
    fn test_var_overload_resolved_by_long_arg() {
        // var str = nc.randomFunction("a", 1l) → Main2
        use crate::completion::context::{CursorLocation, LocalVar};
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        for (name, desc, ret) in [
            (
                "randomFunction",
                "(Ljava/lang/String;I)LRandomClass;",
                "RandomClass",
            ),
            ("randomFunction", "(Ljava/lang/String;J)LMain2;", "Main2"),
        ] {
            // add to same class — need one add_classes call
        }
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("NestedClass"),
            internal_name: Arc::from("NestedClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![
                MethodSummary {
                    name: Arc::from("randomFunction"),
                    descriptor: Arc::from("(Ljava/lang/String;I)LRandomClass;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("RandomClass")),
                },
                MethodSummary {
                    name: Arc::from("randomFunction"),
                    descriptor: Arc::from("(Ljava/lang/String;J)LMain2;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("Main2")),
                },
            ],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("nc"),
                    type_internal: Arc::from("NestedClass"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("str"),
                    type_internal: Arc::from("var"),
                    // "a" resolves to String, "1l" resolves to long
                    init_expr: Some("nc.randomFunction(\"a\", 1l)".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );

        engine.enrich_context(&mut ctx, &idx);

        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(
            str_var.type_internal.as_ref(),
            "Main2",
            "long arg should select Main2 overload, got: {}",
            str_var.type_internal
        );
    }

    #[test]
    fn test_var_bare_method_call_resolved() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("Main"),
            internal_name: Arc::from("Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("getString"),
                descriptor: Arc::from("()Ljava/lang/String;"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: Some(Arc::from("java/lang/String")),
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("str"),
                type_internal: Arc::from("var"),
                init_expr: Some("getString()".to_string()),
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("Main")), // enclosing_internal_name
            None,
            vec![],
        );

        engine.enrich_context(&mut ctx, &idx);

        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(
            str_var.type_internal.as_ref(),
            "java/lang/String",
            "var str should resolve to String via bare method call return type"
        );
    }

    #[test]
    fn test_resolve_method_return_walks_mro() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        // Parent has the method
        idx.add_classes(vec![
            ClassMetadata {
                package: None,
                name: Arc::from("Parent"),
                internal_name: Arc::from("Parent"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("getValue"),
                    descriptor: Arc::from("()Ljava/lang/String;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("java/lang/String")),
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
            ClassMetadata {
                package: None,
                name: Arc::from("Child"),
                internal_name: Arc::from("Child"),
                super_name: Some("Parent".to_string()),
                interfaces: vec![],
                methods: vec![], // Child has no methods of its own
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
        ]);

        let resolver = TypeResolver::new(&idx);
        let result = resolver.resolve_method_return("Child", "getValue", 0, &[]);
        assert_eq!(
            result.as_deref(),
            Some("java/lang/String"),
            "should find getValue() in Parent via MRO"
        );
    }

    #[test]
    fn test_complete_member_after_bare_method_call() {
        use crate::completion::context::{CompletionContext, CursorLocation};
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            ClassMetadata {
                package: None,
                name: Arc::from("Main"),
                internal_name: Arc::from("Main"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("getMain2"),
                    descriptor: Arc::from("()LMain2;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("Main2")),
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
            ClassMetadata {
                package: None,
                name: Arc::from("Main2"),
                internal_name: Arc::from("Main2"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("func"),
                    descriptor: Arc::from("()V"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: None,
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
        ]);

        let engine = CompletionEngine::new();
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "getMain2()".to_string(),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("Main")), // enclosing_internal_name
            None,
            vec![],
        );

        let results = engine.complete(ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "func"),
            "getMain2().| should complete with func from Main2: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }
}
