use rust_asm::constants::ACC_STATIC;

use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
    scorer::{self, AccessFilter},
};
use super::CompletionProvider;
use crate::{completion::fuzzy, index::GlobalIndex};
use std::sync::Arc;

pub struct MemberProvider;

impl CompletionProvider for MemberProvider {
    fn name(&self) -> &'static str {
        "member"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let (receiver_type, member_prefix, receiver_expr) = match &ctx.location {
            CursorLocation::MemberAccess {
                receiver_type,
                member_prefix,
                receiver_expr,
            } => (
                receiver_type.as_ref(),
                member_prefix.as_str(),
                receiver_expr.as_str(),
            ),
            _ => return vec![],
        };

        tracing::debug!(
            receiver_expr,
            member_prefix,
            locals = ?ctx.local_variables.iter().map(|lv| format!("{}:{}", lv.name, lv.type_internal)).collect::<Vec<_>>(),
            imports = ?ctx.existing_imports,
            "MemberProvider.provide"
        );

        if receiver_expr == "this" && !ctx.current_class_members.is_empty() {
            return self.provide_from_source_members(ctx, member_prefix);
        }

        let resolved: Option<Arc<str>> = receiver_type.map(Arc::clone).or_else(|| {
            let r = resolve_receiver_type(receiver_expr, ctx, index);
            tracing::debug!(?r, receiver_expr, "resolve_receiver_type result");
            r
        });

        let class_internal = match resolved.as_deref() {
            Some(t) => t,
            None => {
                tracing::debug!(
                    receiver_expr,
                    "resolve_receiver_type returned None, returning empty"
                );
                return vec![];
            }
        };

        tracing::debug!(class_internal, "looking up class in index");
        let class_meta = match index.get_class(class_internal) {
            Some(m) => m,
            None => {
                tracing::debug!(class_internal, "class NOT FOUND in index");
                return vec![];
            }
        };
        tracing::debug!(
            class_internal,
            methods = class_meta.methods.len(),
            "class found"
        );

        // Check if it's a similar access (allow private/protected access)
        let is_same_class = ctx.enclosing_internal_name.as_deref() == Some(class_internal);

        let filter = if is_same_class {
            AccessFilter::same_class()
        } else {
            AccessFilter::member_completion()
        };

        let prefix_lower = member_prefix.to_lowercase();
        let mut results = Vec::new();

        for method in &class_meta.methods {
            if method.name.as_ref() == "<init>" || method.name.as_ref() == "<clinit>" {
                continue;
            }
            if !filter.is_method_accessible(method.access_flags, method.is_synthetic) {
                continue;
            }
            if !prefix_lower.is_empty() && !method.name.to_lowercase().contains(&prefix_lower) {
                continue;
            }
            let is_static = method.access_flags & ACC_STATIC != 0;
            let kind = if is_static {
                CandidateKind::StaticMethod {
                    descriptor: Arc::clone(&method.descriptor),
                    defining_class: Arc::from(class_internal),
                }
            } else {
                CandidateKind::Method {
                    descriptor: Arc::clone(&method.descriptor),
                    defining_class: Arc::from(class_internal),
                }
            };
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&method.name),
                    if ctx.has_paren_after_cursor() {
                        method.name.to_string()
                    } else {
                        format!("{}(", method.name)
                    },
                    kind,
                    self.name(),
                )
                .with_detail(scorer::method_detail(class_internal, method)),
            );
        }

        for field in &class_meta.fields {
            if !filter.is_field_accessible(field.access_flags, field.is_synthetic) {
                continue;
            }
            if !prefix_lower.is_empty() && !field.name.to_lowercase().contains(&prefix_lower) {
                continue;
            }
            let is_static = field.access_flags & ACC_STATIC != 0;
            let kind = if is_static {
                CandidateKind::StaticField {
                    descriptor: Arc::clone(&field.descriptor),
                    defining_class: Arc::from(class_internal),
                }
            } else {
                CandidateKind::Field {
                    descriptor: Arc::clone(&field.descriptor),
                    defining_class: Arc::from(class_internal),
                }
            };
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&field.name),
                    field.name.to_string(),
                    kind,
                    self.name(),
                )
                .with_detail(scorer::field_detail(class_internal, field)),
            );
        }

        results
    }
}

impl MemberProvider {
    fn provide_from_source_members(
        &self,
        ctx: &CompletionContext,
        member_prefix: &str,
    ) -> Vec<CompletionCandidate> {
        let enclosing = ctx.enclosing_internal_name.as_deref().unwrap_or("");

        let scored =
            fuzzy::fuzzy_filter_sort(member_prefix, ctx.current_class_members.values(), |m| {
                m.name.as_ref()
            });

        scored
            .into_iter()
            .map(|(m, score)| {
                let kind = match (m.is_method, m.is_static) {
                    (true, true) => CandidateKind::StaticMethod {
                        descriptor: Arc::clone(&m.descriptor),
                        defining_class: Arc::from(enclosing),
                    },
                    (true, false) => CandidateKind::Method {
                        descriptor: Arc::clone(&m.descriptor),
                        defining_class: Arc::from(enclosing),
                    },
                    (false, true) => CandidateKind::StaticField {
                        descriptor: Arc::clone(&m.descriptor),
                        defining_class: Arc::from(enclosing),
                    },
                    (false, false) => CandidateKind::Field {
                        descriptor: Arc::clone(&m.descriptor),
                        defining_class: Arc::from(enclosing),
                    },
                };
                let insert_text = if m.is_method {
                    if ctx.has_paren_after_cursor() {
                        m.name.to_string()
                    } else {
                        format!("{}(", m.name)
                    }
                } else {
                    m.name.to_string()
                };
                let detail = format!(
                    "{} {} {}",
                    if m.is_private { "private" } else { "public" },
                    if m.is_static { "static" } else { "" },
                    m.name
                );
                CompletionCandidate::new(Arc::clone(&m.name), insert_text, kind, self.name())
                    .with_detail(detail)
                    .with_score(70.0 + score as f32 * 0.1)
            })
            .collect()
    }
}

/// Parses the receiver expression into an inner class name
fn resolve_receiver_type(
    expr: &str,
    ctx: &CompletionContext,
    index: &mut GlobalIndex,
) -> Option<Arc<str>> {
    tracing::debug!(
        expr,
        locals_count = ctx.local_variables.len(),
        "resolve_receiver_type"
    );

    if expr == "this" {
        let r = ctx.enclosing_internal_name.clone();
        tracing::debug!(?r, "this -> enclosing");
        return r;
    }

    // new Foo() / new Foo(args) -> return Foo's internal name
    if let Some(class_name) = extract_constructor_class(expr) {
        return resolve_simple_name_to_internal(class_name, ctx, index);
    }

    // local variable
    if let Some(lv) = ctx
        .local_variables
        .iter()
        .find(|lv| lv.name.as_ref() == expr)
    {
        tracing::debug!(
            expr,
            type_internal = lv.type_internal.as_ref(),
            "found in locals"
        );
        let ty = lv.type_internal.as_ref();

        if ty.contains('/') {
            tracing::debug!(ty, "type contains '/', returning directly");
            return Some(Arc::clone(&lv.type_internal));
        }

        let result = resolve_simple_name_to_internal(ty, ctx, index);
        tracing::debug!(?result, ty, "resolve_simple_name_to_internal result");
        return result;
    }

    tracing::debug!(expr, "local var not found");
    None
}

/// 从 "new Foo()" / "new Foo(a, b)" 中提取 "Foo"
fn extract_constructor_class(expr: &str) -> Option<&str> {
    let rest = expr.trim().strip_prefix("new ")?;
    // 取到 '(' 之前的部分就是类名（可能含泛型，如 "ArrayList<String>"）
    let class_part = rest.split('(').next()?.trim();
    // 去掉泛型参数：ArrayList<String> → ArrayList
    let simple = class_part.split('<').next()?.trim();
    if simple.is_empty() {
        None
    } else {
        Some(simple)
    }
}

/// Resolves simple class names to internal names
/// Search order: imports → same package → global
fn resolve_simple_name_to_internal(
    simple: &str,
    ctx: &CompletionContext,
    index: &mut GlobalIndex,
) -> Option<Arc<str>> {
    tracing::debug!(simple, "resolve_simple_name_to_internal called");

    let imported = index.resolve_imports(&ctx.existing_imports);
    // if let Some(m) = imported.iter().find(|m| m.name.as_ref() == simple) {
    //     // 防御：internal_name 必须包含 '/' 或者等于简单名（默认包）
    //     // 不应该是基本类型
    //     // 但这只是治标，要治本还需要找到为什么索引里有 internal_name = "long" 的类。
    //     if is_valid_class_internal(m.internal_name.as_ref()) {
    //         return Some(Arc::clone(&m.internal_name));
    //     }
    // }
    tracing::debug!(
        simple,
        imported = ?imported.iter().map(|m| format!("name={} internal={}", m.name, m.internal_name)).collect::<Vec<_>>(),
        "resolve_simple_name: imports"
    );

    if let Some(m) = imported.iter().find(|m| m.name.as_ref() == simple) {
        let exists = index.get_class(m.internal_name.as_ref()).is_some();
        tracing::debug!(
            internal = m.internal_name.as_ref(),
            exists,
            "found in imports"
        );
        return Some(Arc::clone(&m.internal_name));
    }

    if let Some(pkg) = ctx.enclosing_package.as_deref() {
        let classes = index.classes_in_package(pkg);
        tracing::debug!(pkg, count = classes.len(), "same package classes");
        if let Some(m) = classes.iter().find(|m| m.name.as_ref() == simple) {
            return Some(Arc::clone(&m.internal_name));
        }
    }

    let candidates = index.get_classes_by_simple_name(simple);
    tracing::debug!(simple, count = candidates.len(), internals = ?candidates.iter().map(|c| c.internal_name.as_ref()).collect::<Vec<_>>(), "global lookup");

    if !candidates.is_empty() {
        return Some(Arc::clone(&candidates[0].internal_name));
    }
    None
}

#[cfg(test)]
mod tests {
    use rust_asm::constants::{ACC_PRIVATE, ACC_PUBLIC, ACC_STATIC};

    use crate::index::{ClassMetadata, ClassOrigin, FieldSummary, GlobalIndex, MethodSummary};
    use crate::language::Language;
    use crate::{
        completion::{
            candidate::CandidateKind,
            context::{CompletionContext, CursorLocation, LocalVar},
            providers::{CompletionProvider, member::MemberProvider},
            type_resolver::parse_return_type_from_descriptor,
        },
        language::JavaLanguage,
    };
    use std::sync::Arc;

    fn at(src: &str, line: u32, col: u32) -> CompletionContext {
        JavaLanguage
            .parse_completion_context(src, line, col, None)
            .unwrap()
    }

    fn make_method(
        name: &str,
        descriptor: &str,
        access_flags: u16,
        is_synthetic: bool,
    ) -> MethodSummary {
        MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor),
            access_flags,
            is_synthetic,
            generic_signature: None,
            return_type: parse_return_type_from_descriptor(descriptor),
        }
    }

    fn make_field(
        name: &str,
        descriptor: &str,
        access_flags: u16,
        is_synthetic: bool,
    ) -> FieldSummary {
        FieldSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor),
            access_flags,
            is_synthetic,
        }
    }

    fn make_index(methods: Vec<MethodSummary>, fields: Vec<FieldSummary>) -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("com/example")),
            name: Arc::from("Foo"),
            internal_name: Arc::from("com/example/Foo"),
            super_name: None,
            interfaces: vec![],
            methods,
            fields,
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        idx
    }

    /// receiver_type is known directly
    fn ctx_with_type(receiver_internal: &str, prefix: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: Some(Arc::from(receiver_internal)),
                member_prefix: prefix.to_string(),
                receiver_expr: "someObj".to_string(),
            },
            prefix,
            vec![],
            None,   // enclosing_class
            None,   // enclosing_internal_name
            None,   // enclosing_package
            vec![], // existing_imports
        )
    }

    /// receiver_type is unknown, deduced from locals
    fn ctx_with_local(var_name: &str, var_type: &str, prefix: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: prefix.to_string(),
                receiver_expr: var_name.to_string(),
            },
            prefix,
            vec![LocalVar {
                name: Arc::from(var_name),
                type_internal: Arc::from(var_type),
                init_expr: None,
            }],
            None,
            None,
            None,
            vec![],
        )
    }

    /// Same type of access (receiver_expr = "this")
    fn ctx_this(
        enclosing_simple: &str,
        enclosing_internal: &str,
        enclosing_pkg: &str,
        prefix: &str,
    ) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: prefix.to_string(),
                receiver_expr: "this".to_string(),
            },
            prefix,
            vec![],
            Some(Arc::from(enclosing_simple)),
            Some(Arc::from(enclosing_internal)),
            Some(Arc::from(enclosing_pkg)),
            vec![],
        )
    }

    /// The local var type is a simple name and needs to be resolved via import.
    fn ctx_with_local_and_import(
        var_name: &str,
        var_type_simple: &str,
        prefix: &str,
        imports: Vec<String>,
        enclosing_pkg: &str,
    ) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: prefix.to_string(),
                receiver_expr: var_name.to_string(),
            },
            prefix,
            vec![LocalVar {
                name: Arc::from(var_name),
                type_internal: Arc::from(var_type_simple), // simple name
                init_expr: None,
            }],
            None,
            None,
            Some(Arc::from(enclosing_pkg)),
            imports,
        )
    }

    #[test]
    fn test_instance_method_found() {
        let mut idx = make_index(
            vec![make_method(
                "getValue",
                "()Ljava/lang/String;",
                ACC_PUBLIC,
                false,
            )],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "get");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "getValue"),
            "results: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_empty_prefix_returns_all_public() {
        let mut idx = make_index(
            vec![
                make_method("getName", "()Ljava/lang/String;", ACC_PUBLIC, false),
                make_method("setName", "(Ljava/lang/String;)V", ACC_PUBLIC, false),
                make_method("secret", "()V", ACC_PRIVATE, false),
            ],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert_eq!(results.len(), 2, "only public methods, secret hidden");
    }

    #[test]
    fn test_prefix_filter_case_insensitive() {
        let mut idx = make_index(
            vec![
                make_method("getName", "()Ljava/lang/String;", ACC_PUBLIC, false),
                make_method("setName", "(Ljava/lang/String;)V", ACC_PUBLIC, false),
            ],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "GET");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "getName"));
        assert!(results.iter().all(|c| c.label.as_ref() != "setName"));
    }

    #[test]
    fn test_private_hidden_from_outside() {
        let mut idx = make_index(
            vec![make_method("secret", "()V", ACC_PRIVATE, false)],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.is_empty());
    }

    #[test]
    fn test_synthetic_attr_hidden() {
        let mut idx = make_index(
            vec![make_method("access$000", "()V", ACC_PUBLIC, true)],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.is_empty());
    }

    #[test]
    fn test_acc_synthetic_flag_only_visible() {
        // ACC_SYNTHETIC flag but no Synthetic attribute -> Kotlin normal method, should show
        let mut idx = make_index(
            vec![make_method("main", "([Ljava/lang/String;)V", 0x1009, false)],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "main"));
    }

    #[test]
    fn test_same_class_private_visible_via_this() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy/a")),
            name: Arc::from("Main"),
            internal_name: Arc::from("org/cubewhy/a/Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![
                make_method("pri", "()V", ACC_PRIVATE | ACC_STATIC, false),
                make_method("func", "()V", ACC_PUBLIC, false),
            ],
            fields: vec![make_field("secret", "I", ACC_PRIVATE, false)],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let ctx = ctx_this("Main", "org/cubewhy/a/Main", "org/cubewhy/a", "pr");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "pri"),
            "private method visible via this from same class"
        );
    }

    #[test]
    fn test_same_class_private_field_visible() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy/a")),
            name: Arc::from("Main"),
            internal_name: Arc::from("org/cubewhy/a/Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![make_field("count", "I", ACC_PRIVATE, false)],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let ctx = ctx_this("Main", "org/cubewhy/a/Main", "org/cubewhy/a", "co");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "count"));
    }

    #[test]
    fn test_static_method_kind() {
        let mut idx = make_index(
            vec![make_method(
                "of",
                "()Lcom/example/Foo;",
                ACC_PUBLIC | ACC_STATIC,
                false,
            )],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(matches!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "of")
                .unwrap()
                .kind,
            CandidateKind::StaticMethod { .. }
        ));
    }

    #[test]
    fn test_instance_method_kind() {
        let mut idx = make_index(vec![make_method("run", "()V", ACC_PUBLIC, false)], vec![]);
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(matches!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "run")
                .unwrap()
                .kind,
            CandidateKind::Method { .. }
        ));
    }

    #[test]
    fn test_init_filtered() {
        let mut idx = make_index(
            vec![
                make_method("<init>", "()V", ACC_PUBLIC, false),
                make_method("<clinit>", "()V", ACC_PUBLIC, false),
                make_method("run", "()V", ACC_PUBLIC, false),
            ],
            vec![],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().all(|c| c.label.as_ref() != "<init>"));
        assert!(results.iter().all(|c| c.label.as_ref() != "<clinit>"));
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn test_field_found() {
        let mut idx = make_index(vec![], vec![make_field("value", "I", ACC_PUBLIC, false)]);
        let ctx = ctx_with_type("com/example/Foo", "val");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "value"));
        assert!(matches!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "value")
                .unwrap()
                .kind,
            CandidateKind::Field { .. }
        ));
    }

    #[test]
    fn test_static_field_kind() {
        let mut idx = make_index(
            vec![],
            vec![make_field("CONST", "I", ACC_PUBLIC | ACC_STATIC, false)],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(matches!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "CONST")
                .unwrap()
                .kind,
            CandidateKind::StaticField { .. }
        ));
    }

    #[test]
    fn test_synthetic_field_hidden() {
        let mut idx = make_index(
            vec![],
            vec![make_field("this$0", "Lcom/example/Outer;", 0x1010, true)],
        );
        let ctx = ctx_with_type("com/example/Foo", "");
        assert!(MemberProvider.provide(&ctx, &mut idx).is_empty());
    }

    #[test]
    fn test_receiver_inferred_from_locals_internal_name() {
        // The type_internal property of a local var is already the internal name.
        let mut idx = make_index(
            vec![make_method("process", "()V", ACC_PUBLIC, false)],
            vec![],
        );
        let ctx = ctx_with_local("foo", "com/example/Foo", "proc");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "process"),
            "infer type from local var (internal name): {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_receiver_inferred_from_locals_via_import() {
        // The local var type is the simple name "Foo", which needs to be resolved via import.
        let mut idx = make_index(vec![make_method("run", "()V", ACC_PUBLIC, false)], vec![]);
        let ctx = ctx_with_local_and_import(
            "myFoo",
            "Foo", // simple name
            "ru",
            vec!["com.example.Foo".to_string()],
            "org/cubewhy",
        );
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "run"),
            "should resolve simple name via import: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_receiver_inferred_from_locals_via_same_package() {
        // The local var type is a simple name, resolved through the same package.
        let mut idx = make_index(vec![make_method("go", "()V", ACC_PUBLIC, false)], vec![]);
        let ctx = ctx_with_local_and_import(
            "f",
            "Foo",
            "",
            vec![],        // no imports
            "com/example", // same package
        );
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "go"),
            "should resolve simple name via same package: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_receiver_unknown_returns_empty() {
        let mut idx = make_index(vec![make_method("run", "()V", ACC_PUBLIC, false)], vec![]);
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "unknown".to_string(),
            },
            "",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        assert!(MemberProvider.provide(&ctx, &mut idx).is_empty());
    }

    #[test]
    fn test_this_receiver_resolves_to_enclosing() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("com/example")),
            name: Arc::from("Foo"),
            internal_name: Arc::from("com/example/Foo"),
            super_name: None,
            interfaces: vec![],
            methods: vec![make_method("run", "()V", ACC_PUBLIC, false)],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let ctx = ctx_this("Foo", "com/example/Foo", "com/example", "ru");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "run"),
            "this.| should resolve to enclosing class"
        );
    }

    #[test]
    fn test_method_insert_ends_with_paren() {
        let mut idx = make_index(vec![make_method("run", "()V", ACC_PUBLIC, false)], vec![]);
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "run")
                .unwrap()
                .insert_text
                .ends_with('(')
        );
    }

    #[test]
    fn test_field_insert_no_paren() {
        let mut idx = make_index(vec![], vec![make_field("count", "I", ACC_PUBLIC, false)]);
        let ctx = ctx_with_type("com/example/Foo", "");
        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            !results
                .iter()
                .find(|c| c.label.as_ref() == "count")
                .unwrap()
                .insert_text
                .contains('(')
        );
    }

    #[test]
    fn test_this_dot_uses_source_members_including_private() {
        // When using `this`, source code members should be used, including private methods and methods not compiled into the index.
        use crate::completion::context::CurrentClassMember;

        let mut idx = GlobalIndex::new(); // The current class is not in the index

        let members = vec![
            CurrentClassMember {
                name: Arc::from("priFunc"),
                is_method: true,
                is_static: false,
                is_private: true,
                descriptor: Arc::from("()V"),
            },
            CurrentClassMember {
                name: Arc::from("fun"),
                is_method: true,
                is_static: false,
                is_private: false,
                descriptor: Arc::from("()V"),
            },
            CurrentClassMember {
                name: Arc::from("func"),
                is_method: true,
                is_static: false,
                is_private: false,
                descriptor: Arc::from("()V"),
            },
        ];

        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(), // this.| : empty prefix
                receiver_expr: "this".to_string(),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members);

        let results = MemberProvider.provide(&ctx, &mut idx);

        assert!(
            results.iter().any(|c| c.label.as_ref() == "priFunc"),
            "priFunc (private) should be visible via this.: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
        assert!(results.iter().any(|c| c.label.as_ref() == "fun"));
        assert!(results.iter().any(|c| c.label.as_ref() == "func"));
    }

    #[test]
    fn test_this_dot_prefix_filter() {
        use crate::completion::context::CurrentClassMember;

        let mut idx = GlobalIndex::new();

        let members = vec![
            CurrentClassMember {
                name: Arc::from("priFunc"),
                is_method: true,
                is_static: false,
                is_private: true,
                descriptor: Arc::from("()V"),
            },
            CurrentClassMember {
                name: Arc::from("fun"),
                is_method: true,
                is_static: false,
                is_private: false,
                descriptor: Arc::from("()V"),
            },
            CurrentClassMember {
                name: Arc::from("other"),
                is_method: true,
                is_static: false,
                is_private: false,
                descriptor: Arc::from("()V"),
            },
        ];

        // Use "xyz" — a character sequence that does not exist in any of the candidates.
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "xyz".to_string(),
                receiver_expr: "this".to_string(),
            },
            "xyz",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members);

        let results = MemberProvider.provide(&ctx, &mut idx);

        // The subsequence "xyz" does not exist in any candidate list, so it is filtered out.
        assert!(
            results.is_empty(),
            "pattern 'xyz' should match nothing: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_this_dot_fuzzy_pf_matches_pri_func() {
        use crate::completion::context::CurrentClassMember;

        let mut idx = GlobalIndex::new();

        let members = vec![
            CurrentClassMember {
                name: Arc::from("priFunc"),
                is_method: true,
                is_static: false,
                is_private: true,
                descriptor: Arc::from("()V"),
            },
            CurrentClassMember {
                name: Arc::from("fun"),
                is_method: true,
                is_static: false,
                is_private: false,
                descriptor: Arc::from("()V"),
            },
        ];

        // "pf" 是 "priFunc" 的子序列（p...f），不是 "fun" 的子序列
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "pf".to_string(),
                receiver_expr: "this".to_string(),
            },
            "pf",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members);

        let results = MemberProvider.provide(&ctx, &mut idx);

        assert!(
            results.iter().any(|c| c.label.as_ref() == "priFunc"),
            "'pf' should fuzzy-match 'priFunc'"
        );
        assert!(
            results.iter().all(|c| c.label.as_ref() != "fun"),
            "'pf' should not match 'fun'"
        );
    }

    #[test]
    fn test_static_member_prefix_starts_with() {
        // StaticMemberProvider should use starts_with instead of contains
        // "ma" should match "main" but not "putIfAbsent" (contains will mistakenly match words containing "ma")
        use crate::completion::providers::static_member::StaticMemberProvider;
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy/a")),
            name: Arc::from("Main"),
            internal_name: Arc::from("org/cubewhy/a/Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![
                MethodSummary {
                    name: Arc::from("main"),
                    descriptor: Arc::from("()V"),
                    access_flags: ACC_PUBLIC | ACC_STATIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: None,
                },
                MethodSummary {
                    name: Arc::from("notStartsWithma"), // Contains "ma" but does not start_with
                    descriptor: Arc::from("()V"),
                    access_flags: ACC_PUBLIC | ACC_STATIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: None,
                },
            ],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let ctx = CompletionContext::new(
            CursorLocation::StaticAccess {
                class_internal_name: Arc::from("org/cubewhy/a/Main"),
                member_prefix: "ma".to_string(),
            },
            "ma",
            vec![],
            None,
            None,
            None,
            vec![],
        );

        let results = StaticMemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "main"));
        assert!(
            results
                .iter()
                .all(|c| c.label.as_ref() != "notStartsWithma"),
            "starts_with should not match notStartsWithma for prefix 'ma'"
        );
    }

    #[test]
    fn test_member_completion_via_wildcard_import() {
        // cl: RandomClass (simple name), resolved via import org.cubewhy.*
        use crate::index::codebase::index_source_text;

        let mut idx = GlobalIndex::new();
        idx.add_classes(index_source_text(
            "file:///RandomClass.java",
            r#"
package org.cubewhy;
public class RandomClass {
    public void f() {}
    public String getName() { return ""; }
}
"#,
            "java",
        ));

        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("cl"),
                // extract_locals will store simple names
                type_internal: Arc::from("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            // wildcard import
            vec!["org.cubewhy.*".to_string()],
        );

        let results = MemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "f"),
            "f() should be found via wildcard import type resolution: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
        assert!(results.iter().any(|c| c.label.as_ref() == "getName"));
    }

    #[test]
    fn test_extract_locals_type_at_cursor_boundary() {
        // When the declaration of cl ends near the cursor, it should not be filtered out.
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aVar = "test";
                String str = "a";
                RandomClass cl = new RandomClass();
                cl.f()
            }
        }
    "#};
        let line = 5u32;
        let raw = src.lines().nth(5).unwrap();
        // The cursor is after the dot "cl."
        let col = raw.find("cl.").unwrap() as u32 + 3;
        let ctx = at(src, line, col);

        assert!(
            ctx.local_variables.iter().any(|v| v.name.as_ref() == "cl"),
            "cl should be in locals: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| format!("{}:{}", v.name, v.type_internal))
                .collect::<Vec<_>>()
        );
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "aVar"),
            "aVar should be in locals"
        );
        assert!(
            ctx.local_variables.iter().any(|v| v.name.as_ref() == "str"),
            "str should be in locals"
        );

        // The type of cl should be the simple name "RandomClass"
        let cl = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "cl")
            .unwrap();
        assert_eq!(cl.type_internal.as_ref(), "RandomClass");
    }
}
