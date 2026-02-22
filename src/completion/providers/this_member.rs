use super::CompletionProvider;
use crate::completion::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
    fuzzy,
};
use crate::index::GlobalIndex;
use std::sync::Arc;

pub struct ThisMemberProvider;

impl CompletionProvider for ThisMemberProvider {
    fn name(&self) -> &'static str {
        "this_member"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        _index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::Expression { prefix } => prefix.as_str(),
            CursorLocation::MethodArgument { prefix } => prefix.as_str(),
            _ => return vec![],
        };

        if ctx.current_class_members.is_empty() {
            return vec![];
        }

        let in_static = ctx.is_in_static_context();
        let enclosing = ctx.enclosing_internal_name.as_deref().unwrap_or("");

        let scored = fuzzy::fuzzy_filter_sort(
            prefix,
            ctx.current_class_members
                .values()
                .filter(|m| !in_static || m.is_static),
            |m| m.name.as_ref(),
        );

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
                    .with_score(60.0 + score as f32 * 0.1)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CurrentClassMember, CursorLocation};
    use crate::index::GlobalIndex;
    use std::sync::Arc;

    fn make_member(
        name: &str,
        is_method: bool,
        is_static: bool,
        is_private: bool,
    ) -> CurrentClassMember {
        CurrentClassMember {
            name: Arc::from(name),
            is_method,
            is_static,
            is_private,
            descriptor: Arc::from(if is_method { "()V" } else { "I" }),
        }
    }

    fn ctx_with_members(prefix: &str, members: Vec<CurrentClassMember>) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Expression {
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members)
    }

    fn ctx_with_members_static(
        prefix: &str,
        members: Vec<CurrentClassMember>,
        enclosing: CurrentClassMember,
    ) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Expression {
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members)
        .with_enclosing_member(Some(enclosing))
    }

    #[test]
    fn test_prefix_match() {
        let members = vec![
            make_member("func", true, false, false),
            make_member("fun", true, false, false),
            make_member("pri", true, true, true),
            make_member("other", true, false, false),
        ];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("fu", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "func"));
        assert!(results.iter().any(|c| c.label.as_ref() == "fun"));
        assert!(results.iter().all(|c| c.label.as_ref() != "other"));
        assert!(results.iter().all(|c| c.label.as_ref() != "pri"));
    }

    #[test]
    fn test_private_method_visible() {
        // Private methods of the same type should be visible
        let members = vec![make_member("pri", true, true, true)];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("pr", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "pri"),
            "private method should be visible: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_private_static_method_visible() {
        let members = vec![make_member("pri", true, true, true)];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("pr", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "pri"));
        assert!(matches!(
            results
                .iter()
                .find(|c| c.label.as_ref() == "pri")
                .unwrap()
                .kind,
            CandidateKind::StaticMethod { .. }
        ));
    }

    #[test]
    fn test_field_no_paren() {
        let members = vec![make_member("count", false, false, true)];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("co", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        let c = results
            .iter()
            .find(|c| c.label.as_ref() == "count")
            .unwrap();
        assert!(!c.insert_text.contains('('));
        assert!(matches!(c.kind, CandidateKind::Field { .. }));
    }

    #[test]
    fn test_empty_prefix_returns_all() {
        let members = vec![make_member("func", true, false, false)];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].label.as_ref(), "func");
    }

    #[test]
    fn test_no_members_returns_nothing() {
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("fu", vec![]);
        assert!(ThisMemberProvider.provide(&ctx, &mut idx).is_empty());
    }

    #[test]
    fn test_method_before_cursor_accessible() {
        // Verification method definition can be completed even after the cursor (full-text scan)
        // Here, it is injected directly through current_class_members to simulate full-text parsing
        let members = vec![
            make_member("afterMethod", true, false, false), // Defined after the cursor
            make_member("beforeMethod", true, false, false), // Defined before the cursor
        ];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("a", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "afterMethod"),
            "method defined after cursor should still be completable"
        );
    }

    #[test]
    fn test_kind_static_field() {
        let members = vec![make_member("CONST", false, true, false)];
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members("CO", members);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
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
    fn test_static_method_no_this() {
        // In a static method, ThisMemberProvider should not return any result.
        let members = vec![
            make_member("helper", true, false, false),
            make_member("CONST", false, true, false),
        ];
        let enclosing = CurrentClassMember {
            name: Arc::from("staticEntry"),
            is_method: true,
            is_static: true, // static method
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members_static("he", members, enclosing);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.is_empty(),
            "ThisMemberProvider should return nothing inside a static method, got: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_instance_method_has_this() {
        // In the instance method, ThisMemberProvider should return normally.
        let members = vec![make_member("helper", true, false, false)];
        let enclosing = CurrentClassMember {
            name: Arc::from("instanceEntry"),
            is_method: true,
            is_static: false, // instance method
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members_static("he", members, enclosing);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "helper"),
            "ThisMemberProvider should work inside an instance method"
        );
    }

    #[test]
    fn test_this_dot_in_static_method() {
        // Even if `this.xxx` is explicitly written, it should not be completed in the static method.
        let members = vec![make_member("field", false, false, false)];
        let enclosing = CurrentClassMember {
            name: Arc::from("staticFn"),
            is_method: true,
            is_static: true,
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let mut idx = GlobalIndex::new();
        let mut ctx = ctx_with_members_static("", members, enclosing);
        ctx.location = CursorLocation::MemberAccess {
            receiver_type: None,
            member_prefix: "fi".to_string(),
            receiver_expr: "this".to_string(),
        };
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.is_empty(),
            "this.xxx should not complete inside a static method"
        );
    }

    #[test]
    fn test_static_context_only_shows_static_members() {
        // Static methods should only display static members
        let members = vec![
            make_member("pri", true, true, false),     // static method
            make_member("fun", true, false, false),    // instance method
            make_member("CONST", false, true, false),  // static field
            make_member("count", false, false, false), // instance field
        ];
        let mut idx = GlobalIndex::new();

        // Construct a static context
        let enclosing_method = CurrentClassMember {
            name: Arc::from("main"),
            is_method: true,
            is_static: true,
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let ctx = CompletionContext::new(
            CursorLocation::Expression {
                prefix: "".to_string(),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members)
        .with_enclosing_member(Some(enclosing_method));

        let results = ThisMemberProvider.provide(&ctx, &mut idx);

        assert!(
            results.iter().any(|c| c.label.as_ref() == "pri"),
            "static method should be visible in static context"
        );
        assert!(
            results.iter().any(|c| c.label.as_ref() == "CONST"),
            "static field should be visible in static context"
        );
        assert!(
            results.iter().all(|c| c.label.as_ref() != "fun"),
            "instance method should NOT be visible in static context"
        );
        assert!(
            results.iter().all(|c| c.label.as_ref() != "count"),
            "instance field should NOT be visible in static context"
        );
    }

    #[test]
    fn test_static_context_prefix_filter() {
        // Static methods use the "pr" prefix; you should be able to find pri
        let members = vec![
            make_member("pri", true, true, false),
            make_member("fun", true, false, false),
        ];
        let mut idx = GlobalIndex::new();

        let enclosing_method = CurrentClassMember {
            name: Arc::from("main"),
            is_method: true,
            is_static: true,
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let ctx = CompletionContext::new(
            CursorLocation::Expression {
                prefix: "pr".to_string(),
            },
            "pr",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members)
        .with_enclosing_member(Some(enclosing_method));

        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "pri"),
            "should find static method 'pri' with prefix 'pr' in static context: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_method_arg_empty_prefix_returns_locals_and_members() {
        // println(|) with an empty prefix should return local variables and class members
        let members = vec![make_member("pri", true, true, false)];
        let mut idx = GlobalIndex::new();

        let enclosing_method = CurrentClassMember {
            name: Arc::from("main"),
            is_method: true,
            is_static: true,
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let ctx = CompletionContext::new(
            CursorLocation::MethodArgument {
                prefix: "".to_string(),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec![],
        )
        .with_class_members(members)
        .with_enclosing_member(Some(enclosing_method));

        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "pri"),
            "empty prefix in method arg should return static members in static context: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_static_method_no_instance_members() {
        let members = vec![
            make_member("helper", true, false, false), // instance
            make_member("CONST", false, true, false),  // static
        ];
        let enclosing = CurrentClassMember {
            name: Arc::from("staticEntry"),
            is_method: true,
            is_static: true,
            is_private: false,
            descriptor: Arc::from("()V"),
        };
        let mut idx = GlobalIndex::new();
        let ctx = ctx_with_members_static("he", members, enclosing);
        let results = ThisMemberProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().all(|c| c.label.as_ref() != "helper"),
            "instance method should not appear in static context: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
        // CONST is static, so "he" cannot match "CONST", and it's normal for the result to be empty.
    }
}
