use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::{completion::fuzzy, index::GlobalIndex};
use std::sync::Arc;

pub struct LocalVarProvider;

impl CompletionProvider for LocalVarProvider {
    fn name(&self) -> &'static str {
        "local_var"
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

        let scored =
            fuzzy::fuzzy_filter_sort(prefix, ctx.local_variables.iter(), |lv| lv.name.as_ref());

        scored
            .into_iter()
            .map(|(lv, score)| {
                let type_simple = lv
                    .type_internal
                    .rsplit('/')
                    .next()
                    .unwrap_or(&lv.type_internal);
                CompletionCandidate::new(
                    Arc::clone(&lv.name),
                    lv.name.to_string(),
                    CandidateKind::LocalVariable {
                        type_descriptor: Arc::clone(&lv.type_internal),
                    },
                    self.name(),
                )
                .with_detail(format!("{} : {}", lv.name, type_simple))
                .with_score(50.0 + score as f32 * 0.1)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation, LocalVar};
    use crate::index::GlobalIndex;
    use std::sync::Arc;

    fn make_ctx(prefix: &str, vars: Vec<(&str, &str)>) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Expression {
                prefix: prefix.to_string(),
            },
            prefix,
            vars.into_iter()
                .map(|(name, ty)| LocalVar {
                    name: Arc::from(name),
                    type_internal: Arc::from(ty),
                    init_expr: None,
                })
                .collect(),
            None,
            None,
            None,
            vec![],
        )
    }

    #[test]
    fn test_prefix_match() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx(
            "str",
            vec![("str", "java/lang/String"), ("aVar", "java/lang/String")],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "str"));
        assert!(results.iter().all(|c| c.label.as_ref() != "aVar"));
    }

    #[test]
    fn test_partial_prefix_match() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx(
            "aV",
            vec![("aVar", "java/lang/String"), ("str", "java/lang/String")],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "aVar"));
        assert!(results.iter().all(|c| c.label.as_ref() != "str"));
    }

    #[test]
    fn test_empty_prefix_returns_all_locals() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx(
            "",
            vec![("aVar", "java/lang/String"), ("str", "java/lang/String")],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert_eq!(
            results.len(),
            2,
            "empty prefix should return all locals: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_case_insensitive() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx("AVAR", vec![("aVar", "java/lang/String")]);
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "aVar"));
    }

    #[test]
    fn test_method_argument_location() {
        let mut idx = GlobalIndex::new();
        let ctx = CompletionContext::new(
            CursorLocation::MethodArgument {
                prefix: "aV".to_string(),
            },
            "aV",
            vec![LocalVar {
                name: Arc::from("aVar"),
                type_internal: Arc::from("java/lang/String"),
                init_expr: None,
            }],
            None,
            None,
            None,
            vec![],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "aVar"),
            "should complete locals inside method arguments"
        );
    }

    #[test]
    fn test_fuzzy_match_var_finds_a_var() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx(
            "var",
            vec![("aVar", "java/lang/String"), ("str", "java/lang/String")],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "aVar"),
            "fuzzy: 'var' should match 'aVar': {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_no_match_returns_empty() {
        let mut idx = GlobalIndex::new();
        let ctx = make_ctx("xyz", vec![("aVar", "java/lang/String")]);
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(results.is_empty(), "no fuzzy match should return empty");
    }

    #[test]
    fn test_member_access_does_not_return_locals() {
        let mut idx = GlobalIndex::new();
        let ctx = CompletionContext::new(
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
            None,
            None,
            None,
            vec![],
        );
        let results = LocalVarProvider.provide(&ctx, &mut idx);
        assert!(
            results.is_empty(),
            "LocalVarProvider should not return locals for MemberAccess: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }
}
