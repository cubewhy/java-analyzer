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
use crate::completion::import_utils::resolve_simple_to_internal;
use crate::completion::providers::expression::ExpressionProvider;
use crate::completion::providers::package::PackageProvider;
use crate::completion::providers::this_member::ThisMemberProvider;
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
            c.score = scorer.score(c);
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
                    ctx.enclosing_class.as_ref(),
                )
            } else {
                resolver.resolve_chain(&chain, &ctx.local_variables, ctx.enclosing_class.as_ref())
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
    }
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
fn parse_chain_from_expr(expr: &str) -> Vec<super::type_resolver::ChainSegment> {
    use super::type_resolver::ChainSegment;

    let mut segments = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    let mut in_method = false;
    let mut comma_count = 0i32;
    let mut has_any_arg = false;

    for ch in expr.chars() {
        match ch {
            '(' => {
                depth += 1;
                if depth == 1 {
                    in_method = true;
                    comma_count = 0;
                    has_any_arg = false;
                }
            }
            ')' => {
                depth -= 1;
                if depth == 0 && in_method {
                    // Number of parameters: If there is content within the parentheses, increment comma_count by 1; otherwise, increment comma_count by 0.
                    let arg_count = if has_any_arg { comma_count + 1 } else { 0 };
                    segments.push(ChainSegment::method(current.trim(), arg_count));
                    current = String::new();
                    in_method = false;
                }
            }
            ',' if depth == 1 => {
                // Only count the commas in the outermost parentheses
                comma_count += 1;
                has_any_arg = true;
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
                } else if depth == 1 && !c.is_whitespace() {
                    // Non-whitespace characters within parentheses -> Parameters are present
                    has_any_arg = true;
                }
            }
        }
    }

    // The last remaining fragment is member_prefix, discard it.
    let _ = current;

    segments
}

#[cfg(test)]
mod tests {
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
}
