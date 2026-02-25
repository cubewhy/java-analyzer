use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::{
    completion::{fuzzy, import_utils::is_import_needed},
    index::GlobalIndex,
};
use std::sync::Arc;

pub struct ExpressionProvider;

impl CompletionProvider for ExpressionProvider {
    fn name(&self) -> &'static str {
        "expression"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::Expression { prefix } => prefix.as_str(),
            CursorLocation::TypeAnnotation { prefix } => prefix.as_str(),
            CursorLocation::MethodArgument { prefix } => prefix.as_str(),
            _ => return vec![],
        };

        if prefix.contains('.') {
            return vec![];
        }

        let prefix_lower = prefix.to_lowercase();

        // Package name of the current file (used to determine if it is in the same package)
        let current_pkg = ctx.enclosing_package.as_deref();

        let mut results = Vec::new();

        // Classes that have already been imported in current context
        let imported = index.resolve_imports(&ctx.existing_imports);
        for meta in &imported {
            let score = if prefix.is_empty() {
                0
            } else {
                match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                    Some(s) => s,
                    None => continue,
                }
            };
            let fqn = fqn_of(meta);
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&meta.name),
                    meta.name.to_string(),
                    CandidateKind::ClassName,
                    self.name(),
                )
                .with_detail(fqn)
                .with_score(80.0 + score as f32 * 0.1),
            );
        }

        let imported_internals: std::collections::HashSet<Arc<str>> = imported
            .iter()
            .map(|m| Arc::clone(&m.internal_name))
            .collect();

        // Same package
        if let Some(pkg) = current_pkg {
            for meta in index.classes_in_package(pkg) {
                if imported_internals.contains(&meta.internal_name) {
                    continue;
                }
                let score = if prefix.is_empty() {
                    0
                } else {
                    match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                        Some(s) => s,
                        None => continue,
                    }
                };
                let fqn = fqn_of(meta);
                results.push(
                    CompletionCandidate::new(
                        Arc::clone(&meta.name),
                        meta.name.to_string(),
                        CandidateKind::ClassName,
                        self.name(),
                    )
                    .with_detail(fqn)
                    .with_score(90.0 + score as f32 * 0.1),
                );
            }
        }

        // Other classes (global, require auto-import)
        if !prefix.is_empty() {
            for meta in index.iter_all_classes() {
                if imported_internals.contains(&meta.internal_name) {
                    continue;
                }
                let score = match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                    Some(s) => s,
                    None => continue,
                };
                let fqn = fqn_of(meta);
                let base_score = if meta.package.as_deref() == Some("java/lang") {
                    70.0
                } else {
                    40.0
                };

                let candidate = CompletionCandidate::new(
                    Arc::clone(&meta.name),
                    meta.name.to_string(),
                    CandidateKind::ClassName,
                    self.name(),
                )
                .with_detail(fqn.clone())
                .with_score(base_score + score as f32 * 0.1);

                let needs_import = is_import_needed(
                    &fqn,
                    &ctx.existing_imports,
                    ctx.enclosing_package.as_deref(),
                );
                let candidate = if needs_import {
                    candidate.with_import(fqn)
                } else {
                    candidate
                };

                results.push(candidate);
            }
        }

        results
    }
}

fn fqn_of(meta: &crate::index::ClassMetadata) -> String {
    match &meta.package {
        Some(pkg) => format!("{}.{}", pkg.replace('/', "."), meta.name),
        None => meta.name.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use rust_asm::constants::ACC_PUBLIC;

    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::completion::providers::CompletionProvider;
    use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex};
    use std::sync::Arc;

    fn make_cls(pkg: &str, name: &str) -> ClassMetadata {
        ClassMetadata {
            package: Some(Arc::from(pkg)),
            name: Arc::from(name),
            internal_name: Arc::from(format!("{}/{}", pkg, name).as_str()),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }
    }

    fn make_index() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_cls("org/cubewhy", "Main"),
            make_cls("org/cubewhy", "Main2"),
            make_cls("java/util", "ArrayList"),
            make_cls("java/util", "HashMap"),
        ]);
        idx
    }

    fn ctx(
        prefix: &str,
        enclosing_class: &str,
        enclosing_pkg: &str,
        imports: Vec<String>,
    ) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Expression {
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            Some(Arc::from(enclosing_class)),
            None, // enclosing_internal_name
            Some(Arc::from(enclosing_pkg)),
            imports,
        )
    }

    #[test]
    fn test_same_name_different_package_not_filtered() {
        // There is a Main package in another package, which should not be filtered.
        let mut index = make_index();
        index.add_classes(vec![make_cls("com/other", "Main")]);
        let ctx = ctx("Main", "Main", "org/cubewhy", vec![]);
        let results = ExpressionProvider.provide(&ctx, &mut index);
        // com/other/Main should appear (with auto-import)
        assert!(
            results.iter().any(|c| {
                c.label.as_ref() == "Main" && c.required_import.as_deref() == Some("com.other.Main")
            }),
            "should suggest Main from other package with import"
        );
    }

    #[test]
    fn test_self_class_appears_in_same_package() {
        // The class itself should appear in the completion (this can be used for type annotations, static access, etc.)
        let mut index = make_index();
        let ctx = ctx("Main", "Main", "org/cubewhy", vec![]);
        let results = ExpressionProvider.provide(&ctx, &mut index);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "Main"),
            "enclosing class itself should appear as a completion candidate: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_same_name_different_package_both_appear() {
        let mut index = make_index();
        index.add_classes(vec![make_cls("com/other", "Main")]);
        let ctx = ctx("Main", "Main", "org/cubewhy", vec![]);
        let results = ExpressionProvider.provide(&ctx, &mut index);
        // Both Main (no import) and com/other/Main (requires import) in the same package should appear.
        assert!(
            results.iter().any(|c| c.label.as_ref() == "Main"),
            "should suggest Main from same package"
        );
        assert!(
            results.iter().any(|c| {
                c.label.as_ref() == "Main" && c.required_import.as_deref() == Some("com.other.Main")
            }),
            "should also suggest Main from other package with import"
        );
    }
}
