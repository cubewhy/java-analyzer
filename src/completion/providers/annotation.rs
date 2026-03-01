use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
    fuzzy,
    import_utils::is_import_needed,
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
use std::sync::Arc;

/// Built-in annotations that are always available without import
const BUILTIN_ANNOTATIONS: &[(&str, &str)] = &[
    ("Override", "java/lang/Override"),
    ("Deprecated", "java/lang/Deprecated"),
    ("SuppressWarnings", "java/lang/SuppressWarnings"),
    ("FunctionalInterface", "java/lang/FunctionalInterface"),
    ("SafeVarargs", "java/lang/SafeVarargs"),
];

pub struct AnnotationProvider;

impl CompletionProvider for AnnotationProvider {
    fn name(&self) -> &'static str {
        "annotation"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::Annotation { prefix } => prefix.as_str(),
            _ => return vec![],
        };

        let prefix_lower = prefix.to_lowercase();
        let mut results = Vec::new();

        // Built-in annotations (java.lang, no import needed)
        for (name, internal) in BUILTIN_ANNOTATIONS {
            if !prefix_lower.is_empty() && !name.to_lowercase().starts_with(&prefix_lower) {
                continue;
            }
            results.push(
                CompletionCandidate::new(
                    Arc::from(*name),
                    name.to_string(),
                    CandidateKind::Annotation,
                    self.name(),
                )
                .with_detail(internal.replace('/', "."))
                .with_score(90.0),
            );
        }

        // Annotations from imports
        let imported = index.resolve_imports(&ctx.existing_imports);
        for meta in &imported {
            if !is_annotation_class(meta) {
                continue;
            }
            let score = match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                Some(s) => s,
                None => continue,
            };
            let fqn = fqn_of(meta);
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&meta.name),
                    meta.name.to_string(),
                    CandidateKind::Annotation,
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

        // Same package annotations
        if let Some(pkg) = ctx.enclosing_package.as_deref() {
            for meta in index.classes_in_package(pkg) {
                if imported_internals.contains(&meta.internal_name) {
                    continue;
                }
                if !is_annotation_class(meta) {
                    continue;
                }
                let score = match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                    Some(s) => s,
                    None => continue,
                };
                results.push(
                    CompletionCandidate::new(
                        Arc::clone(&meta.name),
                        meta.name.to_string(),
                        CandidateKind::Annotation,
                        self.name(),
                    )
                    .with_detail(fqn_of(meta))
                    .with_score(70.0 + score as f32 * 0.1),
                );
            }
        }

        // Global index — all annotation classes (require auto-import)
        for meta in index.iter_all_classes() {
            if imported_internals.contains(&meta.internal_name) {
                continue;
            }
            if !is_annotation_class(meta) {
                continue;
            }
            let score = match fuzzy::fuzzy_match(&prefix_lower, &meta.name.to_lowercase()) {
                Some(s) => s,
                None => continue,
            };
            let fqn = fqn_of(meta);
            let needs_import = is_import_needed(
                &fqn,
                &ctx.existing_imports,
                ctx.enclosing_package.as_deref(),
            );
            let candidate = CompletionCandidate::new(
                Arc::clone(&meta.name),
                meta.name.to_string(),
                CandidateKind::Annotation,
                self.name(),
            )
            .with_detail(fqn.clone())
            .with_score(50.0 + score as f32 * 0.1);

            results.push(if needs_import {
                candidate.with_import(fqn)
            } else {
                candidate
            });
        }

        results
    }
}

fn is_annotation_class(meta: &crate::index::ClassMetadata) -> bool {
    use rust_asm::constants::ACC_ANNOTATION;
    meta.access_flags & ACC_ANNOTATION != 0
}

fn fqn_of(meta: &crate::index::ClassMetadata) -> String {
    match &meta.package {
        Some(pkg) => format!("{}.{}", pkg.replace('/', "."), meta.name),
        None => meta.name.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::completion::providers::CompletionProvider;
    use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex};
    use rust_asm::constants::{ACC_ANNOTATION, ACC_PUBLIC};
    use std::sync::Arc;

    fn make_annotation(pkg: &str, name: &str) -> ClassMetadata {
        ClassMetadata {
            package: Some(Arc::from(pkg)),
            name: Arc::from(name),
            internal_name: Arc::from(format!("{}/{}", pkg, name).as_str()),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC | ACC_ANNOTATION,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
            javadoc: None,
        }
    }

    fn make_class(pkg: &str, name: &str) -> ClassMetadata {
        ClassMetadata {
            package: Some(Arc::from(pkg)),
            name: Arc::from(name),
            internal_name: Arc::from(format!("{}/{}", pkg, name).as_str()),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC, // not an annotation
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
            javadoc: None,
        }
    }

    fn annotation_ctx(prefix: &str, imports: Vec<Arc<str>>, pkg: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Annotation {
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            None,
            None,
            Some(Arc::from(pkg)),
            imports,
        )
    }

    #[test]
    fn test_builtin_override_always_present() {
        let mut idx = GlobalIndex::new();
        let ctx = annotation_ctx("Over", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "Override"),
            "Override should always appear: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_builtin_empty_prefix_returns_all_builtins() {
        let mut idx = GlobalIndex::new();
        let ctx = annotation_ctx("", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        for (name, _) in BUILTIN_ANNOTATIONS {
            assert!(
                results.iter().any(|c| c.label.as_ref() == *name),
                "{} should appear with empty prefix",
                name
            );
        }
    }

    #[test]
    fn test_non_annotation_class_excluded() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_class("com/example", "NotAnAnnotation")]);
        let ctx = annotation_ctx("Not", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        assert!(
            results
                .iter()
                .all(|c| c.label.as_ref() != "NotAnAnnotation"),
            "regular class should not appear in annotation completions"
        );
    }

    #[test]
    fn test_annotation_from_import_appears() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_annotation("org/junit", "Test")]);
        let ctx = annotation_ctx("Te", vec!["org.junit.Test".into()], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "Test"),
            "imported annotation should appear: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_annotation_from_global_index_has_import() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![make_annotation("org/junit", "Test")]);
        let ctx = annotation_ctx("Te", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        let test_candidate = results.iter().find(|c| c.label.as_ref() == "Test");
        assert!(test_candidate.is_some(), "Test annotation should appear");
        assert_eq!(
            test_candidate.unwrap().required_import.as_deref(),
            Some("org.junit.Test"),
            "should carry auto-import"
        );
    }

    #[test]
    fn test_annotation_kind_is_annotation() {
        let mut idx = GlobalIndex::new();
        let ctx = annotation_ctx("Over", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        let c = results
            .iter()
            .find(|c| c.label.as_ref() == "Override")
            .unwrap();
        assert!(
            matches!(c.kind, CandidateKind::Annotation),
            "kind should be Annotation"
        );
    }

    #[test]
    fn test_prefix_filter_case_insensitive() {
        let mut idx = GlobalIndex::new();
        let ctx = annotation_ctx("over", vec![], "com/example");
        let results = AnnotationProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "Override"),
            "case-insensitive prefix should match Override"
        );
    }
}
