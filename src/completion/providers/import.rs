use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::{completion::import_utils::fqn_of_meta, index::GlobalIndex};
use std::sync::Arc;

pub struct ImportProvider;

impl CompletionProvider for ImportProvider {
    fn name(&self) -> &'static str {
        "import"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::Import { prefix } => prefix.as_str(),
            _ => return vec![],
        };
        crate::completion::import_completion::candidates_for_import(prefix, index)
    }
}

fn search_by_package_prefix(index: &GlobalIndex, prefix: &str) -> Vec<CompletionCandidate> {
    let internal_prefix = prefix.replace('.', "/");
    let internal_prefix = internal_prefix.trim_end_matches('/');
    if internal_prefix.len() < 2 {
        return vec![];
    }
    tracing::debug!(
        internal_prefix = internal_prefix,
        "search_by_package_prefix"
    );

    let results: Vec<_> = index
        .iter_all_classes()
        .filter(|meta| {
            meta.package
                .as_ref()
                .is_some_and(|p| p.as_ref().starts_with(internal_prefix))
        })
        .take(50)
        // TODO: Intellij IDEA don't use this method to filter results
        .map(make_import_candidate)
        .collect();

    tracing::debug!(
        count = results.len(),
        internal_prefix,
        "search_by_package_prefix result"
    );

    results
}

/// Fuzzy search by simple name
fn search_by_simple_name(index: &GlobalIndex, prefix: &str) -> Vec<CompletionCandidate> {
    // Iterate through all classes, filtering by simple name prefix
    // GlobalIndex requires an iterator
    index
        .iter_all_classes()
        .filter(|meta| {
            if prefix.is_empty() {
                return true;
            }
            meta.name.to_lowercase().starts_with(&prefix.to_lowercase())
        })
        .take(50)
        .map(make_import_candidate)
        .collect()
}

/// Filter by package path prefix + simple name
fn search_by_package_and_name(
    index: &GlobalIndex,
    pkg_prefix: &str,
    simple_prefix: &str,
) -> Vec<CompletionCandidate> {
    // pkg_prefix: "org.cubewhy" → "org/cubewhy"
    let internal_pkg = pkg_prefix.replace('.', "/");

    index
        .iter_all_classes()
        .filter(|meta| {
            let pkg_matches = meta
                .package
                .as_ref()
                .map(|p| {
                    p.as_ref() == internal_pkg.as_str()
                        || p.as_ref().starts_with(&format!("{}/", internal_pkg))
                })
                .unwrap_or(false);

            let name_matches = simple_prefix.is_empty()
                || meta
                    .name
                    .to_lowercase()
                    .starts_with(&simple_prefix.to_lowercase());

            pkg_matches && name_matches
        })
        .take(50)
        .map(make_import_candidate)
        .collect()
}

fn make_import_candidate(meta: &Arc<crate::index::ClassMetadata>) -> CompletionCandidate {
    let fqn = fqn_of_meta(meta);
    CompletionCandidate::new(
        Arc::from(fqn.as_str()),
        fqn.clone(),
        CandidateKind::ClassName,
        "import",
    )
    .with_detail(meta.name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex};
    use rust_asm::constants::ACC_PUBLIC;
    use std::sync::Arc;

    fn make_index() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            make_cls("org/cubewhy", "Main"),
            make_cls("org/cubewhy", "RealMain"),
            make_cls("org/cubewhy/utils", "StringUtil"),
            make_cls("java/util", "ArrayList"),
            make_cls("java/util", "HashMap"),
        ]);
        idx
    }

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

    fn import_ctx(prefix: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Import {
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            None,
            None,
            None,
            vec![],
        )
    }

    #[test]
    fn test_non_import_location_returns_empty() {
        let mut idx = make_index();
        let ctx = CompletionContext::new(
            CursorLocation::Expression {
                prefix: "Ma".to_string(),
            },
            "Ma",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        assert!(ImportProvider.provide(&ctx, &mut idx).is_empty());
    }

    #[test]
    fn test_delegates_to_import_completion() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("org.cubewhy.Ma"), &mut idx);
        assert!(
            results
                .iter()
                .any(|c| c.label.as_ref() == "org.cubewhy.Main"),
            "{:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }
}
