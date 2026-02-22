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

        // prefix could be:
        // "" → List all (limited quantity)
        // "Ma" → Fuzzy search by simple name
        // "org.cubewhy.Ma" → Search by package path + simple name

        if prefix.contains('.') {
            // Package path: split into pkg_prefix + simple_prefix
            let last_dot = prefix.rfind('.').unwrap();
            let pkg_prefix = &prefix[..last_dot];
            let simple_prefix = &prefix[last_dot + 1..];
            search_by_package_and_name(index, pkg_prefix, simple_prefix)
        } else {
            // Simple name search
            search_by_simple_name(index, prefix)
        }
    }
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
        Arc::clone(&meta.name),
        fqn.clone(),
        CandidateKind::ClassName,
        "import",
    )
    .with_detail(fqn)
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
    fn test_simple_name_prefix_matches() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("Re"), &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "RealMain"),
            "Re should match RealMain: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_simple_name_insert_text_is_fqn() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("Re"), &mut idx);
        let r = results
            .iter()
            .find(|c| c.label.as_ref() == "RealMain")
            .unwrap();
        assert_eq!(
            r.insert_text, "org.cubewhy.RealMain",
            "insert_text should be FQN"
        );
    }

    #[test]
    fn test_package_prefix_matches_classes_in_pkg() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("org.cubewhy.Ma"), &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "Main"));
        assert!(
            !results.iter().any(|c| c.label.as_ref() == "RealMain"),
            "RealMain doesn't start with 'Ma'"
        );
    }

    #[test]
    fn test_package_prefix_dot_only_lists_all_in_pkg() {
        // "org.cubewhy." → all classes in org.cubewhy
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(labels.contains(&"Main"), "{:?}", labels);
        assert!(labels.contains(&"RealMain"), "{:?}", labels);
    }

    #[test]
    fn test_package_prefix_includes_subpackage_classes() {
        // "org.cubewhy." の実装は pkg == "org/cubewhy" OR starts_with("org/cubewhy/")
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(
            labels.contains(&"StringUtil"),
            "subpackage class should appear: {:?}",
            labels
        );
    }

    #[test]
    fn test_empty_prefix_returns_some() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx(""), &mut idx);
        assert!(
            !results.is_empty(),
            "empty prefix should return classes (up to limit)"
        );
    }

    #[test]
    fn test_no_match_returns_empty() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("Zzz"), &mut idx);
        assert!(results.is_empty(), "no class starts with Zzz");
    }

    #[test]
    fn test_case_insensitive_match() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("real"), &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "RealMain"),
            "should match case-insensitively: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_fqn_format_dot_notation() {
        let mut idx = make_index();
        let results = ImportProvider.provide(&import_ctx("ArrayList"), &mut idx);
        let r = results
            .iter()
            .find(|c| c.label.as_ref() == "ArrayList")
            .unwrap();
        assert_eq!(r.insert_text, "java.util.ArrayList");
        assert!(
            !r.insert_text.contains('/'),
            "FQN should use dots not slashes"
        );
    }
}
