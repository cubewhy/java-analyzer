use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
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
    let fqn = match &meta.package {
        Some(pkg) => format!("{}.{}", pkg.replace('/', "."), meta.name),
        None => meta.name.to_string(),
    };
    CompletionCandidate::new(
        Arc::clone(&meta.name),
        fqn.clone(),
        CandidateKind::ClassName,
        "import",
    )
    .with_detail(fqn)
}
