use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
use std::sync::Arc;

pub struct PackageProvider;

// TODO: this module should be completely rewrited
// it doesn't work at all.

impl CompletionProvider for PackageProvider {
    fn name(&self) -> &'static str {
        "package"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        match &ctx.location {
            CursorLocation::Import { prefix } => provide_for_import_prefix(prefix, index),
            CursorLocation::Expression { prefix } | CursorLocation::TypeAnnotation { prefix } => {
                if prefix.contains('.') && prefix.chars().next().map_or(false, |c| c.is_lowercase())
                {
                    provide_for_package_prefix(prefix, index)
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }
}

fn provide_for_import_prefix(prefix: &str, index: &mut GlobalIndex) -> Vec<CompletionCandidate> {
    if prefix.is_empty() {
        return vec![];
    }

    if let Some(dot_pos) = prefix.rfind('.') {
        let pkg_path = &prefix[..dot_pos];
        let name_prefix = &prefix[dot_pos + 1..];
        let internal_pkg = pkg_path.replace('.', "/");

        let mut results = Vec::new();

        for meta in index.classes_in_package(&internal_pkg) {
            if name_prefix.is_empty()
                || meta
                    .name
                    .to_lowercase()
                    .starts_with(&name_prefix.to_lowercase())
            {
                let fqn = format!("{}.{}", pkg_path, meta.name);
                results.push(
                    CompletionCandidate::new(
                        Arc::clone(&meta.name),
                        fqn.clone(),
                        CandidateKind::ClassName,
                        "package",
                    )
                    .with_detail(fqn),
                );
            }
        }

        let pkg_prefix_slash = format!("{}/", internal_pkg);
        let mut sub_packages: std::collections::BTreeSet<String> =
            std::collections::BTreeSet::new();

        for meta in index.iter_all_classes() {
            if let Some(pkg) = &meta.package {
                if pkg.starts_with(&pkg_prefix_slash) {
                    // 取下一级子包名
                    let rest = &pkg[pkg_prefix_slash.len()..];
                    let sub = rest.split('/').next().unwrap_or("");
                    if !sub.is_empty()
                        && (name_prefix.is_empty()
                            || sub.to_lowercase().starts_with(&name_prefix.to_lowercase()))
                    {
                        sub_packages.insert(sub.to_string());
                    }
                }
            }
        }

        for sub in sub_packages {
            let label = format!("{}.{}.", pkg_path, sub);
            results.push(
                CompletionCandidate::new(
                    Arc::from(sub.as_str()),
                    label.clone(),
                    CandidateKind::ClassName, // 暂用 ClassName，后续可加 Package kind
                    "package",
                )
                .with_detail(format!("package {}", label.trim_end_matches('.'))),
            );
        }

        results
    } else {
        // 没有点：按顶层包名补全（很少见，暂时用类名搜索兜底）
        vec![]
    }
}

fn provide_for_package_prefix(prefix: &str, index: &mut GlobalIndex) -> Vec<CompletionCandidate> {
    provide_for_import_prefix(prefix, index)
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
            make_cls("org/cubewhy", "Main2"),
            make_cls("org/cubewhy/utils", "StringUtil"),
            make_cls("org/cubewhy/utils", "FileUtil"),
            make_cls("com/other", "Other"),
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

    fn expr_ctx(prefix: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Expression {
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
    fn test_import_pkg_dot_lists_classes() {
        // "org.cubewhy." → Main, Main2
        let mut idx = make_index();
        let ctx = import_ctx("org.cubewhy.");
        let results = PackageProvider.provide(&ctx, &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(labels.contains(&"Main"), "labels={:?}", labels);
        assert!(labels.contains(&"Main2"), "labels={:?}", labels);
    }

    #[test]
    fn test_import_pkg_dot_lists_sub_packages() {
        // "org.cubewhy." → utils (子包)
        let mut idx = make_index();
        let ctx = import_ctx("org.cubewhy.");
        let results = PackageProvider.provide(&ctx, &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(
            labels.contains(&"utils"),
            "should show sub-package: {:?}",
            labels
        );
    }

    #[test]
    fn test_import_pkg_with_name_prefix() {
        // "org.cubewhy.Ma" → Main, Main2
        let mut idx = make_index();
        let ctx = import_ctx("org.cubewhy.Ma");
        let results = PackageProvider.provide(&ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "Main"));
        assert!(results.iter().any(|c| c.label.as_ref() == "Main2"));
        assert!(results.iter().all(|c| c.label.as_ref() != "Other"));
    }

    #[test]
    fn test_import_insert_text_is_fqn() {
        // insert_text 应该是完整 FQN（用于 import 语句）
        let mut idx = make_index();
        let ctx = import_ctx("org.cubewhy.Ma");
        let results = PackageProvider.provide(&ctx, &mut idx);
        let main = results.iter().find(|c| c.label.as_ref() == "Main").unwrap();
        assert_eq!(main.insert_text, "org.cubewhy.Main");
    }

    #[test]
    fn test_sub_package_insert_text_ends_with_dot() {
        let mut idx = make_index();
        let ctx = import_ctx("org.cubewhy.");
        let results = PackageProvider.provide(&ctx, &mut idx);
        let utils = results
            .iter()
            .find(|c| c.label.as_ref() == "utils")
            .unwrap();
        assert!(
            utils.insert_text.ends_with('.'),
            "sub-package insert_text should end with '.': {:?}",
            utils.insert_text
        );
    }

    #[test]
    fn test_expression_package_prefix_triggers() {
        // 表达式位置："org.cubewhy." → 触发包补全
        let mut idx = make_index();
        let ctx = expr_ctx("org.cubewhy.");
        let results = PackageProvider.provide(&ctx, &mut idx);
        assert!(
            !results.is_empty(),
            "package prefix in expression should trigger completion"
        );
    }

    #[test]
    fn test_expression_no_dot_no_package_completion() {
        // 无点：不触发包补全（交给 ExpressionProvider）
        let mut idx = make_index();
        let ctx = expr_ctx("Main");
        let results = PackageProvider.provide(&ctx, &mut idx);
        assert!(results.is_empty());
    }

    #[test]
    fn test_empty_prefix_no_crash() {
        let mut idx = make_index();
        let ctx = import_ctx("");
        let results = PackageProvider.provide(&ctx, &mut idx);
        assert!(results.is_empty());
    }
}
