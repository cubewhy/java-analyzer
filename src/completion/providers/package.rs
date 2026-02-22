use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
use std::sync::Arc;

pub struct PackageProvider;

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
            CursorLocation::Import { prefix } => provide_import(prefix, index),
            // 表达式位置：只有当 prefix 看起来像包路径时触发
            CursorLocation::Expression { prefix } | CursorLocation::TypeAnnotation { prefix } => {
                if is_package_like(prefix) {
                    provide_import(prefix, index)
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }
}

/// prefix 是否看起来像包路径（含小写字母开头的段 + 点）
fn is_package_like(prefix: &str) -> bool {
    if !prefix.contains('.') {
        return false;
    }
    // 第一段必须小写字母开头
    prefix.chars().next().is_some_and(|c| c.is_lowercase())
}

fn provide_import(prefix: &str, index: &mut GlobalIndex) -> Vec<CompletionCandidate> {
    if prefix.is_empty() {
        return vec![];
    }

    match prefix.rfind('.') {
        Some(dot_pos) => {
            // "org.cubewhy." 或 "org.cubewhy.Ma"
            let pkg_path = &prefix[..dot_pos]; // "org.cubewhy"
            let name_prefix = &prefix[dot_pos + 1..]; // "" 或 "Ma"
            let internal_pkg = pkg_path.replace('.', "/");

            let mut results = Vec::new();

            // ── 当前包下的类 ──────────────────────────────────────────
            for meta in index.classes_in_package(&internal_pkg) {
                if !name_prefix.is_empty()
                    && !meta
                        .name
                        .to_lowercase()
                        .starts_with(&name_prefix.to_lowercase())
                {
                    continue;
                }
                let fqn = format!("{}.{}", pkg_path, meta.name);
                results.push(
                    CompletionCandidate::new(
                        Arc::clone(&meta.name),
                        fqn.clone(),
                        CandidateKind::ClassName,
                        "package",
                    )
                    .with_detail(fqn)
                    .with_score(70.0),
                );
            }

            // ── 子包 ─────────────────────────────────────────────────
            let pkg_prefix_slash = format!("{}/", internal_pkg);
            let mut sub_packages: std::collections::BTreeSet<String> =
                std::collections::BTreeSet::new();
            for meta in index.iter_all_classes() {
                if let Some(pkg) = &meta.package
                    && pkg.starts_with(&pkg_prefix_slash)
                {
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
            for sub in sub_packages {
                // insert_text 末尾加 "." 方便继续输入
                let insert = format!("{}.{}.", pkg_path, sub);
                let label = format!("{}.{}", pkg_path, sub);
                results.push(
                    CompletionCandidate::new(
                        Arc::from(sub.as_str()),
                        insert,
                        CandidateKind::Package,
                        "package",
                    )
                    .with_detail(label)
                    .with_score(65.0),
                );
            }

            results
        }

        None => {
            // "org" — 无点，按顶层包名前缀匹配
            // 收集所有顶层包名（第一段）
            let mut tops: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
            let prefix_lower = prefix.to_lowercase();
            for meta in index.iter_all_classes() {
                if let Some(pkg) = &meta.package {
                    let top = pkg.split('/').next().unwrap_or("");
                    if !top.is_empty() && top.to_lowercase().starts_with(&prefix_lower) {
                        tops.insert(top.to_string());
                    }
                }
            }
            tops.into_iter()
                .map(|top| {
                    let insert = format!("{}.", top);
                    CompletionCandidate::new(
                        Arc::from(top.as_str()),
                        insert,
                        CandidateKind::Package,
                        "package",
                    )
                    .with_detail(format!("package {}", top))
                    .with_score(60.0)
                })
                .collect()
        }
    }
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
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(labels.contains(&"Main"), "{:?}", labels);
        assert!(labels.contains(&"Main2"), "{:?}", labels);
    }

    #[test]
    fn test_import_pkg_dot_lists_sub_packages() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(
            labels.contains(&"utils"),
            "sub-package missing: {:?}",
            labels
        );
    }

    #[test]
    fn test_import_pkg_with_name_prefix() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy.Ma"), &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "Main"));
        assert!(results.iter().any(|c| c.label.as_ref() == "Main2"));
        assert!(results.iter().all(|c| c.label.as_ref() != "Other"));
    }

    #[test]
    fn test_import_insert_text_is_fqn() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy.Ma"), &mut idx);
        let main = results.iter().find(|c| c.label.as_ref() == "Main").unwrap();
        assert_eq!(main.insert_text, "org.cubewhy.Main");
    }

    #[test]
    fn test_sub_package_insert_text_ends_with_dot() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let utils = results
            .iter()
            .find(|c| c.label.as_ref() == "utils")
            .unwrap();
        assert!(utils.insert_text.ends_with('.'), "{:?}", utils.insert_text);
    }

    #[test]
    fn test_sub_package_kind_is_package() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org.cubewhy."), &mut idx);
        let utils = results
            .iter()
            .find(|c| c.label.as_ref() == "utils")
            .unwrap();
        assert_eq!(utils.kind, CandidateKind::Package);
    }

    #[test]
    fn test_top_level_package_no_dot() {
        // "org" → 返回顶层包 org（insert_text = "org."）
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx("org"), &mut idx);
        let org = results.iter().find(|c| c.label.as_ref() == "org");
        assert!(
            org.is_some(),
            "should suggest top-level package 'org': {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
        assert_eq!(org.unwrap().insert_text, "org.");
        assert_eq!(org.unwrap().kind, CandidateKind::Package);
    }

    #[test]
    fn test_expression_package_prefix_triggers() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&expr_ctx("org.cubewhy."), &mut idx);
        assert!(
            !results.is_empty(),
            "package prefix in expression should trigger"
        );
    }

    #[test]
    fn test_expression_no_dot_no_package_completion() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&expr_ctx("Main"), &mut idx);
        assert!(results.is_empty(), "no dot = no package completion");
    }

    #[test]
    fn test_empty_prefix_no_crash() {
        let mut idx = make_index();
        let results = PackageProvider.provide(&import_ctx(""), &mut idx);
        assert!(results.is_empty());
    }

    #[test]
    fn test_import_no_dot_returns_top_level_packages() {
        // "import Main" → 按类名 auto-import（这个交给 ImportProvider，PackageProvider 这里返回空）
        // PackageProvider 只处理包路径，单词无点时只在 Import 场景做顶层包匹配
        let mut idx = make_index();
        // "Main" 首字母大写，不是包路径
        // 但 import 场景下无点：这个测试验证不崩溃即可
        let results = PackageProvider.provide(&import_ctx("Main"), &mut idx);
        // 首字母大写，is_package_like=false for expression，但 Import 分支直接调用 provide_import
        // provide_import("Main") → rfind('.') = None → 按顶层包前缀 "main" 匹配 → 不匹配任何包
        assert!(
            results.is_empty(),
            "uppercase prefix should not match packages: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }
}
