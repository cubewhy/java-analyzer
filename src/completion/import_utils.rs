use crate::index::{ClassMetadata, GlobalIndex};
use std::sync::Arc;

/// 判断一个 FQN 是否已被现有 imports 覆盖（精确 + 通配符 + 同包）
///
/// - `fqn`: 点分 FQN，如 "org.cubewhy.RandomClass"
/// - `existing_imports`: 文件中已有的 import 语句
/// - `enclosing_package`: 当前文件所在包（内部格式，如 "org/cubewhy/a"）
/// 判断一个 FQN 是否需要插入 import 语句
///
/// 不需要 import 的情况：
/// 1. 精确匹配已有 import
/// 2. 通配符 import 已覆盖（同一层包，不跨子包）
/// 3. 同包类（enclosing_package 与 fqn 的包相同）
/// 4. java.lang 包（自动导入）
/// 5. 默认包（无包名的类）
pub fn is_import_needed(
    fqn: &str,
    existing_imports: &[String],
    enclosing_package: Option<&str>,
) -> bool {
    // 5. 默认包：没有 '.' 说明是默认包的类，不需要 import
    if !fqn.contains('.') {
        return false;
    }

    // 4. java.lang 自动导入
    if is_java_lang(fqn) {
        return false;
    }

    // 1. 精确匹配
    if existing_imports.iter().any(|imp| imp == fqn) {
        return false;
    }

    // 2. 通配符匹配
    if existing_imports.iter().any(|imp| wildcard_covers(imp, fqn)) {
        return false;
    }

    // 3. 同包
    if let Some(enc_pkg) = enclosing_package {
        if same_package(fqn, enc_pkg) {
            return false;
        }
    }

    true
}

/// "java.lang.*" 或 "java.lang.String" 等 java.lang 下的类不需要显式 import
fn is_java_lang(fqn: &str) -> bool {
    // 只有直接在 java.lang 包下的类才自动导入，子包不算
    // e.g. "java.lang.String" → true
    //      "java.lang.reflect.Method" → false
    if let Some(rest) = fqn.strip_prefix("java.lang.") {
        // rest 不能含有 '.'（子包）
        !rest.contains('.')
    } else {
        false
    }
}

/// 判断 fqn 是否与 enclosing_package 同包
/// enclosing_package 是内部格式（"org/cubewhy/a"），fqn 是点分格式（"org.cubewhy.a.Main"）
fn same_package(fqn: &str, enclosing_package: &str) -> bool {
    if enclosing_package.is_empty() {
        // 默认包：fqn 也必须是默认包（无 '.'）
        return !fqn.contains('.');
    }
    let enc_dot = enclosing_package.replace('/', ".");
    match fqn.rfind('.') {
        Some(pos) => fqn[..pos] == enc_dot,
        None => false, // fqn 无包，enclosing 有包 → 不同包
    }
}

/// 判断通配符 import 是否覆盖 fqn
/// "org.cubewhy.*" covers "org.cubewhy.Foo" but NOT "org.cubewhy.sub.Bar"
fn wildcard_covers(wildcard_import: &str, fqn: &str) -> bool {
    let pkg = match wildcard_import.strip_suffix(".*") {
        Some(p) => p,
        None => return false,
    };
    match fqn.strip_prefix(pkg) {
        Some(rest) => rest.starts_with('.') && !rest[1..].contains('.'),
        None => false,
    }
}

/// 从简单类名解析到内部名（FQN）
/// 搜索顺序：已 import → 同包 → 全局唯一
pub fn resolve_simple_to_internal(
    simple: &str,
    existing_imports: &[String],
    enclosing_package: Option<&str>,
    index: &GlobalIndex,
) -> Option<Arc<str>> {
    // 1. 从 imports 解析
    let imported = index.resolve_imports(existing_imports);
    if let Some(m) = imported.iter().find(|m| m.name.as_ref() == simple) {
        return Some(Arc::clone(&m.internal_name));
    }

    // 2. 同包
    if let Some(pkg) = enclosing_package {
        let classes = index.classes_in_package(pkg);
        if let Some(m) = classes.iter().find(|m| m.name.as_ref() == simple) {
            return Some(Arc::clone(&m.internal_name));
        }
    }

    // 3. 全局唯一匹配（优先同包）
    let candidates = index.get_classes_by_simple_name(simple);
    if !candidates.is_empty() {
        if let Some(pkg) = enclosing_package {
            if let Some(m) = candidates
                .iter()
                .find(|c| c.package.as_deref() == Some(pkg))
            {
                return Some(Arc::clone(&m.internal_name));
            }
        }
        return Some(Arc::clone(&candidates[0].internal_name));
    }

    None
}

/// 给定一个 ClassMetadata，计算其点分 FQN
pub fn fqn_of_meta(meta: &ClassMetadata) -> String {
    match &meta.package {
        Some(pkg) => format!("{}.{}", pkg.replace('/', "."), meta.name),
        None => meta.name.to_string(),
    }
}

/// 从 source 文本提取已有 import 列表
pub fn extract_imports_from_source(source: &str) -> Vec<String> {
    source
        .lines()
        .filter_map(|line| {
            let t = line.trim();
            t.strip_prefix("import ")
                .map(|rest| rest.trim_end_matches(';').trim().to_string())
        })
        .filter(|s| !s.is_empty())
        .collect()
}

/// 从 Java/Kotlin source 文本中提取包名（内部格式，如 "org/cubewhy/a"）
/// 用于 converter 层的兜底检查
pub fn extract_package_from_source(source: &str) -> Option<String> {
    source.lines().find_map(|line| {
        let t = line.trim();
        t.strip_prefix("package ")
            .map(|rest| rest.trim_end_matches(';').trim().replace('.', "/"))
            .filter(|s| !s.is_empty())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_java_lang_string_not_needed() {
        assert!(!is_import_needed("java.lang.String", &[], None));
    }

    #[test]
    fn test_java_lang_object_not_needed() {
        assert!(!is_import_needed("java.lang.Object", &[], None));
    }

    #[test]
    fn test_java_lang_subpackage_needed() {
        // java.lang.reflect.Method 不是直接在 java.lang 下
        assert!(is_import_needed("java.lang.reflect.Method", &[], None));
    }

    #[test]
    fn test_java_lang_runnable_not_needed() {
        assert!(!is_import_needed("java.lang.Runnable", &[], None));
    }

    #[test]
    fn test_default_package_not_needed() {
        // 无包名的类（默认包）不需要 import
        assert!(!is_import_needed("MyClass", &[], None));
        assert!(!is_import_needed("MyClass", &[], Some("org/cubewhy")));
    }

    // ── 精确 import ───────────────────────────────────────────────────────

    #[test]
    fn test_exact_import_not_needed() {
        assert!(!is_import_needed(
            "org.cubewhy.RandomClass",
            &["org.cubewhy.RandomClass".to_string()],
            None,
        ));
    }

    #[test]
    fn test_exact_import_different_class_needed() {
        assert!(is_import_needed(
            "org.cubewhy.OtherClass",
            &["org.cubewhy.RandomClass".to_string()],
            None,
        ));
    }

    // ── 通配符 import ─────────────────────────────────────────────────────

    #[test]
    fn test_wildcard_covers_direct_child() {
        assert!(!is_import_needed(
            "org.cubewhy.RandomClass",
            &["org.cubewhy.*".to_string()],
            None,
        ));
    }

    #[test]
    fn test_wildcard_does_not_cover_subpackage() {
        assert!(is_import_needed(
            "org.cubewhy.sub.Foo",
            &["org.cubewhy.*".to_string()],
            None,
        ));
    }

    #[test]
    fn test_wildcard_does_not_cover_sibling_package() {
        // "org.cubewhy2.Foo" is NOT covered by "org.cubewhy.*"
        assert!(is_import_needed(
            "org.cubewhy2.Foo",
            &["org.cubewhy.*".to_string()],
            None,
        ));
    }

    #[test]
    fn test_multiple_wildcards_one_covers() {
        assert!(!is_import_needed(
            "java.util.List",
            &["org.cubewhy.*".to_string(), "java.util.*".to_string()],
            None,
        ));
    }

    // ── 同包 ──────────────────────────────────────────────────────────────

    #[test]
    fn test_same_package_not_needed() {
        assert!(!is_import_needed(
            "org.cubewhy.a.Main",
            &[],
            Some("org/cubewhy/a"),
        ));
    }

    #[test]
    fn test_same_package_slash_notation() {
        assert!(!is_import_needed(
            "org.cubewhy.a.Helper",
            &[],
            Some("org/cubewhy/a"),
        ));
    }

    #[test]
    fn test_different_package_needed() {
        assert!(is_import_needed(
            "org.cubewhy.RandomClass",
            &[],
            Some("org/cubewhy/a"),
        ));
    }

    #[test]
    fn test_subpackage_is_different_package() {
        // org.cubewhy.a.sub.Foo 和 enclosing org/cubewhy/a 不同包
        assert!(is_import_needed(
            "org.cubewhy.a.sub.Foo",
            &[],
            Some("org/cubewhy/a"),
        ));
    }

    // ── 组合场景 ──────────────────────────────────────────────────────────

    #[test]
    fn test_already_imported_via_wildcard_and_same_pkg() {
        // 通配符覆盖，同时也在同包 → 不需要
        assert!(!is_import_needed(
            "org.cubewhy.a.Foo",
            &["org.cubewhy.a.*".to_string()],
            Some("org/cubewhy/a"),
        ));
    }

    #[test]
    fn test_no_imports_no_package_foreign_class_needed() {
        assert!(is_import_needed("org.cubewhy.Foo", &[], None));
    }

    // ── wildcard_covers 单元测试 ──────────────────────────────────────────

    #[test]
    fn test_wildcard_covers_basic() {
        assert!(wildcard_covers("org.cubewhy.*", "org.cubewhy.Foo"));
    }

    #[test]
    fn test_wildcard_not_covers_subpkg() {
        assert!(!wildcard_covers("org.cubewhy.*", "org.cubewhy.sub.Foo"));
    }

    #[test]
    fn test_wildcard_not_covers_unrelated() {
        assert!(!wildcard_covers("org.cubewhy.*", "org.other.Foo"));
    }

    #[test]
    fn test_non_wildcard_import_not_covers() {
        assert!(!wildcard_covers("org.cubewhy.Foo", "org.cubewhy.Foo"));
    }

    // ── same_package 单元测试 ─────────────────────────────────────────────

    #[test]
    fn test_same_package_basic() {
        assert!(same_package("org.cubewhy.a.Foo", "org/cubewhy/a"));
    }

    #[test]
    fn test_same_package_different() {
        assert!(!same_package("org.cubewhy.Foo", "org/cubewhy/a"));
    }

    #[test]
    fn test_same_package_default_pkg_fqn_no_dot() {
        // enclosing 是默认包（空串），fqn 也是默认包
        assert!(same_package("Foo", ""));
    }

    #[test]
    fn test_same_package_default_pkg_fqn_has_dot() {
        // enclosing 是默认包，fqn 有包 → 不同
        assert!(!same_package("org.Foo", ""));
    }

    // ── is_java_lang 单元测试 ─────────────────────────────────────────────

    #[test]
    fn test_is_java_lang_direct() {
        assert!(is_java_lang("java.lang.String"));
        assert!(is_java_lang("java.lang.Integer"));
        assert!(is_java_lang("java.lang.Thread"));
    }

    #[test]
    fn test_is_java_lang_subpackage() {
        assert!(!is_java_lang("java.lang.reflect.Method"));
        assert!(!is_java_lang("java.lang.annotation.Retention"));
    }

    #[test]
    fn test_is_not_java_lang() {
        assert!(!is_java_lang("java.util.List"));
        assert!(!is_java_lang("org.cubewhy.Foo"));
    }

    #[test]
    fn test_extract_package_basic() {
        let src = "package org.cubewhy.a;\nclass A {}";
        assert_eq!(
            extract_package_from_source(src).as_deref(),
            Some("org/cubewhy/a")
        );
    }

    #[test]
    fn test_extract_package_none() {
        let src = "class A {}";
        assert!(extract_package_from_source(src).is_none());
    }

    #[test]
    fn test_extract_package_default_pkg() {
        // "package ;" 这种畸形情况返回 None
        let src = "class A {}";
        assert!(extract_package_from_source(src).is_none());
    }
}
