use tower_lsp::lsp_types::*;

use crate::completion::{
    candidate::{CandidateKind, CompletionCandidate},
    import_utils::{extract_imports_from_source, extract_package_from_source, is_import_needed},
};

/// Convert internal completion candidates to LSP CompletionItem
pub fn candidate_to_lsp(candidate: &CompletionCandidate, source: &str) -> CompletionItem {
    let kind = map_kind(&candidate.kind);

    let additional_text_edits = candidate
        .required_import
        .as_ref()
        .filter(|import| !is_already_in_source(import, source))
        .map(|import| {
            let insert_line = find_import_insert_line(source);
            let new_text = make_import_text(import, source);
            vec![TextEdit {
                range: Range {
                    start: Position {
                        line: insert_line,
                        character: 0,
                    },
                    end: Position {
                        line: insert_line,
                        character: 0,
                    },
                },
                new_text,
            }]
        });

    CompletionItem {
        label: candidate.label.to_string(),
        kind: Some(kind),
        detail: candidate.detail.clone(),
        additional_text_edits,
        sort_text: Some(format!("{:010.4}", 10000.0 - candidate.score)),
        ..Default::default()
    }
}

/// 检查 source 中是否已经通过精确 import、通配符 import 或同包覆盖了这个 fqn
fn is_already_in_source(fqn: &str, source: &str) -> bool {
    let existing_imports = extract_imports_from_source(source);
    let enclosing_pkg = extract_package_from_source(source);

    !is_import_needed(fqn, &existing_imports, enclosing_pkg.as_deref())
}

/// Find the line number where the import should be inserted: the first line after the package declaration.
fn find_import_insert_line(source: &str) -> u32 {
    let mut last_package_line: Option<u32> = None;
    let mut last_import_line: Option<u32> = None;

    for (i, line) in source.lines().enumerate() {
        let t = line.trim();
        if t.starts_with("package ") {
            last_package_line = Some(i as u32);
        }
        if t.starts_with("import ") {
            last_import_line = Some(i as u32);
        }
    }

    // Insert it after the last import statement if preferred, otherwise after the package statement, otherwise on line 0
    if let Some(l) = last_import_line {
        return l + 1;
    }
    if let Some(l) = last_package_line {
        return l + 1;
    }
    0
}

/// import Insert text: A blank line is required after package.
fn make_import_text(import: &str, source: &str) -> String {
    let has_existing_imports = source.lines().any(|l| l.trim().starts_with("import "));
    if has_existing_imports {
        // If an import already exists, simply append it.
        format!("import {};\n", import)
    } else {
        // A blank line after the first import: package
        format!("\nimport {};\n", import)
    }
}

fn map_kind(kind: &CandidateKind) -> CompletionItemKind {
    match kind {
        CandidateKind::ClassName => CompletionItemKind::CLASS,
        CandidateKind::Method { .. } => CompletionItemKind::METHOD,
        CandidateKind::StaticMethod { .. } => CompletionItemKind::FUNCTION,
        CandidateKind::Field { .. } => CompletionItemKind::FIELD,
        CandidateKind::StaticField { .. } => CompletionItemKind::CONSTANT,
        CandidateKind::LocalVariable { .. } => CompletionItemKind::VARIABLE,
        CandidateKind::Constructor { .. } => CompletionItemKind::CONSTRUCTOR,
        CandidateKind::Keyword => CompletionItemKind::KEYWORD,
    }
}

/// LSP Position -> Byte Offset within File (Unicode based)
pub fn lsp_pos_to_offset(source: &str, pos: Position) -> Option<usize> {
    crate::language::java::line_col_to_offset(source, pos.line, pos.character)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::candidate::{CandidateKind, CompletionCandidate};
    use std::sync::Arc;

    fn make_candidate(label: &str, import: Option<&str>) -> CompletionCandidate {
        let mut c = CompletionCandidate::new(
            Arc::from(label),
            label.to_string(),
            CandidateKind::ClassName,
            "test",
        );
        c.required_import = import.map(|s| s.to_string());
        c
    }

    // ── extract_imports_from_source ───────────────────────────────────────

    #[test]
    fn test_extract_imports_basic() {
        let src = "package a;\nimport java.util.List;\nimport java.util.Map;\nclass A {}";
        let imports = extract_imports_from_source(src);
        assert_eq!(imports, vec!["java.util.List", "java.util.Map"]);
    }

    #[test]
    fn test_extract_imports_empty() {
        let src = "package a;\nclass A {}";
        assert!(extract_imports_from_source(src).is_empty());
    }

    #[test]
    fn test_extract_imports_wildcard() {
        let src = "import org.cubewhy.*;\nclass A {}";
        let imports = extract_imports_from_source(src);
        assert_eq!(imports, vec!["org.cubewhy.*"]);
    }

    // ── is_already_in_source ──────────────────────────────────────────────

    #[test]
    fn test_already_in_source_exact() {
        let src = "package a;\nimport org.cubewhy.Foo;\nclass A {}";
        assert!(is_already_in_source("org.cubewhy.Foo", src));
    }

    #[test]
    fn test_already_in_source_wildcard() {
        let src = "package a;\nimport org.cubewhy.*;\nclass A {}";
        assert!(is_already_in_source("org.cubewhy.Foo", src));
    }

    #[test]
    fn test_already_in_source_same_package() {
        let src = "package org.cubewhy.a;\nclass A {}";
        assert!(is_already_in_source("org.cubewhy.a.Helper", src));
    }

    #[test]
    fn test_already_in_source_java_lang() {
        let src = "package a;\nclass A {}";
        assert!(is_already_in_source("java.lang.String", src));
    }

    #[test]
    fn test_not_already_in_source() {
        let src = "package a;\nimport java.util.List;\nclass A {}";
        assert!(!is_already_in_source("org.cubewhy.Foo", src));
    }

    // ── candidate_to_lsp 完整场景 ─────────────────────────────────────────

    #[test]
    fn test_no_edit_when_already_imported_exact() {
        let src = "package a;\nimport org.cubewhy.Foo;\nclass A {}";
        let c = make_candidate("Foo", Some("org.cubewhy.Foo"));
        let item = candidate_to_lsp(&c, src);
        assert!(
            item.additional_text_edits
                .as_ref()
                .is_none_or(|e| e.is_empty()),
            "should not insert duplicate import"
        );
    }

    #[test]
    fn test_no_edit_when_covered_by_wildcard() {
        let src = "package a;\nimport org.cubewhy.*;\nclass A {}";
        let c = make_candidate("Foo", Some("org.cubewhy.Foo"));
        let item = candidate_to_lsp(&c, src);
        assert!(
            item.additional_text_edits
                .as_ref()
                .is_none_or(|e| e.is_empty()),
            "should not insert import when wildcard covers it"
        );
    }

    #[test]
    fn test_no_edit_for_same_package_class() {
        let src = "package org.cubewhy.a;\nclass A {}";
        let c = make_candidate("Helper", Some("org.cubewhy.a.Helper"));
        let item = candidate_to_lsp(&c, src);
        assert!(
            item.additional_text_edits
                .as_ref()
                .is_none_or(|e| e.is_empty()),
            "should not insert import for same-package class"
        );
    }

    #[test]
    fn test_no_edit_for_java_lang() {
        let src = "package a;\nclass A {}";
        let c = make_candidate("String", Some("java.lang.String"));
        let item = candidate_to_lsp(&c, src);
        assert!(
            item.additional_text_edits
                .as_ref()
                .is_none_or(|e| e.is_empty()),
            "java.lang classes should not get an import edit"
        );
    }

    #[test]
    fn test_edit_inserted_when_needed() {
        let src = "package a;\nclass A {}";
        let c = make_candidate("List", Some("java.util.List"));
        let item = candidate_to_lsp(&c, src);
        let edits = item.additional_text_edits.unwrap();
        assert_eq!(edits.len(), 1);
        assert!(edits[0].new_text.contains("import java.util.List;"));
    }

    // ── 原有测试（保持不变）──────────────────────────────────────────────

    #[test]
    fn test_import_text_first_import_has_blank_line() {
        let src = "package org.cubewhy;\nclass Main {}\n";
        let text = make_import_text("java.util.List", src);
        assert!(text.starts_with('\n'));
        assert!(text.contains("import java.util.List;"));
    }

    #[test]
    fn test_import_text_subsequent_no_blank_line() {
        let src = "package org.cubewhy;\nimport java.util.List;\nclass Main {}\n";
        let text = make_import_text("java.util.Map", src);
        assert!(!text.starts_with('\n'));
    }

    #[test]
    fn test_import_insert_after_package() {
        let src = "package org.cubewhy;\n\nclass Main {}\n";
        assert_eq!(find_import_insert_line(src), 1);
    }

    #[test]
    fn test_import_insert_after_last_import() {
        let src =
            "package org.cubewhy;\nimport java.util.List;\nimport java.util.Map;\nclass Main {}\n";
        assert_eq!(find_import_insert_line(src), 3);
    }

    #[test]
    fn test_import_insert_no_package_no_import() {
        let src = "class Main {}\n";
        assert_eq!(find_import_insert_line(src), 0);
    }

    #[test]
    fn test_candidate_to_lsp_auto_import_edit() {
        let src = "package org.cubewhy;\nclass Main {}\n";
        let c = make_candidate("ArrayList", Some("java.util.ArrayList"));
        let item = candidate_to_lsp(&c, src);
        let edits = item.additional_text_edits.unwrap();
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "\nimport java.util.ArrayList;\n");
        assert_eq!(edits[0].range.start.line, 1);
    }

    #[test]
    fn test_candidate_to_lsp_no_import_no_edit() {
        let c = make_candidate("String", None);
        let item = candidate_to_lsp(&c, "class A {}");
        assert!(
            item.additional_text_edits.is_none()
                || item.additional_text_edits.as_ref().unwrap().is_empty()
        );
    }

    #[test]
    fn test_sort_text_higher_score_sorts_first() {
        let make = |score: f32| {
            let mut c = CompletionCandidate::new(
                Arc::from("x"),
                "x".to_string(),
                CandidateKind::Keyword,
                "test",
            );
            c.score = score;
            candidate_to_lsp(&c, "")
        };
        let high = make(90.0);
        let low = make(10.0);
        assert!(high.sort_text < low.sort_text);
    }
}
