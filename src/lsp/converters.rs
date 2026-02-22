use tower_lsp::lsp_types::*;

use crate::completion::candidate::{CandidateKind, CompletionCandidate};

/// Convert internal completion candidates to LSP CompletionItem
pub fn candidate_to_lsp(candidate: &CompletionCandidate, source: &str) -> CompletionItem {
    let kind = map_kind(&candidate.kind);

    let additional_text_edits = candidate.required_import.as_ref().map(|import| {
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

    #[test]
    fn test_import_text_first_import_has_blank_line() {
        let src = "package org.cubewhy;\nclass Main {}\n";
        let text = make_import_text("java.util.List", src);
        assert!(
            text.starts_with('\n'),
            "first import should be preceded by blank line"
        );
        assert!(text.contains("import java.util.List;"));
    }

    #[test]
    fn test_import_text_subsequent_no_blank_line() {
        let src = "package org.cubewhy;\nimport java.util.List;\nclass Main {}\n";
        let text = make_import_text("java.util.Map", src);
        assert!(
            !text.starts_with('\n'),
            "subsequent import should not add extra blank line"
        );
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
    fn test_import_insert_package_only() {
        let src = "package org.cubewhy;\nclass Main {}\n";
        assert_eq!(find_import_insert_line(src), 1);
    }

    #[test]
    fn test_import_insert_prefers_last_import_over_package() {
        // If there are both `package` and `import` directives, the `import` directive should be inserted after the last `import` directive.
        let src = "package org.cubewhy;\nimport java.util.List;\nclass Main {}\n";
        assert_eq!(find_import_insert_line(src), 2);
    }

    #[test]
    fn test_candidate_to_lsp_auto_import_edit() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;

        // No existing import in the source code → Add a blank line before the first import
        let src = "package org.cubewhy;\nclass Main {}\n";
        let mut c = CompletionCandidate::new(
            Arc::from("ArrayList"),
            "ArrayList".to_string(),
            CandidateKind::ClassName,
            "test",
        );
        c.required_import = Some("java.util.ArrayList".to_string());

        let item = candidate_to_lsp(&c, src);
        let edits = item.additional_text_edits.unwrap();
        assert_eq!(edits.len(), 1);

        // The first import statement has a blank line before it: "\nimport ...;\n"
        assert_eq!(edits[0].new_text, "\nimport java.util.ArrayList;\n");

        // Inserted on the first line after package
        assert_eq!(edits[0].range.start.line, 1);
    }

    #[test]
    fn test_candidate_to_lsp_auto_import_edit_with_existing_import() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;

        // The source code already contains imports → No extra blank lines are added when appending.
        let src = "package org.cubewhy;\nimport java.util.List;\nclass Main {}\n";
        let mut c = CompletionCandidate::new(
            Arc::from("ArrayList"),
            "ArrayList".to_string(),
            CandidateKind::ClassName,
            "test",
        );
        c.required_import = Some("java.util.ArrayList".to_string());

        let item = candidate_to_lsp(&c, src);
        let edits = item.additional_text_edits.unwrap();
        assert_eq!(edits.len(), 1);

        // If an import already exists: append it directly without adding an extra blank line.
        assert_eq!(edits[0].new_text, "import java.util.ArrayList;\n");

        // Inserted on the second line after the last import.
        assert_eq!(edits[0].range.start.line, 2);
    }

    #[test]
    fn test_candidate_to_lsp_no_import_no_edit() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;

        let c = CompletionCandidate::new(
            Arc::from("String"),
            "String".to_string(),
            CandidateKind::ClassName,
            "test",
        );
        // required_import = None
        let item = candidate_to_lsp(&c, "class A {}");
        assert!(
            item.additional_text_edits.is_none()
                || item.additional_text_edits.as_ref().unwrap().is_empty()
        );
    }

    #[test]
    fn test_sort_text_higher_score_sorts_first() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;

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
        // sort_text uses lexicographical order; a higher score should result in a smaller sort_text.
        assert!(
            high.sort_text < low.sort_text,
            "high score={:?} should sort before low score={:?}",
            high.sort_text,
            low.sort_text
        );
    }
}
