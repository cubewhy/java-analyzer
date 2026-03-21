use super::Db;
use crate::salsa_db::{ParseTreeOrigin, ParseTreeSnapshot, SourceFile};
/// Parse queries - handle syntax tree parsing and basic extraction
///
/// These queries are the foundation of incremental parsing. When a file's
/// content changes, only these queries (and their dependents) are invalidated.
use std::sync::Arc;
use tree_sitter::{InputEdit, Parser, Point, Tree};

/// Metadata about a parsed syntax tree
///
/// We don't store the tree-sitter Tree itself because it doesn't implement
/// the traits required by Salsa. Instead, we store metadata and re-parse
/// when needed (tree-sitter is fast enough for this).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseResult {
    pub root_kind: Arc<str>,
    pub has_error: bool,
    pub node_count: usize,
    /// Hash of the content for change detection
    pub content_hash: u64,
}

/// Parse a source file and extract syntax tree metadata
///
/// This is memoized by Salsa - it will only re-parse when the file content changes.
#[salsa::tracked]
pub fn parse_file(db: &dyn Db, file: SourceFile) -> ParseResult {
    let content = file.content(db);
    let (root_kind, has_error, node_count) = if let Some(tree) = parse_tree(db, file) {
        let root = tree.root_node();
        (
            Arc::from(root.kind()),
            root.has_error(),
            root.descendant_count(),
        )
    } else {
        (Arc::from("error"), true, 0)
    };

    // Simple hash for change detection
    let content_hash = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        content.hash(&mut hasher);
        hasher.finish()
    };

    ParseResult {
        root_kind,
        has_error,
        node_count,
        content_hash,
    }
}

/// Parse a Salsa source file using the last cached tree when available.
///
/// The cache is stored outside Salsa because `tree_sitter::Tree` is not hashable,
/// but it is still keyed by the stable Salsa file identity and current content.
pub fn parse_tree(db: &dyn Db, file: SourceFile) -> Option<Tree> {
    let content = file.content(db);
    let language_id = file.language_id(db);
    let file_id = file.file_id(db).clone();
    let cached = db.cached_parse_tree(&file_id);

    if let Some(snapshot) = cached.as_ref()
        && snapshot.content.as_ref() == content
        && snapshot.language_id.as_ref() == language_id.as_ref()
    {
        return Some(snapshot.tree.clone());
    }

    let tree = parse_tree_from_snapshot(content, language_id.as_ref(), cached.as_ref());
    match tree {
        Some((tree, origin)) => {
            db.store_parse_tree(
                file_id,
                ParseTreeSnapshot {
                    content: Arc::from(content.as_str()),
                    language_id: Arc::clone(&language_id),
                    tree: tree.clone(),
                    origin,
                },
            );
            Some(tree)
        }
        None => {
            db.remove_parse_tree(&file_id);
            None
        }
    }
}

/// Seed the parse cache with an already-computed tree, typically from the LSP document path.
pub fn seed_parse_tree(db: &dyn Db, file: SourceFile, tree: &Tree) {
    db.store_parse_tree(
        file.file_id(db).clone(),
        ParseTreeSnapshot {
            content: Arc::from(file.content(db).as_str()),
            language_id: file.language_id(db),
            tree: tree.clone(),
            origin: ParseTreeOrigin::Seeded,
        },
    );
}

/// Extract package declaration from a source file
///
/// Memoized - only recomputes when file content changes.
#[salsa::tracked]
pub fn extract_package(db: &dyn Db, file: SourceFile) -> Option<Arc<str>> {
    let lang_id = file.language_id(db);

    // For Java files, extract package
    if lang_id.as_ref() == "java" {
        let content = file.content(db);
        crate::language::java::class_parser::extract_package_from_source(content)
    } else if lang_id.as_ref() == "kotlin" {
        super::kotlin::extract_kotlin_package(db, file)
    } else {
        None
    }
}

/// Extract import declarations from a source file
///
/// Memoized - only recomputes when file content changes.
#[salsa::tracked]
pub fn extract_imports(db: &dyn Db, file: SourceFile) -> Arc<Vec<Arc<str>>> {
    let lang_id = file.language_id(db);

    let imports = if lang_id.as_ref() == "java" {
        let content = file.content(db);
        crate::language::java::class_parser::extract_imports_from_source(content)
    } else if lang_id.as_ref() == "kotlin" {
        super::kotlin::extract_kotlin_imports(db, file)
    } else {
        vec![]
    };

    Arc::new(imports)
}

/// Helper: Parse a tree for a given language (not cached - used by other queries)
///
/// This is NOT a Salsa query because Tree doesn't implement the required traits.
/// Instead, we parse on-demand when needed by Salsa queries.
pub fn parse_tree_for_language(content: &str, language_id: &str) -> Option<Tree> {
    let mut parser = parser_for_language(language_id)?;
    parser.parse(content, None)
}

fn parse_tree_from_snapshot(
    content: &str,
    language_id: &str,
    cached: Option<&ParseTreeSnapshot>,
) -> Option<(Tree, ParseTreeOrigin)> {
    let mut parser = parser_for_language(language_id)?;

    if let Some(snapshot) = cached
        && snapshot.language_id.as_ref() == language_id
    {
        let mut old_tree = snapshot.tree.clone();
        if snapshot.content.as_ref() != content {
            old_tree.edit(&compute_input_edit(snapshot.content.as_ref(), content));
        }

        if let Some(tree) = parser.parse(content, Some(&old_tree)) {
            return Some((tree, ParseTreeOrigin::Incremental));
        }
    }

    parser
        .parse(content, None)
        .map(|tree| (tree, ParseTreeOrigin::Full))
}

fn parser_for_language(language_id: &str) -> Option<Parser> {
    let mut parser = Parser::new();

    match language_id {
        "java" => {
            parser
                .set_language(&tree_sitter_java::LANGUAGE.into())
                .ok()?;
        }
        "kotlin" => {
            parser
                .set_language(&tree_sitter_kotlin::LANGUAGE.into())
                .ok()?;
        }
        _ => return None,
    }

    Some(parser)
}

fn compute_input_edit(old_content: &str, new_content: &str) -> InputEdit {
    let prefix = common_prefix_len(old_content, new_content);
    let old_suffix = common_suffix_len(&old_content[prefix..], &new_content[prefix..]);
    let old_end_byte = old_content.len() - old_suffix;
    let new_end_byte = new_content.len() - old_suffix;

    InputEdit {
        start_byte: prefix,
        old_end_byte,
        new_end_byte,
        start_position: point_for_offset(old_content, prefix),
        old_end_position: point_for_offset(old_content, old_end_byte),
        new_end_position: point_for_offset(new_content, new_end_byte),
    }
}

fn common_prefix_len(left: &str, right: &str) -> usize {
    left.chars()
        .zip(right.chars())
        .take_while(|(a, b)| a == b)
        .map(|(ch, _)| ch.len_utf8())
        .sum()
}

fn common_suffix_len(left: &str, right: &str) -> usize {
    left.chars()
        .rev()
        .zip(right.chars().rev())
        .take_while(|(a, b)| a == b)
        .map(|(ch, _)| ch.len_utf8())
        .sum()
}

fn point_for_offset(source: &str, offset: usize) -> Point {
    let mut row = 0usize;
    let mut column = 0usize;
    for byte in source.as_bytes().iter().take(offset) {
        if *byte == b'\n' {
            row += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    Point::new(row, column)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_parse_file_memoization() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "public class Test {}".to_string(),
            Arc::from("java"),
        );

        // First parse
        let result1 = parse_file(&db, file);
        assert_eq!(result1.root_kind.as_ref(), "program");
        assert!(!result1.has_error);

        // Second parse - should return same result (memoized)
        let result2 = parse_file(&db, file);
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_parse_file_invalidation() {
        use salsa::Setter;

        let mut db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "public class Test {}".to_string(),
            Arc::from("java"),
        );

        let result1 = parse_file(&db, file);
        let hash1 = result1.content_hash;

        // Modify content
        file.set_content(&mut db)
            .to("public class Modified {}".to_string());

        // Should recompute
        let result2 = parse_file(&db, file);
        let hash2 = result2.content_hash;

        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_parse_tree_reuses_unchanged_method_subtree() {
        use salsa::Setter;

        let mut db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let original = r#"
public class Test {
    void stable() {
        int x = 1;
    }

    void edited() {
        int y = 2;
    }
}
"#;
        let updated = r#"
public class Test {
    void stable() {
        int x = 1;
    }

    void edited() {
        int y = 2;
        int z = 3;
    }
}
"#;
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            original.to_string(),
            Arc::from("java"),
        );

        let _tree1 = parse_tree(&db, file).unwrap();
        let file_id = file.file_id(&db);
        let snapshot1 = db.cached_parse_tree(&file_id).unwrap();
        assert_eq!(snapshot1.origin, ParseTreeOrigin::Full);

        file.set_content(&mut db).to(updated.to_string());

        let tree2 = parse_tree(&db, file).unwrap();
        let snapshot2 = db.cached_parse_tree(&file_id).unwrap();

        assert_eq!(snapshot2.origin, ParseTreeOrigin::Incremental);
        assert!(
            tree2.root_node().descendant_count() > snapshot1.tree.root_node().descendant_count()
        );
    }

    #[test]
    fn test_extract_package() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package com.example;\npublic class Test {}".to_string(),
            Arc::from("java"),
        );

        let package = extract_package(&db, file);
        assert_eq!(package.as_deref(), Some("com/example"));
    }

    #[test]
    fn test_extract_imports() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "import java.util.List;\nimport java.util.Map;\npublic class Test {}".to_string(),
            Arc::from("java"),
        );

        let imports = extract_imports(&db, file);
        assert_eq!(imports.len(), 2);
        assert!(imports.iter().any(|i| i.as_ref() == "java.util.List"));
        assert!(imports.iter().any(|i| i.as_ref() == "java.util.Map"));
    }

    #[test]
    fn test_extract_kotlin_package() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.kt").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package org.example.test\nclass Test".to_string(),
            Arc::from("kotlin"),
        );

        let package = extract_package(&db, file);
        assert_eq!(package.as_deref(), Some("org/example/test"));
    }

    #[test]
    fn test_extract_kotlin_imports() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.kt").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "import kotlin.collections.List\nimport org.example.Foo\nclass Test".to_string(),
            Arc::from("kotlin"),
        );

        let imports = extract_imports(&db, file);
        assert_eq!(imports.len(), 2);
        assert!(
            imports
                .iter()
                .any(|i| i.as_ref() == "kotlin.collections.List")
        );
        assert!(imports.iter().any(|i| i.as_ref() == "org.example.Foo"));
    }
}
