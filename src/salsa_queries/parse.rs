use super::Db;
use crate::salsa_db::SourceFile;
/// Parse queries - handle syntax tree parsing and basic extraction
///
/// These queries are the foundation of incremental parsing. When a file's
/// content changes, only these queries (and their dependents) are invalidated.
use std::sync::Arc;
use tree_sitter::{Parser, Tree};

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
    let lang_id = file.language_id(db);

    // Get the language implementation
    let registry = crate::language::LanguageRegistry::new();
    let lang = registry.find(lang_id.as_ref());

    let (root_kind, has_error, node_count) = if let Some(lang) = lang {
        let tree = lang.parse_tree(content, None);
        if let Some(tree) = tree {
            let root = tree.root_node();
            (
                Arc::from(root.kind()),
                root.has_error(),
                root.descendant_count(),
            )
        } else {
            (Arc::from("error"), true, 0)
        }
    } else {
        (Arc::from("unknown"), true, 0)
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

/// Extract package declaration from a source file
///
/// Memoized - only recomputes when file content changes.
#[salsa::tracked]
pub fn extract_package(db: &dyn Db, file: SourceFile) -> Option<Arc<str>> {
    let content = file.content(db);
    let lang_id = file.language_id(db);

    // For Java files, extract package
    if lang_id.as_ref() == "java" {
        crate::language::java::class_parser::extract_package_from_source(content)
    } else if lang_id.as_ref() == "kotlin" {
        // TODO: Implement Kotlin package extraction
        None
    } else {
        None
    }
}

/// Extract import declarations from a source file
///
/// Memoized - only recomputes when file content changes.
#[salsa::tracked]
pub fn extract_imports(db: &dyn Db, file: SourceFile) -> Arc<Vec<Arc<str>>> {
    let content = file.content(db);
    let lang_id = file.language_id(db);

    let imports = if lang_id.as_ref() == "java" {
        crate::language::java::class_parser::extract_imports_from_source(content)
    } else if lang_id.as_ref() == "kotlin" {
        // TODO: Implement Kotlin import extraction
        vec![]
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

    parser.parse(content, None)
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
}
