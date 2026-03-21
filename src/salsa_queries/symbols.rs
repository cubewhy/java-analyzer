/// Salsa queries for symbol resolution and goto definition
///
/// This module provides incremental, cached symbol resolution for navigation features.
use super::Db;
use crate::salsa_db::SourceFile;
use std::sync::Arc;

// ============================================================================
// Salsa-Compatible Data Structures
// ============================================================================

/// Resolved symbol data (Salsa-compatible)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedSymbolData {
    pub kind: SymbolKind,
    pub target_internal_name: Arc<str>,
    pub member_name: Option<Arc<str>>,
    pub descriptor: Option<Arc<str>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Class,
    Method,
    Field,
    LocalVariable,
    Parameter,
    Unknown,
}

// ============================================================================
// Core Salsa Queries
// ============================================================================

/// Resolve the symbol at a specific position (CACHED)
///
/// This is the main entry point for goto definition. It's memoized by
/// (file, line, character) so repeated requests are instant.
#[salsa::tracked]
pub fn resolve_symbol_at_position(
    db: &dyn Db,
    file: SourceFile,
    line: u32,
    character: u32,
) -> Option<Arc<ResolvedSymbolData>> {
    let language_id = file.language_id(db);

    match language_id.as_ref() {
        "java" => super::java::resolve_java_symbol(db, file, line, character),
        "kotlin" => super::kotlin::resolve_kotlin_symbol(db, file, line, character),
        _ => None,
    }
}

/// Check if a symbol name is a local variable in the current scope (CACHED)
#[salsa::tracked]
pub fn is_local_variable(
    db: &dyn Db,
    file: SourceFile,
    symbol_name: Arc<str>,
    offset: usize,
) -> bool {
    let language_id = file.language_id(db);

    match language_id.as_ref() {
        "java" => super::java::is_java_local_variable(db, file, symbol_name, offset),
        "kotlin" => super::kotlin::is_kotlin_local_variable(db, file, symbol_name, offset),
        _ => false,
    }
}

/// Find the declaration offset of a local variable (CACHED)
#[salsa::tracked]
pub fn find_local_variable_declaration(
    db: &dyn Db,
    file: SourceFile,
    var_name: Arc<str>,
    search_offset: usize,
) -> Option<usize> {
    let content = file.content(db);

    // Parse tree
    let tree = super::parse::parse_tree(db, file)?;
    let root = tree.root_node();

    // Find all variable declarations before search_offset
    find_var_decl_offset(root, content.as_bytes(), var_name.as_ref(), search_offset)
}

/// Helper: Find variable declaration offset in AST
fn find_var_decl_offset(
    root: tree_sitter::Node,
    source: &[u8],
    var_name: &str,
    before_offset: usize,
) -> Option<usize> {
    let mut cursor = root.walk();
    find_var_decl_recursive(&mut cursor, source, var_name, before_offset)
}

fn find_var_decl_recursive(
    cursor: &mut tree_sitter::TreeCursor,
    source: &[u8],
    var_name: &str,
    before_offset: usize,
) -> Option<usize> {
    let node = cursor.node();

    // Skip nodes after our search point
    if node.start_byte() >= before_offset {
        return None;
    }

    // Check if this is a variable declaration
    match node.kind() {
        "variable_declarator" | "parameter" | "catch_formal_parameter" => {
            // Find the identifier child
            let mut child_cursor = node.walk();
            for child in node.children(&mut child_cursor) {
                if (child.kind() == "identifier" || child.kind() == "simple_identifier")
                    && let Ok(text) = child.utf8_text(source)
                    && text == var_name
                {
                    return Some(child.start_byte());
                }
            }
        }
        _ => {}
    }

    // Recurse into children
    if cursor.goto_first_child() {
        loop {
            if let Some(offset) = find_var_decl_recursive(cursor, source, var_name, before_offset) {
                cursor.goto_parent();
                return Some(offset);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        cursor.goto_parent();
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_resolve_symbol_caching() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "public class Test { void method() { int x = 5; } }".to_string(),
            Arc::from("java"),
        );

        // First resolution
        let result1 = resolve_symbol_at_position(&db, file, 0, 30);

        // Second resolution - should be cached
        let result2 = resolve_symbol_at_position(&db, file, 0, 30);

        // Results should be identical (same Arc)
        assert_eq!(result1, result2);
    }
}
