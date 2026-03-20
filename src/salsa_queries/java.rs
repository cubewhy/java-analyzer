use super::Db;
use crate::index::{ClassMetadata, NameTable};
use crate::salsa_db::SourceFile;
use crate::salsa_queries::context::{
    CompletionContextData, CursorLocationData, line_col_to_offset,
};
use crate::salsa_queries::hints::{InlayHintData, InlayHintKindData};
use crate::salsa_queries::symbols::{ResolvedSymbolData, SymbolKind};
/// Java-specific Salsa queries
///
/// These queries handle Java-specific parsing and analysis.
use std::sync::Arc;

/// Parse Java source and extract class metadata with full incremental support
///
/// This is the main entry point for Java file indexing.
/// Note: We return the classes directly, not wrapped in Arc, because Salsa
/// will handle the memoization.
pub fn parse_java_classes(db: &dyn Db, file: SourceFile) -> Vec<ClassMetadata> {
    let content = file.content(db);
    let file_id = file.file_id(db);

    // Get name table if available
    let name_table = get_name_table_for_java_file(db, file);

    let origin = crate::index::ClassOrigin::SourceFile(Arc::from(file_id.as_str()));

    crate::language::java::class_parser::parse_java_source(content, origin, name_table)
}

/// Get name table for a Java file's context
fn get_name_table_for_java_file(_db: &dyn Db, _file: SourceFile) -> Option<Arc<NameTable>> {
    // TODO: Query the workspace model to determine the file's context
    // For now, return None and let the parser work without name table
    None
}

/// Extract Java package declaration
pub fn extract_java_package(db: &dyn Db, file: SourceFile) -> Option<Arc<str>> {
    let content = file.content(db);
    crate::language::java::class_parser::extract_package_from_source(content)
}

/// Extract Java imports
pub fn extract_java_imports(db: &dyn Db, file: SourceFile) -> Vec<Arc<str>> {
    let content = file.content(db);
    crate::language::java::class_parser::extract_imports_from_source(content)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_parse_java_classes() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package com.example;\npublic class Test { void foo() {} }".to_string(),
            Arc::from("java"),
        );

        let classes = parse_java_classes(&db, file);
        assert_eq!(classes.len(), 1);
        assert_eq!(classes[0].name.as_ref(), "Test");
        assert_eq!(classes[0].package.as_deref(), Some("com/example"));
        assert_eq!(classes[0].methods.len(), 1);
        assert_eq!(classes[0].methods[0].name.as_ref(), "foo");
    }

    #[test]
    fn test_extract_java_package() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package org.example.test;\npublic class Test {}".to_string(),
            Arc::from("java"),
        );

        let package = extract_java_package(&db, file);
        assert_eq!(package.as_deref(), Some("org/example/test"));
    }

    #[test]
    fn test_extract_java_imports() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "import java.util.*;\nimport java.io.File;\npublic class Test {}".to_string(),
            Arc::from("java"),
        );

        let imports = extract_java_imports(&db, file);
        assert_eq!(imports.len(), 2);
    }
}

// ============================================================================
// Java Completion Context Extraction
// ============================================================================

/// Extract Java completion context (CACHED)
#[salsa::tracked]
pub fn extract_java_completion_context(
    db: &dyn Db,
    file: SourceFile,
    line: u32,
    character: u32,
    trigger_char: Option<char>,
) -> Arc<CompletionContextData> {
    let content = file.content(db);
    let Some(offset) = line_col_to_offset(content, line, character) else {
        return Arc::new(empty_context(db, file));
    };

    // Check if in comment
    if is_in_comment(content, offset) {
        return Arc::new(empty_context(db, file));
    }

    // Parse tree
    let Some(tree) = super::parse::parse_tree_for_language(content, "java") else {
        return Arc::new(empty_context(db, file));
    };

    let root = tree.root_node();

    // Find cursor node
    let cursor_node = root.named_descendant_for_byte_range(offset.saturating_sub(1), offset);

    // Determine location
    let location = determine_java_cursor_location(content, cursor_node, offset, trigger_char);

    // Extract query string
    let query = extract_query_string(content, offset, &location);

    // Extract scope information (all cached separately)
    let package = super::parse::extract_package(db, file);
    let imports = super::parse::extract_imports(db, file);
    let enclosing_class = find_java_enclosing_class_name(db, file, offset);
    let enclosing_internal_name = build_internal_name(&package, &enclosing_class);

    // Count locals (cached)
    let local_var_count = count_java_locals_in_scope(db, file, offset);

    // Compute content hash for the relevant scope
    let content_hash = super::context::compute_scope_content_hash(db, file, offset);

    Arc::new(CompletionContextData {
        location,
        query,
        enclosing_class,
        enclosing_internal_name,
        enclosing_package: package,
        local_var_count,
        import_count: imports.len(),
        static_import_count: 0, // TODO: count static imports
        content_hash,
        file_uri: Arc::from(file.file_id(db).as_str()),
        language_id: Arc::from("java"),
    })
}

fn determine_java_cursor_location(
    content: &str,
    cursor_node: Option<tree_sitter::Node>,
    offset: usize,
    trigger_char: Option<char>,
) -> CursorLocationData {
    let source = content.as_bytes();

    let Some(node) = cursor_node else {
        return fallback_location(content, offset, trigger_char);
    };

    // Check for member access (dot trigger)
    if trigger_char == Some('.') {
        return handle_member_access(content, offset);
    }

    // Walk up the tree to find semantic context
    let mut current = node;
    loop {
        match current.kind() {
            "import_declaration" => {
                return handle_import(current, source);
            }
            "field_access" | "method_invocation" => {
                return handle_field_or_method_access(current, source);
            }
            "identifier" => {
                // Check parent context
                if let Some(parent) = current.parent() {
                    match parent.kind() {
                        "import_declaration" => {
                            return handle_import(parent, source);
                        }
                        "field_access" | "method_invocation" => {
                            return handle_field_or_method_access(parent, source);
                        }
                        _ => {}
                    }
                }

                // Default to expression
                let text = current.utf8_text(source).unwrap_or("");
                return CursorLocationData::Expression {
                    prefix: Arc::from(text),
                };
            }
            _ => {}
        }

        // Move to parent
        match current.parent() {
            Some(p) => current = p,
            None => break,
        }
    }

    fallback_location(content, offset, trigger_char)
}

fn handle_import(node: tree_sitter::Node, source: &[u8]) -> CursorLocationData {
    let text = node.utf8_text(source).unwrap_or("");
    let prefix = text
        .trim_start_matches("import")
        .trim_start_matches("static")
        .trim()
        .trim_end_matches(';')
        .to_string();

    if text.contains("static") {
        CursorLocationData::ImportStatic {
            prefix: Arc::from(prefix),
        }
    } else {
        CursorLocationData::Import {
            prefix: Arc::from(prefix),
        }
    }
}

fn handle_field_or_method_access(node: tree_sitter::Node, source: &[u8]) -> CursorLocationData {
    // Get the receiver (object before the dot)
    let receiver = if let Some(obj) = node.child_by_field_name("object") {
        obj.utf8_text(source).unwrap_or("").to_string()
    } else {
        String::new()
    };

    // Get the member name (after the dot)
    let member = if let Some(field) = node.child_by_field_name("field") {
        field.utf8_text(source).unwrap_or("").to_string()
    } else if let Some(name) = node.child_by_field_name("name") {
        name.utf8_text(source).unwrap_or("").to_string()
    } else {
        String::new()
    };

    // Check if receiver looks like a class name (starts with uppercase)
    if receiver.chars().next().is_some_and(|c| c.is_uppercase()) {
        CursorLocationData::StaticAccess {
            class_internal_name: Arc::from(receiver.replace('.', "/")),
            member_prefix: Arc::from(member),
        }
    } else {
        CursorLocationData::MemberAccess {
            receiver_expr: Arc::from(receiver),
            member_prefix: Arc::from(member),
            receiver_type_hint: None,
            arguments: None,
        }
    }
}

fn handle_member_access(content: &str, offset: usize) -> CursorLocationData {
    // Find the line containing the cursor
    let line_start = content[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line = &content[line_start..offset];

    // Find the last token before the dot
    let receiver = line
        .trim_end_matches('.')
        .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .next_back()
        .unwrap_or("")
        .to_string();

    if receiver.chars().next().is_some_and(|c| c.is_uppercase()) {
        CursorLocationData::StaticAccess {
            class_internal_name: Arc::from(receiver.replace('.', "/")),
            member_prefix: Arc::from(""),
        }
    } else {
        CursorLocationData::MemberAccess {
            receiver_expr: Arc::from(receiver),
            member_prefix: Arc::from(""),
            receiver_type_hint: None,
            arguments: None,
        }
    }
}

fn fallback_location(
    content: &str,
    offset: usize,
    _trigger_char: Option<char>,
) -> CursorLocationData {
    // Extract the word at cursor
    let before = &content[..offset];
    let after = &content[offset..];

    let word_start = before
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let word_end = after
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(after.len());

    let word = format!("{}{}", &before[word_start..], &after[..word_end]);

    CursorLocationData::Expression {
        prefix: Arc::from(word),
    }
}

fn extract_query_string(_content: &str, _offset: usize, location: &CursorLocationData) -> Arc<str> {
    match location {
        CursorLocationData::Expression { prefix } => Arc::clone(prefix),
        CursorLocationData::MemberAccess { member_prefix, .. } => Arc::clone(member_prefix),
        CursorLocationData::StaticAccess { member_prefix, .. } => Arc::clone(member_prefix),
        CursorLocationData::Import { prefix } => Arc::from(prefix.rsplit('.').next().unwrap_or("")),
        CursorLocationData::ImportStatic { prefix } => {
            Arc::from(prefix.rsplit('.').next().unwrap_or(""))
        }
        _ => Arc::from(""),
    }
}

fn is_in_comment(content: &str, offset: usize) -> bool {
    // Simple check: look backwards for comment markers
    let before = &content[..offset];

    // Check for line comment
    if let Some(line_start) = before.rfind('\n') {
        let line = &before[line_start + 1..];
        if line.trim_start().starts_with("//") {
            return true;
        }
    }

    // Check for block comment
    let last_block_start = before.rfind("/*");
    let last_block_end = before.rfind("*/");

    match (last_block_start, last_block_end) {
        (Some(start), Some(end)) => start > end,
        (Some(_), None) => true,
        _ => false,
    }
}

fn empty_context(db: &dyn Db, file: SourceFile) -> CompletionContextData {
    CompletionContextData {
        location: CursorLocationData::Unknown,
        query: Arc::from(""),
        enclosing_class: None,
        enclosing_internal_name: None,
        enclosing_package: None,
        local_var_count: 0,
        import_count: 0,
        static_import_count: 0,
        content_hash: 0,
        file_uri: Arc::from(file.file_id(db).as_str()),
        language_id: Arc::from("java"),
    }
}

// ============================================================================
// Java Scope Queries
// ============================================================================

/// Find the enclosing class name (CACHED)
#[salsa::tracked]
pub fn find_java_enclosing_class_name(
    db: &dyn Db,
    file: SourceFile,
    offset: usize,
) -> Option<Arc<str>> {
    if let Some((name, _, _)) = super::semantic::find_enclosing_class_bounds(db, file, offset) {
        Some(name)
    } else {
        None
    }
}

/// Count local variables in scope (CACHED)
#[salsa::tracked]
pub fn count_java_locals_in_scope(db: &dyn Db, file: SourceFile, offset: usize) -> usize {
    // Get method bounds
    let Some((method_start, method_end)) =
        super::semantic::find_enclosing_method_bounds(db, file, offset)
    else {
        return 0;
    };

    // Get metadata (which includes count)
    let metadata =
        super::semantic::extract_method_locals_metadata(db, file, method_start, method_end);
    metadata.local_count
}

fn build_internal_name(package: &Option<Arc<str>>, class: &Option<Arc<str>>) -> Option<Arc<str>> {
    match (package, class) {
        (Some(pkg), Some(cls)) => Some(Arc::from(format!("{}/{}", pkg, cls))),
        (None, Some(cls)) => Some(Arc::clone(cls)),
        _ => None,
    }
}

// ============================================================================
// Java Symbol Resolution
// ============================================================================

/// Resolve Java symbol at position (CACHED)
#[salsa::tracked]
pub fn resolve_java_symbol(
    db: &dyn Db,
    file: SourceFile,
    line: u32,
    character: u32,
) -> Option<Arc<ResolvedSymbolData>> {
    let content = file.content(db);
    let offset = line_col_to_offset(content, line, character)?;

    // Get context
    let context = extract_java_completion_context(db, file, line, character, None);

    // Resolve based on location
    match &context.location {
        CursorLocationData::Expression { prefix } => {
            resolve_java_expression_symbol(db, file, Arc::clone(prefix), offset)
        }
        CursorLocationData::MemberAccess {
            receiver_expr,
            member_prefix,
            ..
        } => resolve_java_member_symbol(
            db,
            file,
            Arc::clone(receiver_expr),
            Arc::clone(member_prefix),
        ),
        CursorLocationData::StaticAccess {
            class_internal_name,
            member_prefix,
        } => Some(Arc::new(ResolvedSymbolData {
            kind: SymbolKind::Class,
            target_internal_name: Arc::clone(class_internal_name),
            member_name: Some(Arc::clone(member_prefix)),
            descriptor: None,
        })),
        CursorLocationData::Import { prefix } => {
            let internal = prefix.replace('.', "/");
            Some(Arc::new(ResolvedSymbolData {
                kind: SymbolKind::Class,
                target_internal_name: Arc::from(internal),
                member_name: None,
                descriptor: None,
            }))
        }
        _ => None,
    }
}

#[salsa::tracked]
fn resolve_java_expression_symbol(
    db: &dyn Db,
    file: SourceFile,
    symbol_name: Arc<str>,
    offset: usize,
) -> Option<Arc<ResolvedSymbolData>> {
    // Check if it's a local variable
    if is_java_local_variable(db, file, Arc::clone(&symbol_name), offset) {
        return Some(Arc::new(ResolvedSymbolData {
            kind: SymbolKind::LocalVariable,
            target_internal_name: Arc::from(""),
            member_name: Some(symbol_name),
            descriptor: None,
        }));
    }

    // TODO: Check fields, methods, imports
    None
}

#[salsa::tracked]
fn resolve_java_member_symbol(
    _db: &dyn Db,
    _file: SourceFile,
    _receiver_expr: Arc<str>,
    _member_name: Arc<str>,
) -> Option<Arc<ResolvedSymbolData>> {
    // TODO: Implement member resolution
    // This requires type inference for the receiver
    None
}

/// Check if a symbol is a local variable (CACHED)
#[salsa::tracked]
pub fn is_java_local_variable(
    db: &dyn Db,
    file: SourceFile,
    symbol_name: Arc<str>,
    offset: usize,
) -> bool {
    super::symbols::find_local_variable_declaration(db, file, symbol_name, offset).is_some()
}

// ============================================================================
// Java Inlay Hints
// ============================================================================

/// Compute Java inlay hints (CACHED)
#[salsa::tracked]
pub fn compute_java_inlay_hints(
    db: &dyn Db,
    file: SourceFile,
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
) -> Arc<Vec<InlayHintData>> {
    let content = file.content(db);
    let Some(start_offset) = line_col_to_offset(content, start_line, start_char) else {
        return Arc::new(Vec::new());
    };
    let Some(end_offset) = line_col_to_offset(content, end_line, end_char) else {
        return Arc::new(Vec::new());
    };

    let mut hints = Vec::new();

    // Find variable declarations (cached)
    let var_decls =
        super::hints::find_variable_declarations_in_range(db, file, start_offset, end_offset);

    // Add type hints for variables without explicit types
    for decl in var_decls.iter() {
        if !decl.has_explicit_type
            && let Some(inferred_type) = infer_java_variable_type(db, file, decl.offset)
        {
            hints.push(InlayHintData {
                offset: decl.offset + decl.name.len(),
                label: format!(": {}", inferred_type).into(),
                kind: InlayHintKindData::Type,
            });
        }
    }

    // Find method calls (cached)
    let _method_calls =
        super::hints::find_method_calls_in_range(db, file, start_offset, end_offset);

    // TODO: Add parameter hints for method calls

    Arc::new(hints)
}

/// Infer Java variable type (CACHED)
#[salsa::tracked]
pub fn infer_java_variable_type(
    db: &dyn Db,
    file: SourceFile,
    decl_offset: usize,
) -> Option<Arc<str>> {
    let content = file.content(db);

    // Parse tree
    let tree = super::parse::parse_tree_for_language(content, "java")?;
    let root = tree.root_node();

    // Find the variable declarator at this offset
    let node = find_node_at_offset(root, decl_offset)?;

    // Find the variable_declarator ancestor
    let var_decl = find_ancestor_of_kind(node, "variable_declarator")?;

    // Get the initializer
    let init = var_decl.child_by_field_name("value")?;

    // Infer type from initializer
    infer_type_from_expression(init, content.as_bytes())
}

fn find_node_at_offset<'a>(
    root: tree_sitter::Node<'a>,
    offset: usize,
) -> Option<tree_sitter::Node<'a>> {
    root.named_descendant_for_byte_range(offset, offset + 1)
}

fn find_ancestor_of_kind<'a>(
    mut node: tree_sitter::Node<'a>,
    kind: &str,
) -> Option<tree_sitter::Node<'a>> {
    loop {
        if node.kind() == kind {
            return Some(node);
        }
        node = node.parent()?;
    }
}

fn infer_type_from_expression(expr: tree_sitter::Node, source: &[u8]) -> Option<Arc<str>> {
    match expr.kind() {
        "decimal_integer_literal"
        | "hex_integer_literal"
        | "octal_integer_literal"
        | "binary_integer_literal" => Some(Arc::from("int")),
        "decimal_floating_point_literal" | "hex_floating_point_literal" => {
            Some(Arc::from("double"))
        }
        "string_literal" => Some(Arc::from("String")),
        "true" | "false" => Some(Arc::from("boolean")),
        "null_literal" => Some(Arc::from("Object")),
        "object_creation_expression" => {
            // Extract the type from "new Type()"
            if let Some(type_node) = expr.child_by_field_name("type") {
                type_node.utf8_text(source).ok().map(Arc::from)
            } else {
                None
            }
        }
        _ => None,
    }
}
