/// Salsa queries for inlay hints computation
///
/// This module provides incremental, cached inlay hint generation.
use super::Db;
use crate::index::{IndexScope, IndexView, ModuleId};
use crate::language::java::editor_semantics::semantic_context_at_offset;
use crate::language::java::expression_typing;
use crate::language::java::type_ctx::SourceTypeCtx;
use crate::salsa_db::SourceFile;
use crate::semantic::types::{TypeResolver, parse_single_type_to_internal};
use ropey::Rope;
use std::sync::Arc;

// ============================================================================
// Salsa-Compatible Data Structures
// ============================================================================

/// Inlay hint data (Salsa-compatible)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InlayHintData {
    pub offset: usize,
    pub label: Arc<str>,
    pub kind: InlayHintKindData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InlayHintKindData {
    Type,
    Parameter,
}

/// Variable declaration metadata for type hints
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarDeclMetadata {
    pub offset: usize,
    pub name: Arc<str>,
    pub has_explicit_type: bool,
}

/// Method call metadata for parameter hints
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodCallMetadata {
    pub offset: usize,
    pub method_name: Arc<str>,
    pub receiver_type: Option<Arc<str>>,
    pub arg_count: usize,
}

// ============================================================================
// Core Salsa Queries
// ============================================================================

/// Compute inlay hints for a range in a file (CACHED)
///
/// This is the main entry point for inlay hints. It's memoized by
/// (file, range) so repeated requests for the same range are instant.
#[salsa::tracked]
pub fn compute_inlay_hints(
    db: &dyn Db,
    file: SourceFile,
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
) -> Arc<Vec<InlayHintData>> {
    let language_id = file.language_id(db);

    match language_id.as_ref() {
        "java" => super::java::compute_java_inlay_hints(
            db, file, start_line, start_char, end_line, end_char,
        ),
        "kotlin" => super::kotlin::compute_kotlin_inlay_hints(
            db, file, start_line, start_char, end_line, end_char,
        ),
        _ => Arc::new(Vec::new()),
    }
}

/// Find variable declarations in a range (CACHED)
///
/// This is cached per (file, range) so it's reused across hint computations.
#[salsa::tracked]
pub fn find_variable_declarations_in_range(
    db: &dyn Db,
    file: SourceFile,
    start_offset: usize,
    end_offset: usize,
) -> Arc<Vec<VarDeclMetadata>> {
    let content = file.content(db);

    // Parse tree
    let Some(tree) = super::parse::parse_tree(db, file) else {
        return Arc::new(Vec::new());
    };

    let root = tree.root_node();
    let mut declarations = Vec::new();

    collect_var_decls(
        root,
        content.as_bytes(),
        start_offset,
        end_offset,
        &mut declarations,
    );

    Arc::new(declarations)
}

/// Find method calls in a range (CACHED)
///
/// This is cached per (file, range) so it's reused across hint computations.
#[salsa::tracked]
pub fn find_method_calls_in_range(
    db: &dyn Db,
    file: SourceFile,
    start_offset: usize,
    end_offset: usize,
) -> Arc<Vec<MethodCallMetadata>> {
    let content = file.content(db);
    let language_id = file.language_id(db);

    // Parse tree
    let Some(tree) = super::parse::parse_tree(db, file) else {
        return Arc::new(Vec::new());
    };

    let root = tree.root_node();
    let mut calls = Vec::new();
    match language_id.as_ref() {
        "java" => {
            let view = root_index_view(db);
            let rope = Rope::from_str(content);
            collect_java_method_calls(
                root,
                root,
                content,
                &rope,
                &view,
                start_offset,
                end_offset,
                &mut calls,
            );
        }
        _ => collect_method_calls(
            root,
            content.as_bytes(),
            start_offset,
            end_offset,
            &mut calls,
        ),
    }

    Arc::new(calls)
}

/// Infer the type of a variable at a declaration (CACHED)
///
/// This is cached per (file, offset) so type inference is only done once.
#[salsa::tracked]
pub fn infer_variable_type(db: &dyn Db, file: SourceFile, decl_offset: usize) -> Option<Arc<str>> {
    let language_id = file.language_id(db);

    match language_id.as_ref() {
        "java" => super::java::infer_java_variable_type(db, file, decl_offset),
        "kotlin" => super::kotlin::infer_kotlin_variable_type(db, file, decl_offset),
        _ => None,
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn collect_var_decls(
    node: tree_sitter::Node,
    source: &[u8],
    start: usize,
    end: usize,
    declarations: &mut Vec<VarDeclMetadata>,
) {
    // Skip nodes outside range
    if node.end_byte() < start || node.start_byte() > end {
        return;
    }

    // Check if this is a variable declaration
    if node.kind() == "local_variable_declaration" {
        // Check if type is explicit or uses 'var'
        let has_explicit_type = !has_var_keyword(node, source);

        // Find all declarators
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "variable_declarator"
                && let Some(name_node) = child.child_by_field_name("name")
                && let Ok(name) = name_node.utf8_text(source)
            {
                declarations.push(VarDeclMetadata {
                    offset: name_node.start_byte(),
                    name: Arc::from(name),
                    has_explicit_type,
                });
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_var_decls(child, source, start, end, declarations);
    }
}

fn has_var_keyword(node: tree_sitter::Node, _source: &[u8]) -> bool {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "var" {
            return true;
        }
    }
    false
}

fn collect_method_calls(
    node: tree_sitter::Node,
    source: &[u8],
    start: usize,
    end: usize,
    calls: &mut Vec<MethodCallMetadata>,
) {
    // Skip nodes outside range
    if node.end_byte() < start || node.start_byte() > end {
        return;
    }

    // Check if this is a method invocation
    if node.kind() == "method_invocation"
        && let Some(name_node) = node.child_by_field_name("name")
        && let Ok(method_name) = name_node.utf8_text(source)
    {
        // Count arguments
        let arg_count = if let Some(args_node) = node.child_by_field_name("arguments") {
            count_arguments(args_node)
        } else {
            0
        };

        let receiver_type = node
            .child_by_field_name("object")
            .and_then(|receiver| receiver.utf8_text(source).ok())
            .and_then(infer_receiver_type_from_text);

        calls.push(MethodCallMetadata {
            offset: name_node.start_byte(),
            method_name: Arc::from(method_name),
            receiver_type,
            arg_count,
        });
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_method_calls(child, source, start, end, calls);
    }
}

fn count_arguments(args_node: tree_sitter::Node) -> usize {
    let mut cursor = args_node.walk();
    args_node
        .named_children(&mut cursor)
        .filter(|n| !matches!(n.kind(), "(" | ")" | ","))
        .count()
}

fn collect_java_method_calls(
    root: tree_sitter::Node,
    node: tree_sitter::Node,
    source: &str,
    rope: &Rope,
    view: &IndexView,
    start: usize,
    end: usize,
    calls: &mut Vec<MethodCallMetadata>,
) {
    if node.end_byte() < start || node.start_byte() > end {
        return;
    }

    if node.kind() == "method_invocation"
        && let Some(name_node) = node.child_by_field_name("name")
        && let Ok(method_name) = name_node.utf8_text(source.as_bytes())
    {
        let arg_count = node
            .child_by_field_name("arguments")
            .map(count_arguments)
            .unwrap_or(0);
        let receiver_expr = node
            .child_by_field_name("object")
            .and_then(|receiver| receiver.utf8_text(source.as_bytes()).ok());

        calls.push(MethodCallMetadata {
            offset: name_node.start_byte(),
            method_name: Arc::from(method_name),
            receiver_type: infer_java_receiver_type_for_call(
                source,
                rope,
                root,
                view,
                name_node.start_byte(),
                receiver_expr,
            ),
            arg_count,
        });
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_java_method_calls(root, child, source, rope, view, start, end, calls);
    }
}

fn infer_receiver_type_from_text(receiver: &str) -> Option<Arc<str>> {
    let trimmed = receiver.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.chars().next().is_some_and(|ch| ch.is_uppercase()) {
        return Some(Arc::from(trimmed.replace('.', "/")));
    }
    None
}

fn infer_java_receiver_type_for_call(
    source: &str,
    rope: &Rope,
    root: tree_sitter::Node,
    view: &IndexView,
    offset: usize,
    receiver_expr: Option<&str>,
) -> Option<Arc<str>> {
    let ctx = semantic_context_at_offset(source, rope, root, offset, view)?;

    if let Some(owner) = ctx.location.member_access_receiver_owner_internal() {
        return Some(Arc::from(owner));
    }

    let receiver_expr = match receiver_expr.map(str::trim) {
        Some(expr) if !expr.is_empty() => expr,
        _ => return ctx.enclosing_internal_name.clone(),
    };

    if receiver_expr == "this" {
        return ctx.enclosing_internal_name.clone();
    }

    if let Some(local) = ctx
        .local_variables
        .iter()
        .find(|local| local.name.as_ref() == receiver_expr)
    {
        return Some(Arc::from(local.type_internal.erased_internal()));
    }

    if let Some(enclosing) = ctx.enclosing_internal_name.as_ref()
        && let Some(field) = view.lookup_field_in_hierarchy(enclosing, receiver_expr)
        && let Some(field_ty) = parse_single_type_to_internal(&field.descriptor)
    {
        return Some(Arc::from(field_ty.erased_internal()));
    }

    if let Some(type_ctx) = ctx.extension::<SourceTypeCtx>() {
        let resolver = TypeResolver::new(view);
        if let Some(receiver_ty) = expression_typing::resolve_expression_type(
            receiver_expr,
            &ctx.local_variables,
            ctx.enclosing_internal_name.as_ref(),
            &resolver,
            type_ctx,
            view,
        ) {
            return Some(Arc::from(receiver_ty.erased_internal()));
        }
    }

    infer_receiver_type_from_text(receiver_expr)
}

fn root_index_view(db: &dyn Db) -> IndexView {
    let workspace_index = db.workspace_index();
    let index = workspace_index.read();
    index.view(IndexScope {
        module: ModuleId::ROOT,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_compute_inlay_hints_caching() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "public class Test { void method() { var x = 5; } }".to_string(),
            Arc::from("java"),
        );

        // First computation
        let result1 = compute_inlay_hints(&db, file, 0, 0, 0, 60);

        // Second computation - should be cached
        let result2 = compute_inlay_hints(&db, file, 0, 0, 0, 60);

        // Results should be identical (same Arc)
        assert!(Arc::ptr_eq(&result1, &result2));
    }
}
