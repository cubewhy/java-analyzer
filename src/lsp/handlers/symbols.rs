use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tree_sitter::Node;

use crate::lsp::converters::ts_node_to_range;
use crate::workspace::Workspace;

pub async fn handle_document_symbol(
    workspace: Arc<Workspace>,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = params.text_document.uri;
    let doc = workspace.documents.get(&uri)?;

    // TODO: handle kotlin
    if doc.language_id != "java" {
        return None;
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_java::LANGUAGE.into())
        .ok()?;
    let tree = parser.parse(doc.content.as_ref(), None)?;
    let root = tree.root_node();

    let mut symbols = Vec::new();
    collect_java_symbols(root, doc.content.as_bytes(), &mut symbols);

    Some(DocumentSymbolResponse::Nested(symbols))
}

fn collect_java_symbols(node: Node, bytes: &[u8], out: &mut Vec<DocumentSymbol>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "class_declaration"
            | "interface_declaration"
            | "enum_declaration"
            | "record_declaration" => {
                if let Some(symbol) = parse_type_symbol(child, bytes) {
                    out.push(symbol);
                }
            }
            "method_declaration" | "constructor_declaration" => {
                if let Some(symbol) = parse_method_symbol(child, bytes) {
                    out.push(symbol);
                }
            }
            "field_declaration" => {
                // Java 一个 field_declaration 可能包含多个变量: int a, b;
                out.extend(parse_field_symbols(child, bytes));
            }
            // 递归处理嵌套类或 Body
            "class_body" | "interface_body" | "enum_body" | "program" | "ERROR" => {
                collect_java_symbols(child, bytes, out);
            }
            _ => {}
        }
    }
}

fn parse_type_symbol(node: Node, bytes: &[u8]) -> Option<DocumentSymbol> {
    let name_node = node.child_by_field_name("name")?;
    let name = name_node.utf8_text(bytes).ok()?.to_string();
    let kind = match node.kind() {
        "interface_declaration" => SymbolKind::INTERFACE,
        "enum_declaration" => SymbolKind::ENUM,
        _ => SymbolKind::CLASS,
    };

    let range = ts_node_to_range(&node);
    let selection_range = ts_node_to_range(&name_node);

    let mut children = Vec::new();
    if let Some(body) = node.child_by_field_name("body") {
        collect_java_symbols(body, bytes, &mut children);
    }

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: Some(children),
    })
}

fn parse_method_symbol(node: Node, bytes: &[u8]) -> Option<DocumentSymbol> {
    let name_node = node
        .child_by_field_name("name")
        .or_else(|| node.child_by_field_name("identifier"))?; // constructor 用的是 identifier
    let name = name_node.utf8_text(bytes).ok()?.to_string();

    let kind = if node.kind() == "constructor_declaration" {
        SymbolKind::CONSTRUCTOR
    } else {
        SymbolKind::METHOD
    };

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range: ts_node_to_range(&node),
        selection_range: ts_node_to_range(&name_node),
        children: None,
    })
}

fn parse_field_symbols(node: Node, bytes: &[u8]) -> Vec<DocumentSymbol> {
    let mut results = Vec::new();
    let mut cursor = node.walk();

    // 找到类型，用于 detail 展示
    let type_text = node
        .children(&mut cursor)
        .find(|c| c.kind().ends_with("_type") || c.kind() == "type_identifier")
        .and_then(|c| c.utf8_text(bytes).ok())
        .map(|t| t.to_string());

    let mut cursor = node.walk();
    for declarator in node
        .children(&mut cursor)
        .filter(|c| c.kind() == "variable_declarator")
    {
        if let Some(name_node) = declarator.child_by_field_name("name") {
            if let Ok(name) = name_node.utf8_text(bytes) {
                #[allow(deprecated)]
                results.push(DocumentSymbol {
                    name: name.to_string(),
                    detail: type_text.clone(),
                    kind: SymbolKind::FIELD,
                    tags: None,
                    deprecated: None,
                    range: ts_node_to_range(&node), // 整个 field_declaration 范围
                    selection_range: ts_node_to_range(&name_node),
                    children: None,
                });
            }
        }
    }
    results
}
