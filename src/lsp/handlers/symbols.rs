use std::sync::Arc;
use tower_lsp::lsp_types::*;

use crate::language::LanguageRegistry;
use crate::workspace::Workspace;

pub async fn handle_document_symbol(
    registry: Arc<LanguageRegistry>,
    workspace: Arc<Workspace>,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = params.text_document.uri;

    let lang_id = workspace
        .documents
        .with_doc(&uri, |doc| doc.language_id().to_owned())?;

    let lang = registry.find(&lang_id)?;

    // Ensure tree is parsed.
    let has_tree = workspace
        .documents
        .with_doc(&uri, |doc| doc.source().has_unified_syntax())
        .unwrap_or(false);
    if !has_tree {
        workspace.documents.with_doc_mut(&uri, |doc| {
            if doc.source().has_unified_syntax() {
                return;
            }
            let tree = lang.parse_tree(doc.source().text(), None);
            doc.set_tree(tree);
        });
    }

    let symbols = workspace.documents.with_doc(&uri, |doc| {
        let file = doc.source();
        let root = file.root_node()?;
        lang.collect_symbols(root, file)
    })??;

    Some(DocumentSymbolResponse::Nested(symbols))
}
