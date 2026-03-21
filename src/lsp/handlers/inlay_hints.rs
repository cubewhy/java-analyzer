use std::sync::Arc;

use tower_lsp::lsp_types::{InlayHint, InlayHintParams};

use crate::language::{LanguageRegistry, ParseEnv};
use crate::workspace::Workspace;

pub async fn handle_inlay_hints(
    workspace: Arc<Workspace>,
    registry: Arc<LanguageRegistry>,
    params: InlayHintParams,
) -> Option<Vec<InlayHint>> {
    let uri = &params.text_document.uri;
    let lang_id = workspace
        .documents
        .with_doc(uri, |doc| doc.language_id().to_owned())?;
    let lang = registry.find(&lang_id)?;
    if !lang.supports_inlay_hints() {
        return None;
    }

    let analysis = workspace.analysis_context_for_uri(uri);
    let scope = analysis.scope();

    // Use cached IndexView and NameTable via Salsa for better performance
    let (view, name_table) = {
        let db = workspace.salsa_db.lock();

        // Get cached IndexView (memoized)
        let view = crate::salsa_queries::get_index_view_for_context(
            &*db,
            scope.module,
            analysis.classpath,
            analysis.source_root,
        );

        // Get cached NameTable (memoized)
        let name_table = crate::salsa_queries::get_name_table_for_context(
            &*db,
            scope.module,
            analysis.classpath,
            analysis.source_root,
        );

        (view, name_table)
    };

    let env = ParseEnv {
        name_table: Some(name_table),
        workspace: Some(workspace.clone()),
    };

    // Ensure tree is parsed.
    let has_tree = workspace
        .documents
        .with_doc(uri, |doc| doc.source().tree.is_some())
        .unwrap_or(false);
    if !has_tree {
        workspace.documents.with_doc_mut(uri, |doc| {
            if doc.source().tree.is_some() {
                return;
            }
            let tree = lang.parse_tree(doc.source().text(), None);
            doc.set_tree(tree);
        });
    }

    workspace.documents.with_doc(uri, |doc| {
        let file = doc.source();
        lang.collect_inlay_hints_with_tree(file, params.range, &env, &view)
    })?
}
