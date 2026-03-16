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
    let index = workspace.index.read().await;
    let view =
        index.view_for_analysis_context(scope.module, analysis.classpath, analysis.source_root);
    let env = ParseEnv {
        name_table: Some(view.build_name_table()),
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
