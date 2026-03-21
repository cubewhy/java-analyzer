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
    // unreachable.
    if params.range.start.line > params.range.end.line
        || (params.range.start.line == params.range.end.line
            && params.range.start.character >= params.range.end.character)
    {
        return Some(Vec::new());
    }

    let lang_id = workspace
        .documents
        .with_doc(uri, |doc| doc.language_id().to_owned())?;
    let lang = registry.find(&lang_id)?;
    if !lang.supports_inlay_hints() {
        return None;
    }

    // Ensure tree is parsed.
    let has_tree = workspace
        .documents
        .with_doc(uri, |doc| doc.source().has_unified_syntax())
        .unwrap_or(false);
    if !has_tree {
        workspace.documents.with_doc_mut(uri, |doc| {
            if doc.source().has_unified_syntax() {
                return;
            }
            let tree = lang.parse_tree(doc.source().text(), None);
            doc.set_tree(tree);
        });
    }

    let has_candidates = workspace.documents.with_doc(uri, |doc| {
        lang.may_have_inlay_hints_in_range(doc.source(), params.range)
    })?;
    if !has_candidates {
        return Some(Vec::new());
    }

    let analysis = workspace.analysis_context_for_uri(uri);
    let scope = analysis.scope();

    let (view, name_table) = {
        let db = workspace.salsa_db.lock();
        let view = crate::salsa_queries::get_index_view_for_context(
            &*db,
            scope.module,
            analysis.classpath,
            analysis.source_root,
        );
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

    workspace.documents.with_doc(uri, |doc| {
        lang.collect_inlay_hints_with_tree(doc.source(), params.range, &env, &view)
    })?
}
