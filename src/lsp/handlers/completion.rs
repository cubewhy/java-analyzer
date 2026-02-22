use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tracing::debug;

use super::super::converters::candidate_to_lsp;
use crate::completion::engine::CompletionEngine;
use crate::language::LanguageRegistry;
use crate::workspace::Workspace;

pub async fn handle_completion(
    workspace: Arc<Workspace>,
    engine: Arc<CompletionEngine>,
    registry: Arc<LanguageRegistry>,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let trigger = params
        .context
        .as_ref()
        .and_then(|ctx| ctx.trigger_character.as_deref())
        .and_then(|s| s.chars().next());

    // load document
    let doc = workspace.documents.get(uri)?;
    let lang = registry.find(&doc.language_id)?;

    let uri_str = uri.as_str();

    tracing::debug!(
        uri = %uri,
        lang = lang.id(),
        line = position.line,
        character = position.character,
        trigger = ?trigger,
        "completion request"
    );

    // Parse and complete the context
    let ctx = lang
        .parse_completion_context(&doc.content, position.line, position.character, trigger)?
        .with_file_uri(Arc::from(uri_str));

    let ctx = if ctx.enclosing_package.is_none() {
        if let Some(pkg) = workspace.infer_package_from_uri(uri_str).await {
            ctx.with_inferred_package(pkg)
        } else {
            ctx
        }
    } else {
        ctx
    };

    tracing::debug!(location = ?ctx.location, query = %ctx.query, "parsed context");

    // Perform completion
    let mut index = workspace.index.write().await;
    let candidates = engine.complete(ctx.clone(), &mut index);
    drop(index);

    if candidates.is_empty() {
        debug!("no candidates");
        return None;
    }

    // Language Specialization Post-processing
    let candidates = lang.post_process_candidates(candidates, &ctx);

    // Convert to LSP format (limit the number to avoid an explosion)
    const MAX_ITEMS: usize = 100;
    let items: Vec<CompletionItem> = candidates
        .iter()
        .take(MAX_ITEMS)
        .map(|c| candidate_to_lsp(c, &doc.content))
        .collect();

    let is_incomplete = candidates.len() > MAX_ITEMS;

    debug!(
        count = items.len(),
        incomplete = is_incomplete,
        "returning completions"
    );

    Some(CompletionResponse::List(CompletionList {
        is_incomplete,
        items,
    }))
}
