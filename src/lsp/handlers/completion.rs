use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tracing::debug;

use super::super::converters::candidate_to_lsp;
use crate::completion::CandidateKind;
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
        .map(|c| {
            let mut item = candidate_to_lsp(c, &doc.content);
            if matches!(
                ctx.location,
                crate::completion::context::CursorLocation::Import { .. }
            ) {
                if let Some(edit) = crate::completion::import_completion::make_import_text_edit(
                    &c.insert_text,
                    &doc.content,
                    position,
                ) {
                    item.text_edit = Some(edit);
                    item.insert_text = None;
                    item.insert_text_format = None;
                }
                item.filter_text = Some(c.insert_text.clone());
            } else if c.kind == CandidateKind::Package {
                // Package 候选在非 import 场景下也需要 textEdit，
                // 否则 VSCode 只插入截断后的短名，导致重复
                if let Some(edit) = make_package_text_edit(&c.insert_text, &doc.content, position) {
                    item.text_edit = Some(edit);
                    item.insert_text = None;
                    item.insert_text_format = None;
                }
                item.filter_text = Some(c.label.to_string());
            }

            item
        })
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

// TODO: move to package_completion.rs

/// Expression/MemberAccess 场景下 Package 候选的 textEdit：
/// 替换光标所在的整个"包路径词"（从行首非空白到光标）
fn make_package_text_edit(
    insert_text: &str,
    source: &str,
    position: Position,
) -> Option<CompletionTextEdit> {
    let line = source.lines().nth(position.line as usize)?;

    // 找光标前最后一个不属于包路径字符（字母数字下划线点）的位置
    // 从那里到光标就是需要替换的范围
    let before_cursor = &line[..position.character as usize];
    let start_char = before_cursor
        .rfind(|c: char| !c.is_alphanumeric() && c != '.' && c != '_')
        .map(|p| p + 1)
        .unwrap_or(0) as u32;

    let end_char = position.character;

    Some(CompletionTextEdit::Edit(TextEdit {
        range: Range {
            start: Position {
                line: position.line,
                character: start_char,
            },
            end: Position {
                line: position.line,
                character: end_char,
            },
        },
        new_text: insert_text.to_string(),
    }))
}
