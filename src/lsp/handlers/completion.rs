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
        .map(|c| {
            let mut item = candidate_to_lsp(c, &doc.content);
            // Import scenario: Replace the entire line with the import prefix using textEdit
            if matches!(
                ctx.location,
                crate::completion::context::CursorLocation::Import { .. }
            ) && let Some(edit) = make_import_text_edit(c, &doc.content, position)
            {
                item.text_edit = Some(edit);
                item.insert_text = None;
                item.insert_text_format = None;
            }
            // TODO: it's a mess
            item.filter_text = Some(c.insert_text.clone());

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

fn make_import_text_edit(
    candidate: &crate::completion::candidate::CompletionCandidate,
    source: &str,
    position: tower_lsp::lsp_types::Position,
) -> Option<tower_lsp::lsp_types::CompletionTextEdit> {
    let lines: Vec<&str> = source.lines().collect();
    let current_line = lines.get(position.line as usize)?;

    // 找包含 import 关键字的行（当前行或往上找）
    let import_line_idx = if current_line.trim_start().starts_with("import") {
        position.line as usize
    } else {
        (0..position.line as usize)
            .rev()
            .find(|&i| lines[i].trim_start().starts_with("import"))?
    };

    // 如果 import 和包名在同一行，替换 import 后面的内容
    // 如果不在同一行（换行 import），只替换当前行（包名行）
    let (start_line, start_char) = if import_line_idx == position.line as usize {
        // 同行：从 "import " 后开始
        let import_line_str = lines[import_line_idx];
        let import_kw_pos = import_line_str.find("import")?;
        let start = (import_kw_pos + "import".len() + 1) as u32;
        (import_line_idx as u32, start)
    } else {
        // 换行：只替换当前行，从行首非空白处开始
        let indent = current_line.len() - current_line.trim_start().len();
        (position.line, indent as u32)
    };

    // 结束位置：当前行去掉分号和注释后的末尾
    let end_char = current_line
        .find(';')
        .or_else(|| current_line.find("//"))
        .map(|p| p as u32)
        .unwrap_or(current_line.len() as u32);

    Some(CompletionTextEdit::Edit(TextEdit {
        range: Range {
            start: Position {
                line: start_line,
                character: start_char,
            },
            end: Position {
                line: position.line,
                character: end_char,
            },
        },
        new_text: candidate.insert_text.clone(),
    }))
}
