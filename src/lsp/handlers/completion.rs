use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tracing::debug;

use super::super::converters::candidate_to_lsp;
use crate::completion::engine::CompletionEngine;
use crate::completion::CandidateKind;
use crate::language::LanguageRegistry;
use crate::semantic::CursorLocation;
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

    let lang_id = workspace
        .documents
        .with_doc(uri, |doc| doc.language_id.clone())?;

    let lang = registry.find(&lang_id)?;

    let uri_str = uri.as_str();

    tracing::debug!(
        uri = %uri,
        lang = lang.id(),
        line = position.line,
        character = position.character,
        trigger = ?trigger,
        "completion request"
    );

    // 2) 确保 doc.tree 已缓存（缺失则同步 parse 一次写回）
    //    注意：闭包里不能 await，所以 parse 必须是同步的（tree-sitter parse 是同步）
    workspace.documents.with_doc_mut(uri, |doc| {
        if doc.tree.is_none() {
            let mut parser = lang.make_parser();
            doc.tree = parser.parse(&doc.text, None);
        }
    })?;

    // 3) 在 read 闭包里：直接用缓存 tree + rope 构造 SemanticContext
    //    同时把 source clone 出来（后面生成 textEdit 需要 &str，且我们会 await index lock）
    let (ctx, source_for_edits) = workspace.documents.with_doc(uri, |doc| {
        let tree = doc.tree.as_ref()?;
        let ctx = lang
            .parse_completion_context_with_tree(
                &doc.text,
                &doc.rope,
                tree.root_node(),
                position.line,
                position.character,
                trigger,
            )?
            .with_file_uri(Arc::from(uri_str))
            .with_language_id(crate::language::LanguageId::new(lang_id.clone()));

        Some((ctx, doc.text.clone()))
    })??;

    tracing::debug!(location = ?ctx.location, query = %ctx.query, "parsed context");

    // 4) completion engine（这里会 await，所以不能在 DashMap guard 里做）
    let scope = workspace.scope_for_uri(uri);
    let index = workspace.index.read().await;
    let view = index.view(scope);
    let candidates = engine.complete(scope, ctx.clone(), lang, &view);

    if candidates.is_empty() {
        debug!("no candidates");
        return None;
    }

    let candidates = lang.post_process_candidates(candidates, &ctx);

    // 5) 转 LSP items（用 source_for_edits，而不是旧的 doc.content）
    const MAX_ITEMS: usize = 100;
    let items: Vec<CompletionItem> = candidates
        .iter()
        .take(MAX_ITEMS)
        .map(|c| {
            let mut item = candidate_to_lsp(c, &source_for_edits);

            if matches!(ctx.location, CursorLocation::Import { .. }) {
                if let Some(edit) = crate::completion::import_completion::make_import_text_edit(
                    &c.insert_text,
                    &source_for_edits,
                    position,
                ) {
                    item.text_edit = Some(edit);
                    item.insert_text = None;
                    item.insert_text_format = None;
                }
                item.filter_text = Some(c.insert_text.clone());
            } else if matches!(c.kind, CandidateKind::Package | CandidateKind::ClassName)
                && matches!(
                    ctx.location,
                    CursorLocation::Expression { .. }
                        | CursorLocation::TypeAnnotation { .. }
                        | CursorLocation::MemberAccess { .. }
                )
            {
                if let Some(edit) =
                    make_package_text_edit(&c.insert_text, &source_for_edits, position)
                {
                    item.text_edit = Some(edit);
                    item.insert_text = None;
                    item.insert_text_format = None;
                }
                item.filter_text = Some(c.label.to_string());
            } else if c.source == "override" {
                if let Some(edit) =
                    make_override_text_edit(&c.insert_text, &source_for_edits, position)
                {
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

/// Override candidate textEdit: Replace the entire access-modifier prefix before the cursor with the full method stub
fn make_override_text_edit(
    insert_text: &str,
    source: &str,
    position: Position,
) -> Option<CompletionTextEdit> {
    let line = source.lines().nth(position.line as usize)?;
    let before_cursor = &line[..position.character as usize];

    let start_char = before_cursor
        .rfind(|c: char| !c.is_alphabetic())
        .map(|p| p + 1)
        .unwrap_or(0) as u32;

    Some(CompletionTextEdit::Edit(TextEdit {
        range: Range {
            start: Position {
                line: position.line,
                character: start_char,
            },
            end: Position {
                line: position.line,
                character: position.character,
            },
        },
        new_text: insert_text.to_string(),
    }))
}

/// Expression/MemberAccess 场景下 Package 候选的 textEdit：
/// 替换光标所在的整个"包路径词"（从行首非空白到光标）
fn make_package_text_edit(
    insert_text: &str,
    source: &str,
    position: Position,
) -> Option<CompletionTextEdit> {
    let line = source.lines().nth(position.line as usize)?;
    let before_cursor = &line[..position.character as usize];
    let start_char = before_cursor
        .rfind(|c: char| !c.is_alphanumeric() && c != '.' && c != '_')
        .map(|p| p + 1)
        .unwrap_or(0) as u32;

    Some(CompletionTextEdit::Edit(TextEdit {
        range: Range {
            start: Position {
                line: position.line,
                character: start_char,
            },
            end: Position {
                line: position.line,
                character: position.character,
            },
        },
        new_text: insert_text.to_string(),
    }))
}
