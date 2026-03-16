use std::sync::Arc;
use tower_lsp::lsp_types::*;

use crate::language::rope_utils::rope_line_col_to_offset;
use crate::language::{LanguageRegistry, TokenCollector};
use crate::workspace::Workspace;

/// Generate a unique but stable result_id for this document version.
fn make_result_id(version: i32) -> String {
    version.to_string()
}

pub async fn handle_semantic_tokens_full(
    registry: Arc<LanguageRegistry>,
    workspace: Arc<Workspace>,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let uri = params.text_document.uri;

    let lang_id = workspace
        .documents
        .with_doc(&uri, |doc| doc.language_id.clone())?;

    let lang = registry.find(&lang_id)?;

    // Ensure the tree is parsed.
    workspace.documents.with_doc_mut(&uri, |doc| {
        if doc.tree.is_none() {
            doc.tree = lang.parse_tree(&doc.text, None);
        }
    })?;

    // Collect all tokens.
    let (version, data) = workspace.documents.with_doc(&uri, |doc| {
        let tree = doc.tree.as_ref()?;
        let mut collector = TokenCollector::new(doc.text.as_bytes(), &doc.rope, lang);
        collector.collect(tree.root_node());
        Some((doc.version, collector.finish()))
    })??;

    let result_id = make_result_id(version);

    // Store in the cache for future delta requests.
    let data_clone = data.clone();
    let result_id_clone = result_id.clone();
    workspace.documents.with_doc_mut(&uri, |doc| {
        doc.semantic_token_cache = Some((result_id_clone, data_clone));
    });

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: Some(result_id),
        data,
    }))
}

/// Kept for backward compat — the old handler name.
pub async fn handle_semantic_tokens(
    registry: Arc<LanguageRegistry>,
    workspace: Arc<Workspace>,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    handle_semantic_tokens_full(registry, workspace, params).await
}

pub async fn handle_semantic_tokens_range(
    registry: Arc<LanguageRegistry>,
    workspace: Arc<Workspace>,
    params: SemanticTokensRangeParams,
) -> Option<SemanticTokensRangeResult> {
    let uri = params.text_document.uri;
    let range = params.range;

    let lang_id = workspace
        .documents
        .with_doc(&uri, |doc| doc.language_id.clone())?;

    let lang = registry.find(&lang_id)?;

    // Ensure the tree is parsed.
    workspace.documents.with_doc_mut(&uri, |doc| {
        if doc.tree.is_none() {
            doc.tree = lang.parse_tree(&doc.text, None);
        }
    })?;

    let data = workspace.documents.with_doc(&uri, |doc| {
        let tree = doc.tree.as_ref()?;
        let rope = &doc.rope;

        // Convert LSP Range (line/character) to byte offsets.
        let start_byte =
            rope_line_col_to_offset(rope, range.start.line, range.start.character).unwrap_or(0);
        let end_byte = rope_line_col_to_offset(rope, range.end.line, range.end.character)
            .unwrap_or(doc.text.len());

        let mut collector = TokenCollector::new(doc.text.as_bytes(), rope, lang);
        collector.collect_range(tree.root_node(), start_byte, end_byte);
        Some(collector.finish())
    })??;

    Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    }))
}

pub async fn handle_semantic_tokens_full_delta(
    registry: Arc<LanguageRegistry>,
    workspace: Arc<Workspace>,
    params: SemanticTokensDeltaParams,
) -> Option<SemanticTokensFullDeltaResult> {
    let uri = params.text_document.uri;
    let previous_result_id = params.previous_result_id;

    let lang_id = workspace
        .documents
        .with_doc(&uri, |doc| doc.language_id.clone())?;

    let lang = registry.find(&lang_id)?;

    // Ensure the tree is parsed.
    workspace.documents.with_doc_mut(&uri, |doc| {
        if doc.tree.is_none() {
            doc.tree = lang.parse_tree(&doc.text, None);
        }
    })?;

    // Compute fresh tokens.
    let (version, new_data) = workspace.documents.with_doc(&uri, |doc| {
        let tree = doc.tree.as_ref()?;
        let mut collector = TokenCollector::new(doc.text.as_bytes(), &doc.rope, lang);
        collector.collect(tree.root_node());
        Some((doc.version, collector.finish()))
    })??;

    let new_result_id = make_result_id(version);

    // Try to compute a delta against the previous result.
    let maybe_old_data = workspace.documents.with_doc(&uri, |doc| {
        doc.semantic_token_cache
            .as_ref()
            .filter(|(id, _)| id == &previous_result_id)
            .map(|(_, data)| data.clone())
    })?;

    // Update the cache with the new result.
    {
        let new_data_clone = new_data.clone();
        let id_clone = new_result_id.clone();
        workspace.documents.with_doc_mut(&uri, |doc| {
            doc.semantic_token_cache = Some((id_clone, new_data_clone));
        });
    }

    match maybe_old_data {
        Some(old_data) => {
            // Compute SemanticTokensEdit list by diffing old vs new flat token arrays.
            // Each SemanticToken is 5 u32 fields; we diff at that granularity.
            let edits = diff_semantic_tokens(&old_data, &new_data);
            Some(SemanticTokensFullDeltaResult::TokensDelta(
                SemanticTokensDelta {
                    result_id: Some(new_result_id),
                    edits,
                },
            ))
        }
        None => {
            // Previous result ID not in cache; fall back to a full response.
            Some(SemanticTokensFullDeltaResult::Tokens(SemanticTokens {
                result_id: Some(new_result_id),
                data: new_data,
            }))
        }
    }
}

/// Compute a minimal sequence of [`SemanticTokensEdit`] to transform `old`
/// into `new`. Both slices are flat arrays of [`SemanticToken`] values in the
/// document order expected by the LSP specification.
///
/// Strategy: a straightforward linear scan that groups consecutive changed
/// positions into single edit operations. This is O(n) and produces at most
/// one edit per changed run, which satisfies the LSP requirement that edits
/// are non-overlapping and sorted by `start`.
fn diff_semantic_tokens(old: &[SemanticToken], new: &[SemanticToken]) -> Vec<SemanticTokensEdit> {
    let old_len = old.len();
    let new_len = new.len();

    if old_len == 0 && new_len == 0 {
        return vec![];
    }

    let mut edits: Vec<SemanticTokensEdit> = Vec::new();

    // Find the longest common prefix.
    let prefix_len = old
        .iter()
        .zip(new.iter())
        .take_while(|(a, b)| a == b)
        .count();

    if prefix_len == old_len && prefix_len == new_len {
        // No change.
        return vec![];
    }

    // Find the longest common suffix (after the prefix).
    let old_tail = &old[prefix_len..];
    let new_tail = &new[prefix_len..];
    let suffix_len = old_tail
        .iter()
        .rev()
        .zip(new_tail.iter().rev())
        .take_while(|(a, b)| a == b)
        .count();

    // The changed region in old is old[prefix_len .. old_len - suffix_len].
    // The replacement data is   new[prefix_len .. new_len - suffix_len].
    let old_start = prefix_len;
    let old_end = old_len.saturating_sub(suffix_len);
    let new_start = prefix_len;
    let new_end = new_len.saturating_sub(suffix_len);

    let delete_count = (old_end - old_start) as u32;
    let replacement: &[SemanticToken] = &new[new_start..new_end];

    edits.push(SemanticTokensEdit {
        start: old_start as u32,
        delete_count,
        data: if replacement.is_empty() {
            None
        } else {
            Some(replacement.to_vec())
        },
    });

    edits
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::SemanticToken;

    fn tok(dl: u32, ds: u32, len: u32, ty: u32, mods: u32) -> SemanticToken {
        SemanticToken {
            delta_line: dl,
            delta_start: ds,
            length: len,
            token_type: ty,
            token_modifiers_bitset: mods,
        }
    }

    #[test]
    fn diff_empty_to_empty() {
        assert!(diff_semantic_tokens(&[], &[]).is_empty());
    }

    #[test]
    fn diff_no_change() {
        let tokens = vec![tok(0, 0, 5, 0, 0), tok(1, 2, 3, 1, 0)];
        assert!(diff_semantic_tokens(&tokens, &tokens).is_empty());
    }

    #[test]
    fn diff_append_one() {
        let old = vec![tok(0, 0, 5, 0, 0)];
        let new = vec![tok(0, 0, 5, 0, 0), tok(1, 0, 3, 1, 0)];
        let edits = diff_semantic_tokens(&old, &new);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        // Insertion after the common prefix (index 1)
        assert_eq!(e.start, 1);
        assert_eq!(e.delete_count, 0);
        assert_eq!(e.data.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn diff_delete_one() {
        let old = vec![tok(0, 0, 5, 0, 0), tok(1, 0, 3, 1, 0)];
        let new = vec![tok(0, 0, 5, 0, 0)];
        let edits = diff_semantic_tokens(&old, &new);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        assert_eq!(e.start, 1);
        assert_eq!(e.delete_count, 1);
        assert!(e.data.is_none());
    }

    #[test]
    fn diff_replace_middle() {
        let a = tok(0, 0, 5, 0, 0);
        let b_old = tok(1, 0, 3, 1, 0);
        let b_new = tok(1, 0, 3, 2, 0); // changed type
        let c = tok(2, 0, 4, 0, 0);

        let old = vec![a, b_old, c];
        let new = vec![a, b_new, c];
        let edits = diff_semantic_tokens(&old, &new);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        assert_eq!(e.start, 1);
        assert_eq!(e.delete_count, 1);
        assert_eq!(e.data.as_ref().unwrap(), &vec![b_new]);
    }

    #[test]
    fn diff_full_replacement() {
        let old = vec![tok(0, 0, 5, 0, 0), tok(1, 0, 3, 1, 0)];
        let new = vec![tok(0, 5, 5, 2, 0)];
        let edits = diff_semantic_tokens(&old, &new);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        assert_eq!(e.start, 0);
        assert_eq!(e.delete_count, 2);
        assert_eq!(e.data.as_ref().unwrap().len(), 1);
    }

    #[test]
    fn diff_empty_to_some() {
        let new = vec![tok(0, 0, 5, 0, 0), tok(1, 0, 3, 1, 0)];
        let edits = diff_semantic_tokens(&[], &new);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        assert_eq!(e.start, 0);
        assert_eq!(e.delete_count, 0);
        assert_eq!(e.data.as_ref().unwrap().len(), 2);
    }

    #[test]
    fn diff_some_to_empty() {
        let old = vec![tok(0, 0, 5, 0, 0), tok(1, 0, 3, 1, 0)];
        let edits = diff_semantic_tokens(&old, &[]);
        assert_eq!(edits.len(), 1);
        let e = &edits[0];
        assert_eq!(e.start, 0);
        assert_eq!(e.delete_count, 2);
        assert!(e.data.is_none());
    }
}
