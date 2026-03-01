use tower_lsp::lsp_types::*;

use crate::lsp::semantic_tokens::{TOKEN_MODIFIERS, TOKEN_TYPES};

pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".into(), ":".into(), "@".into(), " ".into()]),
            all_commit_characters: None,
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: Some(true),
            },
        }),
        document_symbol_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(
            SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                SemanticTokensRegistrationOptions {
                    text_document_registration_options: TextDocumentRegistrationOptions {
                        document_selector: Some(vec![DocumentFilter {
                            language: Some("java".into()),
                            scheme: Some("file".into()),
                            pattern: None,
                        }]),
                    },
                    semantic_tokens_options: SemanticTokensOptions {
                        legend: SemanticTokensLegend {
                            token_types: TOKEN_TYPES.to_vec(),
                            token_modifiers: TOKEN_MODIFIERS.to_vec(),
                        },
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        ..Default::default()
                    },
                    static_registration_options: StaticRegistrationOptions {
                        ..Default::default()
                    },
                },
            ),
        ),
        ..Default::default()
    }
}
