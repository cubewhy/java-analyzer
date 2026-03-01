use tower_lsp::lsp_types::*;

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
        ..Default::default()
    }
}
