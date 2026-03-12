use anyhow::Result;
use tower_lsp::Client;
use tower_lsp::lsp_types::{
    NumberOrString, ProgressParams, ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin,
    WorkDoneProgressCreateParams, WorkDoneProgressEnd, WorkDoneProgressReport,
};

pub struct ImportProgress {
    client: Client,
    token: NumberOrString,
    finished: bool,
}

impl ImportProgress {
    pub async fn begin(client: Client, token: String, title: &str, message: &str) -> Result<Self> {
        let token = NumberOrString::String(token);
        client
            .send_request::<tower_lsp::lsp_types::request::WorkDoneProgressCreate>(
                WorkDoneProgressCreateParams {
                    token: token.clone(),
                },
            )
            .await
            .ok();

        client
            .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
                token: token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(
                    WorkDoneProgressBegin {
                        title: title.to_string(),
                        cancellable: Some(false),
                        message: Some(message.to_string()),
                        percentage: None,
                    },
                )),
            })
            .await;

        Ok(Self {
            client,
            token,
            finished: false,
        })
    }

    pub async fn report(&self, message: &str) {
        self.client
            .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::Report(
                    WorkDoneProgressReport {
                        cancellable: Some(false),
                        message: Some(message.to_string()),
                        percentage: None,
                    },
                )),
            })
            .await;
    }

    pub async fn finish(mut self, message: &str) {
        self.finished = true;
        self.client
            .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                    message: Some(message.to_string()),
                })),
            })
            .await;
    }
}

impl Drop for ImportProgress {
    fn drop(&mut self) {
        if self.finished {
            return;
        }

        let client = self.client.clone();
        let token = self.token.clone();
        tokio::spawn(async move {
            client
                .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
                    token,
                    value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(
                        WorkDoneProgressEnd {
                            message: Some("Import ended".to_string()),
                        },
                    )),
                })
                .await;
        });
    }
}
