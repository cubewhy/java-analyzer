use std::sync::Arc;
use tower_lsp::jsonrpc::Result as LspResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info};

use super::capabilities::server_capabilities;
use super::handlers::completion::handle_completion;
use crate::completion::engine::CompletionEngine;
use crate::index::ClassOrigin;
use crate::index::codebase::{index_codebase, index_source_text};
use crate::index::jdk::JdkIndexer;
use crate::language::LanguageRegistry;
use crate::workspace::{Workspace, document::Document};

pub struct Backend {
    client: Client,
    workspace: Arc<Workspace>,
    engine: Arc<CompletionEngine>,
    registry: Arc<LanguageRegistry>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            workspace: Arc::new(Workspace::new()),
            engine: Arc::new(CompletionEngine::new()),
            registry: Arc::new(LanguageRegistry::new()),
        }
    }

    /// Language ID determination
    fn is_supported(lang_id: &str) -> bool {
        matches!(lang_id, "java" | "kotlin")
    }

    /// Trigger background indexing (without blocking the response)
    fn spawn_index_workspace(&self, root: std::path::PathBuf) {
        let workspace = Arc::clone(&self.workspace);
        let client = self.client.clone();

        tokio::spawn(async move {
            client
                .log_message(
                    MessageType::INFO,
                    format!("Indexing workspace: {}", root.display()),
                )
                .await;

            // Source file priority, blocking and waiting for completion
            let codebase = tokio::task::spawn_blocking({
                let root = root.clone();
                move || index_codebase(&root)
            })
            .await;

            match codebase {
                Ok(result) => {
                    let file_count = result.file_count;
                    let class_count = result.classes.len();
                    workspace.index.write().await.add_classes(result.classes);

                    client
                        .log_message(
                            MessageType::INFO,
                            format!(
                                "✓ Codebase indexed: {} files, {} classes",
                                file_count, class_count
                            ),
                        )
                        .await;

                    client.semantic_tokens_refresh().await.ok();
                }
                Err(e) => error!(error = %e, "codebase indexing panicked"),
            }

            // JAR files are indexed after the source files and have lower priority.
            for jar_dir in find_jar_dirs(&root) {
                workspace.load_jars_from_dir(jar_dir).await;
            }

            // Index JDK (lowest priority, runs last)
            client
                .log_message(MessageType::INFO, "Indexing JDK...")
                .await;
            let jdk_classes =
                tokio::task::spawn_blocking(crate::index::jdk::JdkIndexer::index).await;

            match jdk_classes {
                Ok(classes) if !classes.is_empty() => {
                    let count = classes.len();
                    workspace.index.write().await.add_classes(classes);
                    client
                        .log_message(
                            MessageType::INFO,
                            format!("✓ JDK indexed: {} classes", count),
                        )
                        .await;
                    client.semantic_tokens_refresh().await.ok();
                }
                Ok(_) => {
                    client
                        .log_message(
                            MessageType::WARNING,
                            "JDK not found (set JAVA_HOME to enable JDK completion)",
                        )
                        .await;
                }
                Err(e) => {
                    error!(error = %e, "JDK indexing panicked");
                }
            }

            // JAR files
            for jar_dir in find_jar_dirs(&root) {
                workspace.load_jars_from_dir(jar_dir).await;
            }

            client
                .log_message(MessageType::INFO, "Workspace indexing complete")
                .await;
        });
    }
}

/// Locate common JAR directories within the workspace
fn find_jar_dirs(root: &std::path::Path) -> Vec<std::path::PathBuf> {
    let candidates = [
        ".gradle/caches",
        ".m2/repository",
        "build/libs",
        "out/artifacts",
        "libs",
        "lib",
    ];
    candidates
        .iter()
        .map(|rel| root.join(rel))
        .filter(|p| p.exists())
        .collect()
}

/// Infer Language ID from LSP URI
fn language_id_from_uri(uri: &Url) -> &'static str {
    match uri.path().rsplit('.').next() {
        Some("kt") | Some("kts") => "kotlin",
        _ => "java",
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> LspResult<InitializeResult> {
        info!("LSP initialize");

        // Trigger workspace index
        if let Some(root) = params.root_uri.as_ref().and_then(|u| u.to_file_path().ok()) {
            self.spawn_index_workspace(root);
        } else if let Some(folders) = params.workspace_folders {
            for folder in folders {
                if let Ok(root) = folder.uri.to_file_path() {
                    self.spawn_index_workspace(root);
                }
            }
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "java-analyzer".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
            capabilities: server_capabilities(),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("LSP initialized");
        self.client
            .log_message(MessageType::INFO, "java-analyzer ready")
            .await;
    }

    async fn shutdown(&self) -> LspResult<()> {
        info!("LSP shutdown");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let td = params.text_document;
        if !Self::is_supported(&td.language_id) {
            return;
        }

        info!(uri = %td.uri, lang = %td.language_id, "did_open");

        // 存入文档
        self.workspace.documents.open(Document::new(
            td.uri.clone(),
            td.language_id.clone(),
            td.version,
            td.text.clone(),
        ));

        // 增量更新索引
        let uri_str = td.uri.to_string();
        let classes = index_source_text(&uri_str, &td.text, &td.language_id);
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        self.workspace
            .index
            .write()
            .await
            .update_source(origin, classes);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = &params.text_document.uri;

        // 全量同步：取最后一次变更
        let content = match params.content_changes.into_iter().last() {
            Some(c) => c.text,
            None => return,
        };

        let lang_id = self
            .workspace
            .documents
            .get(uri)
            .map(|d| d.language_id.clone())
            .unwrap_or_else(|| language_id_from_uri(uri).to_string());

        if !Self::is_supported(&lang_id) {
            return;
        }

        self.workspace
            .documents
            .update(uri, params.text_document.version, content.clone());

        // 增量更新索引（去抖动：生产实现可加 500ms debounce）
        let uri_str = uri.to_string();
        let classes = index_source_text(&uri_str, &content, &lang_id);
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        self.workspace
            .index
            .write()
            .await
            .update_source(origin, classes);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        // 保存时重新解析（如果有内容的话）
        if let Some(text) = params.text {
            let uri = &params.text_document.uri;
            let lang_id = self
                .workspace
                .documents
                .get(uri)
                .map(|d| d.language_id.clone())
                .unwrap_or_else(|| language_id_from_uri(uri).to_string());

            if Self::is_supported(&lang_id) {
                let uri_str = uri.to_string();
                let classes = index_source_text(&uri_str, &text, &lang_id);
                let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
                self.workspace
                    .index
                    .write()
                    .await
                    .update_source(origin, classes);
            }
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;
        info!(uri = %uri, "did_close");
        self.workspace.documents.close(uri);
        // 注意：关闭文件不从索引里删除，索引保留（用户可能只是关闭了编辑器 tab）
    }

    // ── 补全 ──────────────────────────────────────────────────────────────────

    async fn completion(&self, params: CompletionParams) -> LspResult<Option<CompletionResponse>> {
        let response = handle_completion(
            Arc::clone(&self.workspace),
            Arc::clone(&self.engine),
            Arc::clone(&self.registry),
            params,
        )
        .await;
        Ok(response)
    }

    // ── 预留：hover ───────────────────────────────────────────────────────────

    async fn hover(&self, _params: HoverParams) -> LspResult<Option<Hover>> {
        Ok(None) // TODO
    }

    // ── 预留：go to definition ────────────────────────────────────────────────

    async fn goto_definition(
        &self,
        _params: GotoDefinitionParams,
    ) -> LspResult<Option<GotoDefinitionResponse>> {
        Ok(None) // TODO
    }

    // ── 预留：document symbols（outline）─────────────────────────────────────

    async fn document_symbol(
        &self,
        _params: DocumentSymbolParams,
    ) -> LspResult<Option<DocumentSymbolResponse>> {
        Ok(None) // TODO
    }
}
