use std::sync::Arc;
use tower_lsp::jsonrpc::Result as LspResult;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::{error, info};
use tree_sitter::{InputEdit, Point};

use super::capabilities::server_capabilities;
use super::handlers::completion::handle_completion;
use crate::build_integration::{BuildIntegrationService, ReloadReason};
use crate::completion::engine::CompletionEngine;
use crate::decompiler::cache::DecompilerCache;
use crate::index::ClassOrigin;
use crate::index::codebase::index_source_text;
use crate::index::jdk::JdkIndexer;
use crate::language::LanguageRegistry;
use crate::language::rope_utils::rope_line_col_to_offset;
use crate::lsp::config::JavaAnalyzerConfig;
use crate::lsp::handlers::goto_definition::handle_goto_definition;
use crate::lsp::handlers::inlay_hints::handle_inlay_hints;
use crate::lsp::handlers::semantic_tokens::{
    handle_semantic_tokens, handle_semantic_tokens_full_delta, handle_semantic_tokens_range,
};
use crate::workspace::{Workspace, document::Document};

pub struct Backend {
    client: Client,
    pub workspace: Arc<Workspace>,
    engine: Arc<CompletionEngine>,
    pub registry: Arc<LanguageRegistry>,
    pub config: tokio::sync::RwLock<JavaAnalyzerConfig>,
    pub decompiler_cache: crate::decompiler::cache::DecompilerCache,
    build_services: tokio::sync::RwLock<Vec<BuildIntegrationService>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let cache_dir = dirs::cache_dir()
            .unwrap_or_else(std::env::temp_dir)
            .join("java-analyzer")
            .join("decompiled");

        Self {
            client,
            workspace: Arc::new(Workspace::new()),
            engine: Arc::new(CompletionEngine::new()),
            registry: Arc::new(LanguageRegistry::new()),
            config: tokio::sync::RwLock::new(JavaAnalyzerConfig::default()),
            decompiler_cache: DecompilerCache::new(cache_dir),
            build_services: tokio::sync::RwLock::new(Vec::new()),
        }
    }

    async fn configure_workspace_root(&self, root: std::path::PathBuf) {
        let workspace = Arc::clone(&self.workspace);
        let client = self.client.clone();

        let config = self.config.read().await;
        let jdk_path = config.jdk_path.clone();

        if let Some(jdk_path) = jdk_path.clone() {
            // JDK
            with_progress(
                &client,
                "java-analyzer/index/jdk",
                "Indexing JDK",
                || async {
                    let jdk_classes = tokio::task::spawn_blocking(|| {
                        let indexer = JdkIndexer::new(jdk_path);

                        indexer.index()
                    })
                    .await;
                    match jdk_classes {
                        Ok(classes) if !classes.is_empty() => {
                            let msg = format!("✓ JDK: {} classes", classes.len());
                            workspace.set_jdk_classes(classes).await;
                            client.log_message(MessageType::INFO, msg).await;
                        }
                        Ok(_) => {
                            client
                                .log_message(
                                    MessageType::WARNING,
                                    "JDK not found — set JAVA_HOME for JDK completion",
                                )
                                .await;
                        }
                        Err(e) => error!(error = %e, "JDK indexing panicked"),
                    }
                },
            )
            .await;
        }
        drop(config);

        // build tools
        let service =
            BuildIntegrationService::new(root, Arc::clone(&workspace), client.clone(), jdk_path);
        service.schedule_reload(ReloadReason::Initialize);
        self.build_services.write().await.push(service);
    }

    pub async fn update_config(&self, params: serde_json::Value) {
        let mut config_guard = self.config.write().await;
        match serde_json::from_value::<JavaAnalyzerConfig>(params) {
            Ok(new_config) => {
                tracing::info!(config = ?new_config, "Config updated");
                self.decompiler_cache
                    .set_decompiler(&new_config.decompiler_backend);
                *config_guard = new_config;
            }
            Err(e) => {
                tracing::error!("Failed to parse incoming config: {e:#}");
            }
        }
    }
    async fn notify_build_file_change(&self, uri: &Url) {
        if let Ok(path) = uri.to_file_path() {
            let services = self.build_services.read().await.clone();
            for service in services {
                service.notify_paths_changed([path.clone()]).await;
            }
        }
    }

    async fn register_build_watchers(&self) {
        let options = serde_json::json!({
            "watchers": [
                { "globPattern": "**/build.gradle" },
                { "globPattern": "**/build.gradle.kts" },
                { "globPattern": "**/settings.gradle" },
                { "globPattern": "**/settings.gradle.kts" },
                { "globPattern": "**/gradle.properties" },
                { "globPattern": "**/gradle/libs.versions.toml" }
            ]
        });

        self.client
            .send_request::<tower_lsp::lsp_types::request::RegisterCapability>(RegistrationParams {
                registrations: vec![Registration {
                    id: "java-analyzer-build-watchers".into(),
                    method: "workspace/didChangeWatchedFiles".into(),
                    register_options: Some(options),
                }],
            })
            .await
            .ok();
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> LspResult<InitializeResult> {
        info!("LSP initialize");

        if let Some(options) = params.initialization_options {
            self.update_config(options).await;
        }

        // Trigger workspace index
        if let Some(root) = params.root_uri.as_ref().and_then(|u| u.to_file_path().ok()) {
            self.configure_workspace_root(root).await;
        } else if let Some(root) = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| folder.uri.to_file_path().ok())
        {
            self.configure_workspace_root(root).await;
        } else if let Some(folders) = params.workspace_folders
            && folders.len() > 1
        {
            self.client
                .log_message(
                    MessageType::WARNING,
                    "java-analyzer build import currently uses the first workspace folder only",
                )
                .await;
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
        self.register_build_watchers().await;
        self.client
            .log_message(MessageType::INFO, "java-analyzer ready")
            .await;
    }

    async fn shutdown(&self) -> LspResult<()> {
        info!("LSP shutdown");
        Ok(())
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> LspResult<Option<Vec<InlayHint>>> {
        Ok(handle_inlay_hints(
            Arc::clone(&self.workspace),
            Arc::clone(&self.registry),
            params,
        )
        .await)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let td = params.text_document;

        let lang = match self.registry.find(&td.language_id) {
            Some(l) => l,
            None => return,
        };

        info!(uri = %td.uri, lang = %td.language_id, "did_open");

        self.workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                td.uri.clone(),
                td.language_id.as_str(),
                td.version,
                td.text.as_str(),
                None,
            )));

        let mut parser = lang.make_parser();
        let tree = parser.parse(&td.text, None);

        self.workspace.documents.with_doc_mut(&td.uri, |doc| {
            doc.set_tree(tree);
        });

        // Initialize Salsa database with the opened file
        let salsa_file =
            self.workspace
                .get_or_create_salsa_file(&td.uri, &td.text, &td.language_id);

        let uri_str = td.uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(&td.uri);

        // Use Salsa queries for incremental parsing
        let classes = {
            let db = self.workspace.salsa_db.lock();

            // Trigger parse query (memoized) - this tracks changes
            let _result = crate::salsa_queries::index::extract_classes(&*db, salsa_file);

            // Get the actual classes (not memoized, but fast)
            crate::salsa_queries::index::get_extracted_classes(&*db, salsa_file)
        };

        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %td.uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            class_count = classes.len(),
            language = lang.id(),
            "did_open indexing with Salsa (incremental)"
        );
        self.workspace.index.write().update_source_in_context(
            analysis.module,
            analysis.source_root,
            origin,
            classes,
        );

        self.client.semantic_tokens_refresh().await.ok();
        self.notify_build_file_change(&td.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = &params.text_document.uri;

        let Some(lang_id) = self
            .workspace
            .documents
            .with_doc(uri, |d| d.language_id().to_owned())
        else {
            return;
        };

        let lang = match self.registry.find(&lang_id) {
            Some(l) => l,
            None => return,
        };

        let mut changed_text_for_index: Option<String> = None;

        let ok = self.workspace.documents.with_doc_mut(uri, |doc| {
            let new_version = params.text_document.version;
            let mut text: String = doc.source().text().to_owned();
            let mut rope: ropey::Rope = (*doc.source().rope).clone();

            let base_tree: Option<tree_sitter::Tree> = match &doc.source().tree {
                Some(t) => Some((**t).clone()),
                None => {
                    let mut p = lang.make_parser();
                    p.parse(&text, None)
                }
            };
            let Some(mut tree) = base_tree else {
                return;
            };

            for ch in &params.content_changes {
                let Some(range) = ch.range else {
                    text = ch.text.clone();
                    let mut p = lang.make_parser();
                    let new_tree = p.parse(&text, None);
                    let uri_c = doc.source().uri.as_ref().clone();
                    let lid_c = doc.source().language_id.clone();
                    doc.update_source(crate::workspace::SourceFile::new(
                        uri_c,
                        lid_c.as_ref(),
                        new_version,
                        text.as_str(),
                        new_tree,
                    ));
                    changed_text_for_index = Some(doc.source().text().to_owned());
                    return;
                };

                let start_byte =
                    match rope_line_col_to_offset(&rope, range.start.line, range.start.character) {
                        Some(x) => x,
                        None => continue,
                    };
                let old_end_byte =
                    match rope_line_col_to_offset(&rope, range.end.line, range.end.character) {
                        Some(x) => x,
                        None => continue,
                    };

                let start_line = range.start.line as usize;
                let end_line = range.end.line as usize;
                let start_line_byte = rope.line_to_byte(start_line);
                let end_line_byte = rope.line_to_byte(end_line);
                let start_position =
                    Point::new(start_line, start_byte.saturating_sub(start_line_byte));
                let old_end_position =
                    Point::new(end_line, old_end_byte.saturating_sub(end_line_byte));

                text.replace_range(start_byte..old_end_byte, &ch.text);

                let start_char = rope.byte_to_char(start_byte);
                let old_end_char = rope.byte_to_char(old_end_byte);
                rope.remove(start_char..old_end_char);
                rope.insert(start_char, &ch.text);

                let new_end_byte = start_byte + ch.text.len();
                let (new_end_row, new_end_col_bytes) =
                    point_after_insert_bytes(start_position.row, start_position.column, &ch.text);
                let new_end_position = Point::new(new_end_row, new_end_col_bytes);

                tree.edit(&InputEdit {
                    start_byte,
                    old_end_byte,
                    new_end_byte,
                    start_position,
                    old_end_position,
                    new_end_position,
                });
            }

            let mut parser = lang.make_parser();
            let new_tree = parser.parse(&text, Some(&tree));
            let uri_c = doc.source().uri.as_ref().clone();
            let lid_c = doc.source().language_id.clone();
            doc.update_source(crate::workspace::SourceFile::new(
                uri_c,
                lid_c.as_ref(),
                new_version,
                text.as_str(),
                new_tree,
            ));
            changed_text_for_index = Some(doc.source().text().to_owned());
        });
        if ok.is_none() {
            return;
        }

        let Some(content) = changed_text_for_index else {
            return;
        };

        // Update Salsa database with the new content
        let salsa_file = self
            .workspace
            .get_or_create_salsa_file(uri, &content, &lang_id);

        let uri_str = uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(uri);

        // Use Salsa queries for incremental parsing
        // Salsa automatically invalidates dependent queries when content changes
        let classes = {
            let db = self.workspace.salsa_db.lock();

            // Trigger parse query (will recompute if content changed)
            let _result = crate::salsa_queries::index::extract_classes(&*db, salsa_file);

            // Get the actual classes
            crate::salsa_queries::index::get_extracted_classes(&*db, salsa_file)
        };

        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            class_count = classes.len(),
            language = lang.id(),
            "did_change indexing with Salsa (incremental)"
        );
        self.workspace.index.write().update_source_in_context(
            analysis.module,
            analysis.source_root,
            origin,
            classes,
        );

        self.client.semantic_tokens_refresh().await.ok();
        self.notify_build_file_change(uri).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = &params.text_document.uri;

        let Some(lang_id) = self
            .workspace
            .documents
            .with_doc(uri, |d| d.language_id().to_owned())
        else {
            return;
        };

        let lang = match self.registry.find(&lang_id) {
            Some(l) => l,
            None => return,
        };

        let mut content_for_index: Option<String> = None;

        self.workspace.documents.with_doc_mut(uri, |doc| {
            if let Some(text) = params.text.as_ref() {
                // TODO: should we use increment parsing here?
                // 规范：如果 didSave 携带 text，以它为准（可能与内存不同步）
                let mut parser = lang.make_parser();
                let new_tree = parser.parse(text.as_str(), None);
                let uri_c = doc.source().uri.as_ref().clone();
                let lid_c = doc.source().language_id.clone();
                let ver = doc.version();
                doc.update_source(crate::workspace::SourceFile::new(
                    uri_c,
                    lid_c.as_ref(),
                    ver,
                    text.as_str(),
                    new_tree,
                ));
                content_for_index = Some(doc.source().text().to_owned());
                return;
            }

            // No text payload: just re-parse the current source.
            let cur_text = doc.source().text().to_owned();
            let mut parser = lang.make_parser();
            let new_tree = parser.parse(&cur_text, None);
            doc.set_tree(new_tree);
            content_for_index = Some(cur_text);
        });

        let Some(content) = content_for_index else {
            return;
        };

        let uri_str = uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(uri);
        let name_table = self
            .workspace
            .index
            .read()
            .build_name_table_for_analysis_context(
                analysis.module,
                analysis.classpath,
                analysis.source_root,
            );
        let visible_classpath = self
            .workspace
            .index
            .read()
            .module_classpath_jars(analysis.module, analysis.classpath);
        let classes = index_source_text(&uri_str, &content, &lang_id, Some(name_table));
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            visible_classpath_len = visible_classpath.len(),
            "did_save indexing with analysis context"
        );
        self.workspace.index.write().update_source_in_context(
            analysis.module,
            analysis.source_root,
            origin,
            classes,
        );

        self.client.semantic_tokens_refresh().await.ok();
        self.notify_build_file_change(uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;
        info!(uri = %uri, "did_close");
        self.workspace.documents.close(uri);
        // Remove from Salsa database
        self.workspace.remove_salsa_file(uri);
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let paths = params
            .changes
            .into_iter()
            .filter_map(|event| event.uri.to_file_path().ok())
            .collect::<Vec<_>>();
        let services = self.build_services.read().await.clone();
        for service in services {
            service.notify_paths_changed(paths.clone()).await;
        }
    }

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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LspResult<Option<GotoDefinitionResponse>> {
        info!("LSP goto_definition request received");

        let result = handle_goto_definition(self, params).await;

        if result.is_none() {
            tracing::warn!("Goto definition could not resolve any target");
        }

        Ok(result)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LspResult<Option<SemanticTokensResult>> {
        let response = handle_semantic_tokens(
            Arc::clone(&self.registry),
            Arc::clone(&self.workspace),
            params,
        )
        .await;
        Ok(response)
    }

    async fn semantic_tokens_full_delta(
        &self,
        params: SemanticTokensDeltaParams,
    ) -> LspResult<Option<SemanticTokensFullDeltaResult>> {
        let response = handle_semantic_tokens_full_delta(
            Arc::clone(&self.registry),
            Arc::clone(&self.workspace),
            params,
        )
        .await;
        Ok(response)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> LspResult<Option<SemanticTokensRangeResult>> {
        let response = handle_semantic_tokens_range(
            Arc::clone(&self.registry),
            Arc::clone(&self.workspace),
            params,
        )
        .await;
        Ok(response)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> LspResult<Option<DocumentSymbolResponse>> {
        let response = super::handlers::symbols::handle_document_symbol(
            self.registry.clone(),
            self.workspace.clone(),
            params,
        )
        .await;
        Ok(response)
    }
}

async fn with_progress<F, Fut>(client: &Client, token: &str, title: &str, f: F)
where
    F: FnOnce() -> Fut,
    Fut: std::future::Future<Output = ()>,
{
    // 创建进度 token
    let token = NumberOrString::String(token.to_string());
    client
        .send_request::<tower_lsp::lsp_types::request::WorkDoneProgressCreate>(
            WorkDoneProgressCreateParams {
                token: token.clone(),
            },
        )
        .await
        .ok();

    // Begin
    client
        .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
            token: token.clone(),
            value: ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(WorkDoneProgressBegin {
                title: title.to_string(),
                cancellable: Some(false),
                message: None,
                percentage: None,
            })),
        })
        .await;

    f().await;

    // End
    client
        .send_notification::<tower_lsp::lsp_types::notification::Progress>(ProgressParams {
            token,
            value: ProgressParamsValue::WorkDone(WorkDoneProgress::End(WorkDoneProgressEnd {
                message: None,
            })),
        })
        .await;
}

fn point_after_insert_bytes(
    start_row: usize,
    start_col_bytes: usize,
    inserted: &str,
) -> (usize, usize) {
    if inserted.is_empty() {
        return (start_row, start_col_bytes);
    }

    // tree-sitter Point.column 是“从行首开始的字节数”
    let mut row = start_row;
    let mut col = start_col_bytes;

    // 按 '\n' 分行，column 取最后一行的字节数
    // 注意：这里用 bytes 计数，和 tree-sitter 的定义一致
    if let Some(last_nl) = inserted.rfind('\n') {
        row += inserted.as_bytes().iter().filter(|&&b| b == b'\n').count();
        col = inserted.len() - (last_nl + 1);
    } else {
        col += inserted.len();
    }

    (row, col)
}
