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

        // 用 registry 判断是否支持
        let lang = match self.registry.find(&td.language_id) {
            Some(l) => l,
            None => return,
        };

        info!(uri = %td.uri, lang = %td.language_id, "did_open");

        // 先存 document（Document::new 需要是 text+rope+tree=None 的新结构）
        self.workspace.documents.open(Document::new(
            td.uri.clone(),
            td.language_id.clone(),
            td.version,
            td.text.clone(),
        ));

        // 立刻 parse 一次，缓存 tree（避免 completion/semantic_tokens 每次 parse）
        let mut parser = lang.make_parser();
        let tree = parser.parse(&td.text, None);

        self.workspace.documents.with_doc_mut(&td.uri, |doc| {
            doc.tree = tree;
        });

        let uri_str = td.uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(&td.uri);
        let name_table = self
            .workspace
            .index
            .read()
            .await
            .build_name_table_for_analysis_context(
                analysis.module,
                analysis.classpath,
                analysis.source_root,
            );
        let visible_classpath = self
            .workspace
            .index
            .read()
            .await
            .module_classpath_jars(analysis.module, analysis.classpath);
        let classes = index_source_text(&uri_str, &td.text, &td.language_id, Some(name_table));
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %td.uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            visible_classpath_len = visible_classpath.len(),
            "did_open indexing with analysis context"
        );
        self.workspace.index.write().await.update_source_in_context(
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

        // 只处理已打开文档
        let Some(lang_id) = self
            .workspace
            .documents
            .with_doc(uri, |d| d.language_id.clone())
        else {
            return;
        };

        let lang = match self.registry.find(&lang_id) {
            Some(l) => l,
            None => return,
        };

        // 在 doc 上完成：应用 edits -> tree.edit -> parse(Some(old))
        // 注意：闭包里不能 await
        let mut changed_text_for_index: Option<String> = None;

        let ok = self.workspace.documents.with_doc_mut(uri, |doc| {
            // 版本更新
            doc.version = params.text_document.version;

            // 如果 tree 还没有（比如之前没 parse 成功），先 full parse 一次兜底
            if doc.tree.is_none() {
                let mut parser = lang.make_parser();
                doc.tree = parser.parse(&doc.text, None);
            }

            // 必须有 tree 才能增量
            let Some(old_tree) = doc.tree.as_ref() else {
                // 解析失败就只能退化：把 change 当 full 文本替换（这里按协议一般不会发生）
                return;
            };

            let mut tree = old_tree.clone();

            // 逐个应用 change（INCREMENTAL 可能一次发多个）
            for ch in &params.content_changes {
                let Some(range) = ch.range else {
                    // 客户端可能发 full text（range=None），那就退化成 full replace + full parse
                    doc.text = ch.text.clone();
                    doc.rope = ropey::Rope::from_str(&doc.text);

                    let mut parser = lang.make_parser();
                    doc.tree = parser.parse(&doc.text, None);
                    changed_text_for_index = Some(doc.text.clone());
                    return;
                };

                // 1) 旧 rope 上算 byte offsets（你已有 rope_line_col_to_offset）
                let start_byte = match rope_line_col_to_offset(
                    &doc.rope,
                    range.start.line,
                    range.start.character,
                ) {
                    Some(x) => x,
                    None => continue,
                };
                let old_end_byte =
                    match rope_line_col_to_offset(&doc.rope, range.end.line, range.end.character) {
                        Some(x) => x,
                        None => continue,
                    };

                // 2) old positions (tree-sitter Point 的 column 用“字节列”)
                let start_line = range.start.line as usize;
                let end_line = range.end.line as usize;

                let start_line_byte = doc.rope.line_to_byte(start_line);
                let end_line_byte = doc.rope.line_to_byte(end_line);

                let start_position =
                    Point::new(start_line, start_byte.saturating_sub(start_line_byte));
                let old_end_position =
                    Point::new(end_line, old_end_byte.saturating_sub(end_line_byte));

                // 3) 更新 doc.text（byte range）
                doc.text.replace_range(start_byte..old_end_byte, &ch.text);

                // 4) 更新 rope（char range）
                let start_char = doc.rope.byte_to_char(start_byte);
                let old_end_char = doc.rope.byte_to_char(old_end_byte);
                doc.rope.remove(start_char..old_end_char);
                doc.rope.insert(start_char, &ch.text);

                // 5) new end byte / new end point（按插入文本计算）
                let new_end_byte = start_byte + ch.text.len();
                let (new_end_row, new_end_col_bytes) =
                    point_after_insert_bytes(start_position.row, start_position.column, &ch.text);
                let new_end_position = Point::new(new_end_row, new_end_col_bytes);

                // 6) tree.edit
                tree.edit(&InputEdit {
                    start_byte,
                    old_end_byte,
                    new_end_byte,
                    start_position,
                    old_end_position,
                    new_end_position,
                });
            }

            // 7) incremental parse（复用 edited old tree）
            let mut parser = lang.make_parser();
            let new_tree = parser.parse(&doc.text, Some(&tree));
            doc.tree = new_tree;

            changed_text_for_index = Some(doc.text.clone());
        });

        if ok.is_none() {
            return;
        }

        // 下面可以 await：更新索引 + refresh
        let Some(content) = changed_text_for_index else {
            return;
        };

        let uri_str = uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(uri);
        let name_table = self
            .workspace
            .index
            .read()
            .await
            .build_name_table_for_analysis_context(
                analysis.module,
                analysis.classpath,
                analysis.source_root,
            );
        let visible_classpath = self
            .workspace
            .index
            .read()
            .await
            .module_classpath_jars(analysis.module, analysis.classpath);
        let classes = index_source_text(&uri_str, &content, &lang_id, Some(name_table));
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            visible_classpath_len = visible_classpath.len(),
            "did_change indexing with analysis context"
        );
        self.workspace.index.write().await.update_source_in_context(
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
            .with_doc(uri, |d| d.language_id.clone())
        else {
            return;
        };

        let lang = match self.registry.find(&lang_id) {
            Some(l) => l,
            None => return,
        };

        // 在 doc 内更新内容 + rope + tree（闭包内不能 await）
        // 最终用于索引更新的内容
        let mut content_for_index: Option<String> = None;

        self.workspace.documents.with_doc_mut(uri, |doc| {
            if let Some(text) = params.text.as_ref() {
                // 规范：如果 didSave 携带 text，以它为准（可能与内存不同步）
                doc.text = text.clone();
                doc.rope = ropey::Rope::from_str(&doc.text);
            }

            // 保存时重建树：稳定可靠（不依赖 edit ranges）
            let mut parser = lang.make_parser();
            doc.tree = parser.parse(&doc.text, None);

            content_for_index = Some(doc.text.clone());
        });

        let Some(content) = content_for_index else {
            return;
        };

        // 重新索引（你原有逻辑）
        let uri_str = uri.to_string();
        let analysis = self.workspace.analysis_context_for_uri(uri);
        let name_table = self
            .workspace
            .index
            .read()
            .await
            .build_name_table_for_analysis_context(
                analysis.module,
                analysis.classpath,
                analysis.source_root,
            );
        let visible_classpath = self
            .workspace
            .index
            .read()
            .await
            .module_classpath_jars(analysis.module, analysis.classpath);
        let classes = index_source_text(&uri_str, &content, &lang_id, Some(name_table));
        let origin = ClassOrigin::SourceFile(Arc::from(uri_str.as_str()));
        tracing::debug!(
            uri = %uri,
            module = analysis.module.0,
            classpath = ?analysis.classpath,
            source_root = ?analysis.source_root.map(|id| id.0),
            visible_classpath_len = visible_classpath.len(),
            asm_visible = visible_classpath.iter().any(|jar| jar.contains("asm-")),
            "did_save indexing with analysis context"
        );
        self.workspace.index.write().await.update_source_in_context(
            analysis.module,
            analysis.source_root,
            origin,
            classes,
        );

        // 刷新语义高亮
        self.client.semantic_tokens_refresh().await.ok();
        self.notify_build_file_change(uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = &params.text_document.uri;
        info!(uri = %uri, "did_close");
        self.workspace.documents.close(uri);
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
