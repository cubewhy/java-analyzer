use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::Duration;

use java_analyzer::completion::candidate::{CandidateKind, CompletionCandidate};
use java_analyzer::completion::engine::CompletionEngine;
use java_analyzer::completion::provider::{CompletionProvider, ProviderCompletionResult};
use java_analyzer::index::IndexScope;
use java_analyzer::language::LanguageRegistry;
use java_analyzer::lsp::handlers::completion::handle_completion;
use java_analyzer::lsp::request_cancellation::{RequestCancellationManager, RequestFamily};
use java_analyzer::lsp::request_context::RequestContext;
use java_analyzer::semantic::{CursorLocation, SemanticContext};
use java_analyzer::workspace::Workspace;
use java_analyzer::workspace::document::Document;
use tokio::sync::Notify;
use tower_lsp::jsonrpc::ErrorCode;
use tower_lsp::lsp_types::*;

fn create_test_workspace() -> Arc<Workspace> {
    Arc::new(Workspace::new())
}

async fn open_document(workspace: &Arc<Workspace>, uri: &Url, content: &str) {
    let doc = Document::new(java_analyzer::workspace::SourceFile::new(
        uri.clone(),
        "java",
        1,
        content,
        None,
    ));
    workspace.documents.open(doc);

    let registry = LanguageRegistry::new();
    let lang = registry.find("java").expect("java language");
    let mut parser = lang.make_parser();
    let tree = parser.parse(content, None);
    workspace.documents.with_doc_mut(uri, |doc| {
        doc.set_tree(tree);
    });

    let salsa_file = workspace
        .get_or_update_salsa_file(uri)
        .expect("opened document should have a salsa file");
    let classes = {
        let db = workspace.salsa_db.lock();
        let _result = java_analyzer::salsa_queries::index::extract_classes(&*db, salsa_file);
        java_analyzer::salsa_queries::index::get_extracted_classes(&*db, salsa_file)
    };
    let analysis = workspace.analysis_context_for_uri(uri);
    let origin = java_analyzer::index::ClassOrigin::SourceFile(Arc::from(uri.as_str()));
    workspace.index.update(|index| {
        index.update_source_in_context(analysis.module, analysis.source_root, origin, classes);
    });
}

fn completion_params(uri: Url) -> CompletionParams {
    CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position::new(4, 11),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    }
}

fn begin_completion_request(
    manager: &Arc<RequestCancellationManager>,
    uri: &Url,
    request_kind: &'static str,
) -> (
    java_analyzer::lsp::request_cancellation::RequestGuard,
    Arc<RequestContext>,
) {
    let guard = manager.begin(RequestFamily::Completion, uri);
    let request = RequestContext::new(
        request_kind,
        uri,
        RequestFamily::Completion,
        guard.generation(),
        guard.token().clone(),
    );
    (guard, request)
}

struct BlockingProvider {
    started: Arc<Notify>,
    call_count: Arc<AtomicUsize>,
    cancelled_observed: Arc<AtomicBool>,
}

impl CompletionProvider for BlockingProvider {
    fn name(&self) -> &'static str {
        "blocking-test"
    }

    fn is_applicable(&self, ctx: &SemanticContext) -> bool {
        matches!(ctx.location, CursorLocation::Expression { .. })
    }

    fn provide(
        &self,
        _scope: IndexScope,
        _ctx: &SemanticContext,
        _index: &java_analyzer::index::IndexView,
        request: Option<&RequestContext>,
        _limit: Option<usize>,
    ) -> java_analyzer::lsp::request_cancellation::RequestResult<ProviderCompletionResult> {
        let call_index = self.call_count.fetch_add(1, Ordering::SeqCst);
        if call_index == 0 {
            self.started.notify_one();
            loop {
                if let Some(request) = request
                    && let Err(reason) = request.check_cancelled("test.blocking_provider")
                {
                    self.cancelled_observed.store(true, Ordering::SeqCst);
                    return Err(reason);
                }
                std::thread::sleep(Duration::from_millis(5));
            }
        }

        Ok(ProviderCompletionResult::from(vec![
            CompletionCandidate::new(
                Arc::from("TestCompletion"),
                "TestCompletion",
                CandidateKind::Keyword,
                self.name(),
            ),
        ]))
    }
}

fn build_engine(
    started: Arc<Notify>,
    call_count: Arc<AtomicUsize>,
    cancelled_observed: Arc<AtomicBool>,
) -> Arc<CompletionEngine> {
    let mut engine = CompletionEngine::new();
    engine.register_provider(Box::new(BlockingProvider {
        started,
        call_count,
        cancelled_observed,
    }));
    Arc::new(engine)
}

#[tokio::test]
async fn dropping_guard_cancels_inflight_completion() {
    let workspace = create_test_workspace();
    let registry = Arc::new(LanguageRegistry::new());
    let uri = Url::parse("file:///test/Main.java").expect("uri");
    let content = r#"
class Main {
    void test() {
        String value = "";
        val
    }
}
"#;
    open_document(&workspace, &uri, content).await;

    let started = Arc::new(Notify::new());
    let call_count = Arc::new(AtomicUsize::new(0));
    let cancelled_observed = Arc::new(AtomicBool::new(false));
    let engine = build_engine(
        Arc::clone(&started),
        Arc::clone(&call_count),
        Arc::clone(&cancelled_observed),
    );
    let manager = Arc::new(RequestCancellationManager::new());

    let (guard, request) = begin_completion_request(&manager, &uri, "completion");
    let task = tokio::spawn(handle_completion(
        Arc::clone(&workspace),
        Arc::clone(&engine),
        Arc::clone(&registry),
        completion_params(uri.clone()),
        request,
    ));

    tokio::time::timeout(Duration::from_secs(1), started.notified())
        .await
        .expect("provider started");
    drop(guard);

    let response = task.await.expect("completion join");
    let error = response.expect_err("request should be cancelled");
    assert_eq!(error.code, ErrorCode::RequestCancelled);
    assert!(cancelled_observed.load(Ordering::SeqCst));
}

#[tokio::test]
async fn newer_completion_supersedes_older_request() {
    let workspace = create_test_workspace();
    let registry = Arc::new(LanguageRegistry::new());
    let uri = Url::parse("file:///test/Main.java").expect("uri");
    let content = r#"
class Main {
    void test() {
        String value = "";
        val
    }
}
"#;
    open_document(&workspace, &uri, content).await;

    let started = Arc::new(Notify::new());
    let call_count = Arc::new(AtomicUsize::new(0));
    let cancelled_observed = Arc::new(AtomicBool::new(false));
    let engine = build_engine(
        Arc::clone(&started),
        Arc::clone(&call_count),
        Arc::clone(&cancelled_observed),
    );
    let manager = Arc::new(RequestCancellationManager::new());

    let (first_guard, first_request) = begin_completion_request(&manager, &uri, "completion");
    let first_task = tokio::spawn(handle_completion(
        Arc::clone(&workspace),
        Arc::clone(&engine),
        Arc::clone(&registry),
        completion_params(uri.clone()),
        first_request,
    ));

    tokio::time::timeout(Duration::from_secs(1), started.notified())
        .await
        .expect("provider started");

    let (second_guard, second_request) = begin_completion_request(&manager, &uri, "completion");
    let second_response = handle_completion(
        Arc::clone(&workspace),
        Arc::clone(&engine),
        Arc::clone(&registry),
        completion_params(uri.clone()),
        second_request,
    )
    .await
    .expect("second request should finish");

    let first_response = first_task.await.expect("first completion join");
    let first_error = first_response.expect_err("first request should be cancelled");

    assert_eq!(first_error.code, ErrorCode::RequestCancelled);
    assert!(cancelled_observed.load(Ordering::SeqCst));
    assert!(second_response.is_some(), "second request should complete");

    first_guard.finish();
    second_guard.finish();
}
