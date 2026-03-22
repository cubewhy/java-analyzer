use std::sync::{Arc, mpsc};
use std::time::Duration;

use java_analyzer::index::{IndexScope, ModuleId, WorkspaceIndex, WorkspaceIndexHandle};
use java_analyzer::salsa_db::{Database, FileId, SourceFile};
use java_analyzer::salsa_queries::java::extract_java_semantic_context_at_offset;
use tower_lsp::lsp_types::Url;

#[test]
fn semantic_context_build_does_not_block_on_serialized_index_write() {
    let workspace_index = WorkspaceIndexHandle::new(WorkspaceIndex::new());
    let source = r#"
class Test {
    void demo() {
        int value = 1;
        val
    }
}
"#
    .to_string();
    let offset = source.find("val").expect("completion marker") + "val".len();
    let (ready_tx, ready_rx) = mpsc::channel();

    let writer = std::thread::spawn({
        let workspace_index = workspace_index.clone();
        move || {
            workspace_index.update(|_| {
                ready_tx
                    .send(())
                    .expect("writer should signal when the serialized update begins");
                std::thread::sleep(Duration::from_millis(150));
            });
        }
    });

    ready_rx
        .recv_timeout(Duration::from_secs(1))
        .expect("writer should enter the serialized update path");

    let db = Database::with_workspace_index(workspace_index.clone());
    let file = SourceFile::new(
        &db,
        FileId::new(Url::parse("file:///test/Test.java").expect("uri")),
        source,
        Arc::from("java"),
    );
    let view = workspace_index.load().view(IndexScope {
        module: ModuleId::ROOT,
    });
    let completed =
        extract_java_semantic_context_at_offset(&db, file, offset, view, None).is_some();
    writer
        .join()
        .expect("writer thread should finish after the read completes");

    assert!(
        completed,
        "semantic context extraction should still produce a context"
    );
}
