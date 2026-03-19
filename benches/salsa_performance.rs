use criterion::{Criterion, black_box, criterion_group, criterion_main};
use java_analyzer::index::{ClasspathId, ModuleId};
use java_analyzer::salsa_db::{Database, FileId, SourceFile};
use java_analyzer::salsa_queries;
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

fn bench_name_table_cold(c: &mut Criterion) {
    c.bench_function("name_table_cold", |b| {
        b.iter(|| {
            let db = Database::default();
            let module_id = ModuleId::ROOT;
            let classpath = ClasspathId::Main;
            let source_root = None;

            black_box(salsa_queries::cached_name_table(
                &db,
                module_id,
                classpath,
                source_root,
            ))
        })
    });
}

fn bench_name_table_cached(c: &mut Criterion) {
    c.bench_function("name_table_cached", |b| {
        let db = Database::default();
        let module_id = ModuleId::ROOT;
        let classpath = ClasspathId::Main;
        let source_root = None;

        // Warm up the cache
        salsa_queries::cached_name_table(&db, module_id, classpath, source_root);

        b.iter(|| {
            black_box(salsa_queries::cached_name_table(
                &db,
                module_id,
                classpath,
                source_root,
            ))
        })
    });
}

fn bench_get_name_table(c: &mut Criterion) {
    c.bench_function("get_name_table_for_context", |b| {
        let db = Database::default();
        let module_id = ModuleId::ROOT;
        let classpath = ClasspathId::Main;
        let source_root = None;

        b.iter(|| {
            black_box(salsa_queries::get_name_table_for_context(
                &db,
                module_id,
                classpath,
                source_root,
            ))
        })
    });
}

fn bench_class_extraction(c: &mut Criterion) {
    c.bench_function("extract_classes", |b| {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package com.example;\npublic class Test { void foo() {} }".to_string(),
            Arc::from("java"),
        );

        b.iter(|| black_box(salsa_queries::extract_classes(&db, file)))
    });
}

fn bench_class_extraction_cached(c: &mut Criterion) {
    c.bench_function("extract_classes_cached", |b| {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "package com.example;\npublic class Test { void foo() {} }".to_string(),
            Arc::from("java"),
        );

        // Warm up the cache
        salsa_queries::extract_classes(&db, file);

        b.iter(|| black_box(salsa_queries::extract_classes(&db, file)))
    });
}

criterion_group!(
    benches,
    bench_name_table_cold,
    bench_name_table_cached,
    bench_get_name_table,
    bench_class_extraction,
    bench_class_extraction_cached
);
criterion_main!(benches);
