use anyhow::Result;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use tokio::sync::{RwLock, watch};
use tower_lsp::lsp_types::Url;
use tracing::info;

use crate::build_integration::{SourceRootId, WorkspaceModelSnapshot, WorkspaceRootKind};
use crate::index::codebase::{
    SourceScanMode, collect_source_files, collect_source_files_for_root, load_source_inputs,
    should_index_source_path,
};
use crate::index::incremental::SourceTextInput;
use crate::index::{
    ClassMetadata, ClassOrigin, ClasspathId, IndexScope, IndexedArchiveData, IndexedJavaModule,
    ModuleId, WorkspaceIndex, WorkspaceIndexHandle,
};
use crate::language::Language;
use crate::language::java::module_info::JavaModuleDescriptor;
use crate::salsa_db::{Database as SalsaDatabase, FileId};
use crate::salsa_queries::Db;
use crate::salsa_queries::semantic::CachedMethodLocal;
use crate::semantic::context::CurrentClassMember;
use document::DocumentStore;

pub mod document;

/// Cache for parsed semantic data (IntelliJ-style PSI cache)
///
/// This stores parsed locals and class members keyed by content hash.
/// When file content changes, the hash changes and cache is automatically invalidated.
#[derive(Default)]
struct SemanticCache {
    /// Cached parsed method locals per method, keyed by content hash
    method_locals: HashMap<u64, Vec<CachedMethodLocal>>,
    /// Cached class members per class, keyed by content hash
    class_members: HashMap<u64, Vec<CurrentClassMember>>,
}

#[derive(Default)]
struct JavaModuleRegistry {
    by_uri: HashMap<Url, Arc<JavaModuleDescriptor>>,
    by_name: HashMap<Arc<str>, Vec<Url>>,
}

impl JavaModuleRegistry {
    fn rebuild_name_index(&mut self) {
        self.by_name.clear();
        for (uri, descriptor) in &self.by_uri {
            self.by_name
                .entry(Arc::clone(&descriptor.name))
                .or_default()
                .push(uri.clone());
        }
    }

    fn upsert(&mut self, uri: Url, descriptor: Arc<JavaModuleDescriptor>) {
        self.by_uri.insert(uri, descriptor);
        self.rebuild_name_index();
    }

    fn remove(&mut self, uri: &Url) {
        if self.by_uri.remove(uri).is_some() {
            self.rebuild_name_index();
        }
    }

    fn replace_all(&mut self, descriptors: Vec<(Url, Arc<JavaModuleDescriptor>)>) {
        self.by_uri.clear();
        for (uri, descriptor) in descriptors {
            self.by_uri.insert(uri, descriptor);
        }
        self.rebuild_name_index();
    }

    fn module_names(&self) -> Vec<Arc<str>> {
        let mut names = self.by_name.keys().cloned().collect::<Vec<_>>();
        names.sort();
        names
    }

    fn descriptor_for_uri(&self, uri: &Url) -> Option<Arc<JavaModuleDescriptor>> {
        self.by_uri.get(uri).cloned()
    }

    fn first_uri_for_name(&self, name: &str) -> Option<Url> {
        self.by_name
            .get(name)
            .and_then(|uris| uris.first())
            .cloned()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AnalysisContext {
    pub module: ModuleId,
    pub classpath: ClasspathId,
    pub source_root: Option<SourceRootId>,
    pub root_kind: Option<WorkspaceRootKind>,
}

impl AnalysisContext {
    pub fn scope(self) -> IndexScope {
        IndexScope {
            module: self.module,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum JavaModuleTarget {
    Source { uri: Url },
    Bytecode { module: Arc<IndexedJavaModule> },
}

pub struct Workspace {
    pub documents: DocumentStore,
    /// Published workspace state. Reads load the current index snapshot, and
    /// full workspace-model publishes swap index+model together.
    pub index: WorkspaceIndexHandle,
    /// Salsa database for incremental computation
    pub salsa_db: Arc<parking_lot::Mutex<SalsaDatabase>>,
    /// Mapping from URI to Salsa SourceFile input
    salsa_files: Arc<parking_lot::RwLock<HashMap<Url, crate::salsa_db::SourceFile>>>,
    /// File URIs currently managed by workspace/fallback bulk indexing.
    indexed_salsa_uris: Arc<parking_lot::RwLock<HashSet<Url>>>,
    /// Root directory used by fallback indexing and source watching when no
    /// managed workspace model is available.
    workspace_root: Arc<parking_lot::RwLock<Option<PathBuf>>>,
    /// IntelliJ-style semantic cache for parsed locals and members
    /// Keyed by content hash, automatically invalidated when content changes
    semantic_cache: Arc<parking_lot::RwLock<SemanticCache>>,
    java_modules: Arc<parking_lot::RwLock<JavaModuleRegistry>>,
    jdk_classes: RwLock<Vec<ClassMetadata>>,
    jdk_modules: RwLock<Vec<IndexedJavaModule>>,
    full_reindex_in_progress: Arc<AtomicUsize>,
    full_reindex_serial: Arc<AtomicUsize>,
    watched_roots_tx: watch::Sender<Vec<WatchedSourceRoot>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WatchedSourceRoot {
    pub path: PathBuf,
    pub scan_mode: SourceScanMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FilesystemChangeKind {
    Upsert,
    Remove,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FilesystemChange {
    pub path: PathBuf,
    pub kind: FilesystemChangeKind,
}

impl FilesystemChange {
    pub(crate) fn upsert(path: PathBuf) -> Self {
        Self {
            path,
            kind: FilesystemChangeKind::Upsert,
        }
    }

    pub(crate) fn remove(path: PathBuf) -> Self {
        Self {
            path,
            kind: FilesystemChangeKind::Remove,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FilesystemApplySummary {
    pub applied: usize,
    pub removed: usize,
    pub skipped_open_documents: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FileApplyState {
    Applied,
    Unchanged,
    SkippedOpenDocument,
    SkippedUntracked,
}

struct FullReindexGuard {
    counter: Arc<AtomicUsize>,
    serial: Arc<AtomicUsize>,
}

impl Drop for FullReindexGuard {
    fn drop(&mut self) {
        self.serial.fetch_add(1, Ordering::AcqRel);
        self.counter.fetch_sub(1, Ordering::Release);
    }
}

impl Workspace {
    pub fn new() -> Self {
        // Create a single WorkspaceIndex handle shared by both async code and Salsa.
        let index = WorkspaceIndexHandle::new(WorkspaceIndex::new());

        // Create Salsa database with the same workspace index reference
        let salsa_db = SalsaDatabase::with_workspace_index(index.clone());
        let (watched_roots_tx, _) = watch::channel(Vec::new());

        Self {
            documents: DocumentStore::new(),
            index,
            salsa_db: Arc::new(parking_lot::Mutex::new(salsa_db)),
            salsa_files: Arc::new(parking_lot::RwLock::new(HashMap::new())),
            indexed_salsa_uris: Arc::new(parking_lot::RwLock::new(HashSet::new())),
            workspace_root: Arc::new(parking_lot::RwLock::new(None)),
            semantic_cache: Arc::new(parking_lot::RwLock::new(SemanticCache::default())),
            java_modules: Arc::new(parking_lot::RwLock::new(JavaModuleRegistry::default())),
            jdk_classes: RwLock::new(Vec::new()),
            jdk_modules: RwLock::new(Vec::new()),
            full_reindex_in_progress: Arc::new(AtomicUsize::new(0)),
            full_reindex_serial: Arc::new(AtomicUsize::new(0)),
            watched_roots_tx,
        }
    }

    fn begin_full_reindex(&self) -> FullReindexGuard {
        self.full_reindex_in_progress.fetch_add(1, Ordering::AcqRel);
        self.full_reindex_serial.fetch_add(1, Ordering::AcqRel);
        FullReindexGuard {
            counter: Arc::clone(&self.full_reindex_in_progress),
            serial: Arc::clone(&self.full_reindex_serial),
        }
    }

    pub fn full_reindex_in_progress(&self) -> bool {
        self.full_reindex_in_progress.load(Ordering::Acquire) != 0
    }

    pub fn full_reindex_serial(&self) -> usize {
        self.full_reindex_serial.load(Ordering::Acquire)
    }

    pub fn set_workspace_root(&self, root: PathBuf) {
        *self.workspace_root.write() = Some(root);
        self.publish_watched_source_roots();
    }

    pub fn workspace_root(&self) -> Option<PathBuf> {
        self.workspace_root.read().clone()
    }

    pub(crate) fn subscribe_watched_source_roots(&self) -> watch::Receiver<Vec<WatchedSourceRoot>> {
        self.watched_roots_tx.subscribe()
    }

    /// Get cached method locals by content hash (IntelliJ-style PSI cache)
    pub fn get_cached_method_locals(&self, content_hash: u64) -> Option<Vec<CachedMethodLocal>> {
        self.semantic_cache
            .read()
            .method_locals
            .get(&content_hash)
            .cloned()
    }

    /// Cache parsed method locals by content hash
    pub fn cache_method_locals(&self, content_hash: u64, locals: Vec<CachedMethodLocal>) {
        self.semantic_cache
            .write()
            .method_locals
            .insert(content_hash, locals);
    }

    /// Get cached class members by content hash (IntelliJ-style PSI cache)
    pub fn get_cached_class_members(&self, content_hash: u64) -> Option<Vec<CurrentClassMember>> {
        self.semantic_cache
            .read()
            .class_members
            .get(&content_hash)
            .cloned()
    }

    /// Cache class members by content hash
    pub fn cache_class_members(&self, content_hash: u64, members: Vec<CurrentClassMember>) {
        self.semantic_cache
            .write()
            .class_members
            .insert(content_hash, members);
    }

    /// Get or create a Salsa SourceFile for a URI string
    ///
    /// This is used by incremental parsing to get a Salsa file handle.
    /// Files are cached so repeated calls return the same SourceFile.
    pub fn get_or_create_salsa_file_by_uri_str(
        &self,
        uri: &str,
    ) -> Option<crate::salsa_db::SourceFile> {
        let url = Url::parse(uri).ok()?;
        self.get_or_update_salsa_file(&url)
    }

    pub fn document_snapshot(&self, uri: &Url) -> Option<Arc<SourceFile>> {
        self.documents.with_doc(uri, |doc| doc.snapshot())
    }

    pub fn ensure_tree(&self, uri: &Url, lang: &dyn Language) -> Option<Arc<SourceFile>> {
        let has_tree = self
            .documents
            .with_doc(uri, |doc| doc.source().tree.is_some())
            .unwrap_or(false);

        if !has_tree {
            self.documents.with_doc_mut(uri, |doc| {
                if doc.source().tree.is_some() {
                    return;
                }
                let tree = lang.parse_tree(doc.source().text(), None);
                doc.set_tree(tree);
            });
        }

        self.document_snapshot(uri)
    }

    /// Get or update a Salsa SourceFile for a URI
    ///
    /// This ensures the Salsa file is synchronized with the document content.
    /// If the file exists but content has changed, it updates the Salsa file.
    pub fn get_or_update_salsa_file(&self, uri: &Url) -> Option<crate::salsa_db::SourceFile> {
        let source = self.document_snapshot(uri)?;
        Some(self.get_or_update_salsa_file_for_snapshot(source.as_ref()))
    }

    pub fn get_or_update_salsa_file_for_snapshot(
        &self,
        source: &SourceFile,
    ) -> crate::salsa_db::SourceFile {
        // Check if file exists
        {
            let files = self.salsa_files.read();
            if let Some(&file) = files.get(source.uri.as_ref()) {
                let in_sync = {
                    let db = self.salsa_db.lock();
                    file.content(&*db).as_str() == source.text()
                        && file.language_id(&*db).as_ref() == source.language_id.as_ref()
                };

                if in_sync {
                    // Already in sync
                    return file;
                }

                // Update the existing Salsa input to match the current document snapshot.
                drop(files); // Release read lock
                self.update_existing_salsa_file_from_source(file, source);
                self.seed_salsa_parse_tree_from_source(file, source);
                return file;
            }
        }

        // Create new file
        let file = self.create_salsa_file_from_source(source);
        self.salsa_files
            .write()
            .insert(source.uri.as_ref().clone(), file);
        self.seed_salsa_parse_tree_from_source(file, source);
        file
    }

    pub fn scope_for_uri(&self, uri: &Url) -> IndexScope {
        self.resolve_analysis_context_for_path(uri.to_file_path().ok().as_deref())
            .scope()
    }

    pub(crate) fn load_analysis_state_for_uri(
        &self,
        uri: &Url,
    ) -> (AnalysisContext, Option<Arc<str>>, Arc<WorkspaceIndex>) {
        let (index, model) = self.index.snapshot();
        let analysis = Self::resolve_analysis_context_for_snapshot(
            model.as_ref(),
            uri.to_file_path().ok().as_deref(),
        );
        let inferred_package =
            Self::infer_java_package_for_uri_with_model(uri, analysis.source_root, model.as_ref());
        (analysis, inferred_package, index)
    }

    pub fn infer_java_package_for_uri(
        &self,
        uri: &Url,
        source_root: Option<SourceRootId>,
    ) -> Option<Arc<str>> {
        let (_, model) = self.index.snapshot();
        Self::infer_java_package_for_uri_with_model(uri, source_root, model.as_ref())
    }

    fn infer_java_package_for_uri_with_model(
        uri: &Url,
        source_root: Option<SourceRootId>,
        model: Option<&WorkspaceModelSnapshot>,
    ) -> Option<Arc<str>> {
        let Ok(path) = uri.to_file_path() else {
            tracing::debug!(
                uri = %uri,
                requested_source_root = ?source_root.map(|id| id.0),
                fallback_used = true,
                "java package inference skipped: URI is not a file path"
            );
            return None;
        };

        let Some(model) = model else {
            tracing::debug!(
                uri = %uri,
                path = %path.display(),
                requested_source_root = ?source_root.map(|id| id.0),
                fallback_used = true,
                "java package inference skipped: no managed workspace model"
            );
            return None;
        };

        match model.infer_java_package_for_file(&path, source_root) {
            Some(inference) => {
                tracing::debug!(
                    uri = %uri,
                    path = %path.display(),
                    resolved_source_root_id = inference.source_root_id.0,
                    resolved_source_root = %inference.source_root_path.display(),
                    relative_dir = %inference.relative_dir.display(),
                    inferred_package = %inference.package,
                    fallback_used = false,
                    "java package inference resolved from workspace source root"
                );
                Some(Arc::from(inference.package))
            }
            None => {
                tracing::debug!(
                    uri = %uri,
                    path = %path.display(),
                    requested_source_root = ?source_root.map(|id| id.0),
                    fallback_used = true,
                    "java package inference fell back"
                );
                None
            }
        }
    }

    pub fn analysis_context_for_uri(&self, uri: &Url) -> AnalysisContext {
        let (ctx, _, _) = self.load_analysis_state_for_uri(uri);
        tracing::debug!(
            uri = %uri,
            module = ctx.module.0,
            classpath = ?ctx.classpath,
            source_root = ?ctx.source_root.map(|id| id.0),
            root_kind = ?ctx.root_kind,
            "resolved analysis context for URI"
        );
        if let Ok(path) = uri.to_file_path() {
            tracing::info!(
                uri = %uri,
                path = %path.display(),
                module = ctx.module.0,
                classpath = ?ctx.classpath,
                source_root = ?ctx.source_root.map(|id| id.0),
                root_kind = ?ctx.root_kind,
                "analysis context resolution"
            );
        }
        ctx
    }

    pub async fn current_model(&self) -> Option<WorkspaceModelSnapshot> {
        self.index.current_model()
    }

    pub async fn set_jdk_classes(&self, classes: Vec<ClassMetadata>) {
        *self.jdk_classes.write().await = classes.clone();
        self.jdk_modules.write().await.clear();
        self.index.update(|index| index.add_jdk_classes(classes));
    }

    pub async fn set_jdk_archive(&self, data: IndexedArchiveData) {
        *self.jdk_classes.write().await = data.classes.clone();
        *self.jdk_modules.write().await = data.modules.clone();
        self.index.update(|index| index.add_jdk_archive(data));
    }

    pub async fn apply_workspace_model(&self, snapshot: WorkspaceModelSnapshot) -> Result<()> {
        let _reindex_guard = self.begin_full_reindex();
        self.set_workspace_root(snapshot.root.path.clone());
        let jdk_classes = self.jdk_classes.read().await.clone();
        let jdk_modules = self.jdk_modules.read().await.clone();
        let open_doc_overlays = self
            .documents
            .snapshot_documents()
            .into_iter()
            .map(|(uri, language_id, content)| (uri.to_string(), (language_id, content)))
            .collect::<HashMap<_, _>>();
        let root_inputs = snapshot
            .modules
            .iter()
            .flat_map(|module| {
                module.roots.iter().filter_map(|root| {
                    if matches!(
                        root.kind,
                        WorkspaceRootKind::Sources
                            | WorkspaceRootKind::Tests
                            | WorkspaceRootKind::Generated
                    ) {
                        Some((
                            module.id,
                            root.id,
                            root.classpath,
                            root.path.clone(),
                            root.kind,
                        ))
                    } else {
                        None
                    }
                })
            })
            .collect::<Vec<_>>();

        let indexed_roots = tokio::task::spawn_blocking(move || {
            root_inputs
                .into_iter()
                .map(|(module_id, root_id, classpath, root_path, root_kind)| {
                    let mode = if matches!(root_kind, WorkspaceRootKind::Generated) {
                        SourceScanMode::IncludeGenerated
                    } else {
                        SourceScanMode::Default
                    };
                    let source_files = collect_source_files_for_root(root_path.clone(), mode);
                    let mut source_inputs = load_source_inputs(source_files);
                    overlay_open_document_inputs(&mut source_inputs, &open_doc_overlays);
                    (
                        module_id,
                        root_id,
                        classpath,
                        root_path.clone(),
                        source_inputs,
                    )
                })
                .collect::<Vec<_>>()
        })
        .await?;

        let new_index = WorkspaceIndex::new();
        if !jdk_classes.is_empty() || !jdk_modules.is_empty() {
            new_index.add_jdk_archive(IndexedArchiveData {
                classes: jdk_classes,
                modules: jdk_modules,
            });
        }

        for module in &snapshot.modules {
            let main_roots = module
                .roots
                .iter()
                .filter(|root| root.classpath == ClasspathId::Main)
                .count();
            let test_roots = module
                .roots
                .iter()
                .filter(|root| root.classpath == ClasspathId::Test)
                .count();
            tracing::debug!(
                module = module.id.0,
                name = %module.name,
                roots = module.roots.len(),
                main_roots,
                test_roots,
                compile_classpath = module.compile_classpath.len(),
                test_classpath = module.test_classpath.len(),
                deps = module.dependency_modules.len(),
                "publishing normalized workspace module"
            );
            tracing::info!(
                module = %module.name,
                roots = ?module
                    .roots
                    .iter()
                    .map(|root| format!("{:?}:{:?}:{}", root.kind, root.classpath, root.path.display()))
                    .collect::<Vec<_>>(),
                compile_classpath = ?module
                    .compile_classpath
                    .iter()
                    .map(|path| path.display().to_string())
                    .collect::<Vec<_>>(),
                test_classpath = ?module
                    .test_classpath
                    .iter()
                    .map(|path| path.display().to_string())
                    .collect::<Vec<_>>(),
                dep_modules = ?module.dependency_modules.iter().map(|id| id.0).collect::<Vec<_>>(),
                "normalized workspace module classpath dump"
            );
            new_index.ensure_module(module.id, Arc::from(module.name.clone()));
            new_index.register_module_source_roots(
                module.id,
                module
                    .roots
                    .iter()
                    .filter(|root| {
                        matches!(
                            root.kind,
                            WorkspaceRootKind::Sources
                                | WorkspaceRootKind::Tests
                                | WorkspaceRootKind::Generated
                        )
                    })
                    .map(|root| (root.id, root.classpath))
                    .collect(),
            );

            let main_classpath = merge_classpath(module.compile_classpath.iter())
                .iter()
                .map(|path| Arc::<str>::from(path.to_string_lossy().into_owned()))
                .collect::<Vec<_>>();
            new_index.set_module_classpath(module.id, ClasspathId::Main, main_classpath);

            let test_classpath = merge_classpath(module.test_classpath.iter())
                .iter()
                .map(|path| Arc::<str>::from(path.to_string_lossy().into_owned()))
                .collect::<Vec<_>>();
            new_index.set_module_classpath(module.id, ClasspathId::Test, test_classpath);
            new_index.set_module_dependencies(module.id, module.dependency_modules.clone());
        }

        let mut current_indexed_uris = HashSet::new();
        for (module_id, root_id, classpath, root_path, source_inputs) in indexed_roots {
            let (classes, indexed_uris) =
                self.index_source_inputs_with_shared_salsa(source_inputs, None);
            current_indexed_uris.extend(indexed_uris);
            let mut by_origin: std::collections::HashMap<ClassOrigin, Vec<_>> =
                std::collections::HashMap::new();
            for class in classes {
                by_origin
                    .entry(class.origin.clone())
                    .or_default()
                    .push(class);
            }
            for (origin, classes) in by_origin {
                tracing::debug!(
                    module = module_id.0,
                    source_root = root_id.0,
                    classpath = ?classpath,
                    root = %root_path.display(),
                    class_count = classes.len(),
                    "indexing imported workspace source root"
                );
                new_index.update_source_in_context(module_id, Some(root_id), origin, classes);
            }
        }

        let open_docs = self.documents.snapshot_documents();
        for (uri, language_id, content) in open_docs {
            let context = Self::resolve_analysis_context_for_snapshot(
                Some(&snapshot),
                uri.to_file_path().ok().as_deref(),
            );
            let origin = ClassOrigin::SourceFile(Arc::from(uri.to_string().as_str()));

            // Get or create Salsa file for this document
            let salsa_file = self.get_or_create_salsa_file(&uri, &content, &language_id);

            // Use Salsa queries for incremental parsing
            let classes = {
                let db = self.salsa_db.lock();

                // Trigger parse query (memoized) - this tracks changes
                let _result = crate::salsa_queries::index::extract_classes(&*db, salsa_file);

                self.extract_salsa_classes_for_index_context(
                    &*db, salsa_file, &origin, &new_index, context,
                )
            };

            tracing::debug!(
                uri = %uri,
                module = context.module.0,
                classpath = ?context.classpath,
                source_root = ?context.source_root.map(|id| id.0),
                class_count = classes.len(),
                "reindexing open document against workspace model (using Salsa)"
            );
            new_index.update_source_in_context(
                context.module,
                context.source_root,
                origin,
                classes,
            );
        }

        self.rebuild_java_module_registry();
        self.prune_indexed_salsa_files(&current_indexed_uris);
        self.index.replace(new_index, Some(snapshot.clone()));
        self.publish_watched_source_roots();

        info!(
            generation = snapshot.generation,
            modules = snapshot.modules.len(),
            tool = ?snapshot.provenance.tool,
            tool_version = ?snapshot.provenance.tool_version,
            "workspace model applied"
        );
        Ok(())
    }

    pub async fn mark_model_stale(&self) {
        self.index.update_model(|model| {
            if let Some(model) = model.as_mut() {
                model.freshness = crate::build_integration::ModelFreshness::Stale;
            }
        });
    }

    pub async fn index_fallback_root(&self, root: PathBuf) -> Result<()> {
        let _reindex_guard = self.begin_full_reindex();
        self.set_workspace_root(root.clone());
        let open_doc_overlays = self
            .documents
            .snapshot_documents()
            .into_iter()
            .map(|(uri, language_id, content)| (uri.to_string(), (language_id, content)))
            .collect::<HashMap<_, _>>();
        let source_inputs = tokio::task::spawn_blocking(move || {
            let source_files = collect_source_files([root]);
            let mut source_inputs = load_source_inputs(source_files);
            overlay_open_document_inputs(&mut source_inputs, &open_doc_overlays);
            source_inputs
        })
        .await?;
        let (classes, current_indexed_uris) =
            self.index_source_inputs_with_shared_salsa(source_inputs, None);
        let scope = IndexScope {
            module: ModuleId::ROOT,
        };
        let index = WorkspaceIndex::new();
        let jdk_classes = self.jdk_classes.read().await.clone();
        let jdk_modules = self.jdk_modules.read().await.clone();
        if !jdk_classes.is_empty() || !jdk_modules.is_empty() {
            index.add_jdk_archive(IndexedArchiveData {
                classes: jdk_classes,
                modules: jdk_modules,
            });
        }
        let mut by_origin: std::collections::HashMap<ClassOrigin, Vec<_>> =
            std::collections::HashMap::new();
        for class in classes {
            by_origin
                .entry(class.origin.clone())
                .or_default()
                .push(class);
        }
        for (origin, classes) in by_origin {
            index.update_source(scope, origin, classes);
        }
        self.rebuild_java_module_registry();
        self.prune_indexed_salsa_files(&current_indexed_uris);
        self.index.replace(index, None);
        self.publish_watched_source_roots();
        Ok(())
    }

    pub(crate) fn watched_source_roots(&self) -> Vec<WatchedSourceRoot> {
        let (_, model) = self.index.snapshot();
        let mut roots = Vec::new();
        let mut seen = HashSet::new();

        if let Some(model) = model.as_ref() {
            for root in model
                .modules
                .iter()
                .flat_map(|module| module.roots.iter())
                .filter(|root| {
                    matches!(
                        root.kind,
                        WorkspaceRootKind::Sources
                            | WorkspaceRootKind::Tests
                            | WorkspaceRootKind::Generated
                    )
                })
            {
                let mode = scan_mode_for_root_kind(root.kind);
                if seen.insert(root.path.clone()) {
                    roots.push(WatchedSourceRoot {
                        path: root.path.clone(),
                        scan_mode: mode,
                    });
                }
            }
        }

        if roots.is_empty()
            && let Some(root) = self.workspace_root()
            && seen.insert(root.clone())
        {
            roots.push(WatchedSourceRoot {
                path: root,
                scan_mode: SourceScanMode::Default,
            });
        }

        roots
    }

    pub(crate) fn apply_filesystem_changes_blocking(
        &self,
        changes: Vec<FilesystemChange>,
    ) -> Result<FilesystemApplySummary> {
        let mut summary = FilesystemApplySummary::default();
        let mut removals = Vec::new();
        let mut upserts = Vec::new();

        for change in changes {
            match change.kind {
                FilesystemChangeKind::Remove => removals.push(change.path),
                FilesystemChangeKind::Upsert => upserts.push(change.path),
            }
        }

        removals.sort();
        removals.dedup();
        upserts.sort();
        upserts.dedup();

        for path in removals {
            match self.remove_disk_source_path_blocking(&path)? {
                FileApplyState::Applied => {
                    summary.applied += 1;
                    summary.removed += 1;
                }
                FileApplyState::SkippedOpenDocument => {
                    summary.skipped_open_documents += 1;
                }
                FileApplyState::Unchanged | FileApplyState::SkippedUntracked => {}
            }
        }

        for path in upserts {
            match self.upsert_disk_source_path_blocking(&path)? {
                FileApplyState::Applied => {
                    summary.applied += 1;
                }
                FileApplyState::SkippedOpenDocument => {
                    summary.skipped_open_documents += 1;
                }
                FileApplyState::Unchanged | FileApplyState::SkippedUntracked => {}
            }
        }

        Ok(summary)
    }

    pub(crate) fn reconcile_closed_document_blocking(&self, uri: &Url) -> Result<bool> {
        if self.documents.with_doc(uri, |_| ()).is_some() {
            return Ok(false);
        }

        if let Ok(path) = uri.to_file_path() {
            let (_, model) = self.index.snapshot();
            if path.exists() && self.should_track_disk_path_with_model(&path, model.as_ref()) {
                return Ok(matches!(
                    self.upsert_disk_source_path_blocking(&path)?,
                    FileApplyState::Applied | FileApplyState::Unchanged
                ));
            }
        }

        Ok(self.remove_source_origin_for_uri_blocking(uri))
    }

    pub(crate) fn rescan_watched_roots_blocking(
        &self,
        roots: Vec<WatchedSourceRoot>,
    ) -> Result<FilesystemApplySummary> {
        let current_paths = roots
            .iter()
            .flat_map(|root| collect_source_files_for_root(root.path.clone(), root.scan_mode))
            .collect::<HashSet<_>>();
        let current_uris = current_paths
            .iter()
            .filter_map(|path| file_url_from_path(path))
            .collect::<HashSet<_>>();

        let removals = {
            let tracked = self.indexed_salsa_uris.read();
            tracked
                .iter()
                .filter(|uri| self.documents.with_doc(uri, |_| ()).is_none())
                .filter_map(|uri| {
                    let path = uri.to_file_path().ok()?;
                    matches_watched_root(&path, &roots)
                        .filter(|_| !current_uris.contains(uri))
                        .map(|_| FilesystemChange::remove(path))
                })
                .collect::<Vec<_>>()
        };

        let upserts = current_paths.into_iter().map(FilesystemChange::upsert);
        self.apply_filesystem_changes_blocking(removals.into_iter().chain(upserts).collect())
    }

    /// Get or create a Salsa SourceFile for the given URI
    /// This is the bridge between LSP and Salsa
    pub fn get_or_create_salsa_file(
        &self,
        uri: &Url,
        content: &str,
        language_id: &str,
    ) -> crate::salsa_db::SourceFile {
        if let Some(file) = self.get_salsa_file(uri) {
            self.update_existing_salsa_file(file, content.to_string(), language_id.to_string());
            self.seed_salsa_parse_tree_from_document(uri, file);
            file
        } else {
            let created =
                self.create_salsa_file(uri.clone(), content.to_string(), language_id.to_string());
            let file = {
                let mut files = self.salsa_files.write();
                *files.entry(uri.clone()).or_insert(created)
            };
            self.update_existing_salsa_file(file, content.to_string(), language_id.to_string());
            self.seed_salsa_parse_tree_from_document(uri, file);
            file
        }
    }

    /// Get an existing Salsa SourceFile for the given URI
    pub fn get_salsa_file(&self, uri: &Url) -> Option<crate::salsa_db::SourceFile> {
        let files = self.salsa_files.read();
        files.get(uri).copied()
    }

    pub(crate) fn refresh_java_module_descriptor_for_salsa_file(
        &self,
        db: &crate::salsa_db::Database,
        file: crate::salsa_db::SourceFile,
    ) {
        let uri = file.file_id(db).uri().clone();
        if file.language_id(db).as_ref() != "java" {
            self.java_modules.write().remove(&uri);
            return;
        }

        let descriptor = crate::salsa_queries::java::extract_java_module_descriptor(db, file);
        let mut modules = self.java_modules.write();
        if let Some(descriptor) = descriptor {
            modules.upsert(uri, descriptor);
        } else {
            modules.remove(&uri);
        }
    }

    fn rebuild_java_module_registry(&self) {
        let files = self
            .salsa_files
            .read()
            .values()
            .copied()
            .collect::<Vec<_>>();
        let db = self.salsa_db.lock();
        let descriptors = files
            .into_iter()
            .filter_map(|file| {
                if file.language_id(&*db).as_ref() != "java" {
                    return None;
                }
                let descriptor =
                    crate::salsa_queries::java::extract_java_module_descriptor(&*db, file)?;
                Some((file.file_id(&*db).uri().clone(), descriptor))
            })
            .collect::<Vec<_>>();
        drop(db);
        self.java_modules.write().replace_all(descriptors);
    }

    pub fn java_module_names(&self) -> Vec<Arc<str>> {
        self.java_modules.read().module_names()
    }

    pub fn visible_java_module_names(&self, context: AnalysisContext) -> Vec<Arc<str>> {
        let mut names = BTreeSet::new();
        for name in self.java_modules.read().module_names() {
            names.insert(name);
        }
        let index = self.index.load();
        for name in index.visible_bytecode_module_names_for_analysis_context(
            context.module,
            context.classpath,
            context.source_root,
        ) {
            names.insert(name);
        }
        names.into_iter().collect()
    }

    pub fn java_module_descriptor_for_uri(&self, uri: &Url) -> Option<Arc<JavaModuleDescriptor>> {
        self.java_modules.read().descriptor_for_uri(uri)
    }

    pub fn java_module_uri(&self, module_name: &str) -> Option<Url> {
        self.java_modules.read().first_uri_for_name(module_name)
    }

    pub(crate) fn resolve_java_module_target(
        &self,
        context: AnalysisContext,
        module_name: &str,
    ) -> Option<JavaModuleTarget> {
        if let Some(uri) = self.java_module_uri(module_name) {
            return Some(JavaModuleTarget::Source { uri });
        }

        let index = self.index.load();
        index
            .find_visible_bytecode_module_for_analysis_context(
                context.module,
                context.classpath,
                context.source_root,
                module_name,
            )
            .map(|module| JavaModuleTarget::Bytecode { module })
    }

    pub(crate) fn extract_salsa_classes_for_index_context(
        &self,
        db: &crate::salsa_db::Database,
        salsa_file: crate::salsa_db::SourceFile,
        origin: &ClassOrigin,
        index: &WorkspaceIndex,
        context: AnalysisContext,
    ) -> Vec<ClassMetadata> {
        let Some(language) = crate::language::lookup_language(salsa_file.language_id(db).as_ref())
        else {
            return crate::salsa_queries::index::get_extracted_classes(db, salsa_file);
        };

        let live_index = db.workspace_index();
        let can_use_live_salsa_context = std::ptr::eq(index, live_index.as_ref());

        if salsa_file.language_id(db).as_ref() == "java" && can_use_live_salsa_context {
            return crate::salsa_queries::java::parse_java_classes_for_analysis_context(
                db,
                salsa_file,
                context.module,
                context.classpath,
                context.source_root,
                index.version(),
            )
            .as_ref()
            .clone();
        }

        let view =
            index.view_for_analysis_context(context.module, context.classpath, context.source_root);
        let name_table = index.build_name_table_for_analysis_context(
            context.module,
            context.classpath,
            context.source_root,
        );

        language.extract_classes_with_index_salsa(
            db,
            salsa_file,
            origin,
            Some(name_table),
            Some(&view),
        )
    }

    /// Remove a Salsa SourceFile (e.g., when file is deleted)
    pub fn remove_salsa_file(&self, uri: &Url) {
        let mut files = self.salsa_files.write();
        let removed = files.remove(uri);
        drop(files);

        let Some(file) = removed else {
            return;
        };

        let db = self.salsa_db.lock();
        let file_id = file.file_id(&*db);
        crate::salsa_queries::Db::remove_parse_tree(&*db, &file_id);
        crate::salsa_queries::Db::remove_class_extraction(&*db, &file_id);
        drop(db);
        self.java_modules.write().remove(uri);
    }

    fn create_salsa_file(
        &self,
        uri: Url,
        content: String,
        language_id: String,
    ) -> crate::salsa_db::SourceFile {
        let db = self.salsa_db.lock();
        crate::salsa_db::SourceFile::new(&*db, FileId::new(uri), content, Arc::from(language_id))
    }

    fn create_salsa_file_from_source(&self, source: &SourceFile) -> crate::salsa_db::SourceFile {
        let db = self.salsa_db.lock();
        crate::salsa_db::SourceFile::new(
            &*db,
            FileId::new(source.uri.as_ref().clone()),
            source.text().to_owned(),
            Arc::clone(&source.language_id),
        )
    }

    fn update_existing_salsa_file(
        &self,
        file: crate::salsa_db::SourceFile,
        content: String,
        language_id: String,
    ) {
        use salsa::Setter;

        let mut db = self.salsa_db.lock();
        if file.content(&*db).as_str() != content.as_str() {
            file.set_content(&mut *db).to(content);
        }
        if file.language_id(&*db).as_ref() != language_id {
            file.set_language_id(&mut *db).to(Arc::from(language_id));
        }
    }

    fn update_existing_salsa_file_from_source(
        &self,
        file: crate::salsa_db::SourceFile,
        source: &SourceFile,
    ) {
        use salsa::Setter;

        let mut db = self.salsa_db.lock();
        if file.content(&*db).as_str() != source.text() {
            file.set_content(&mut *db).to(source.text().to_owned());
        }
        if file.language_id(&*db).as_ref() != source.language_id.as_ref() {
            file.set_language_id(&mut *db)
                .to(Arc::clone(&source.language_id));
        }
    }

    fn seed_salsa_parse_tree_from_document(&self, uri: &Url, file: crate::salsa_db::SourceFile) {
        let Some(source) = self.document_snapshot(uri) else {
            return;
        };
        self.seed_salsa_parse_tree_from_source(file, source.as_ref());
    }

    fn seed_salsa_parse_tree_from_source(
        &self,
        file: crate::salsa_db::SourceFile,
        source: &SourceFile,
    ) {
        let Some(tree) = source.tree.as_deref().cloned() else {
            return;
        };

        let db = self.salsa_db.lock();
        if file.content(&*db).as_str() != source.text()
            || file.language_id(&*db).as_ref() != source.language_id.as_ref()
        {
            return;
        }

        crate::salsa_queries::parse::seed_parse_tree(&*db, file, &tree);
    }

    fn index_source_inputs_with_shared_salsa(
        &self,
        source_inputs: Vec<SourceTextInput>,
        name_table: Option<Arc<crate::index::NameTable>>,
    ) -> (Vec<ClassMetadata>, HashSet<Url>) {
        let mut prepared = Vec::with_capacity(source_inputs.len());
        let mut indexed_uris = HashSet::new();

        for source in source_inputs {
            let Ok(uri) = Url::parse(source.uri.as_ref()) else {
                continue;
            };
            indexed_uris.insert(uri.clone());
            let salsa_file =
                if crate::language::lookup_language(source.language_id.as_ref()).is_some() {
                    Some(self.get_or_create_salsa_file(
                        &uri,
                        source.content.as_str(),
                        source.language_id.as_ref(),
                    ))
                } else {
                    None
                };
            prepared.push(IndexedWorkspaceSource {
                language_id: source.language_id,
                content: source.content,
                origin: source.origin,
                salsa_file,
            });
        }

        let discovered_names = {
            let db = self.salsa_db.lock();
            prepared
                .iter()
                .flat_map(|source| source.discover_internal_names(&db))
                .collect::<Vec<_>>()
        };

        let enriched_name_table = match name_table {
            Some(existing) => existing.extend_with(discovered_names),
            None => crate::index::NameTable::from_names(discovered_names),
        };

        let classes = {
            let db = self.salsa_db.lock();
            prepared
                .into_iter()
                .flat_map(|source| source.extract_classes(&db, Some(enriched_name_table.clone())))
                .collect::<Vec<_>>()
        };

        (classes, indexed_uris)
    }

    fn prune_indexed_salsa_files(&self, current_indexed_uris: &HashSet<Url>) {
        let stale_uris = {
            let tracked = self.indexed_salsa_uris.read();
            tracked
                .difference(current_indexed_uris)
                .cloned()
                .collect::<Vec<_>>()
        };

        for uri in &stale_uris {
            if self.documents.with_doc(uri, |_| ()).is_none() {
                self.remove_salsa_file(uri);
            }
        }

        *self.indexed_salsa_uris.write() = current_indexed_uris.clone();
    }

    fn publish_watched_source_roots(&self) {
        let _ = self.watched_roots_tx.send(self.watched_source_roots());
    }

    fn should_track_disk_path_with_model(
        &self,
        path: &Path,
        model: Option<&WorkspaceModelSnapshot>,
    ) -> bool {
        if let Some(model) = model
            && let Some(root) = model.source_root_for_path(path, None)
        {
            return matches!(
                root.kind,
                WorkspaceRootKind::Sources
                    | WorkspaceRootKind::Tests
                    | WorkspaceRootKind::Generated
            ) && should_index_source_path(path, scan_mode_for_root_kind(root.kind));
        }

        self.workspace_root().is_some_and(|root| {
            path.starts_with(&root) && should_index_source_path(path, SourceScanMode::Default)
        })
    }

    fn upsert_disk_source_path_blocking(&self, path: &Path) -> Result<FileApplyState> {
        let Some(language_id) = language_id_for_path(path) else {
            return Ok(FileApplyState::SkippedUntracked);
        };
        let Some(uri) = file_url_from_path(path) else {
            return Ok(FileApplyState::SkippedUntracked);
        };
        if self.documents.with_doc(&uri, |_| ()).is_some() {
            return Ok(FileApplyState::SkippedOpenDocument);
        }

        let (index_snapshot, model) = self.index.snapshot();
        if !self.should_track_disk_path_with_model(path, model.as_ref()) {
            return Ok(FileApplyState::SkippedUntracked);
        }

        let content = match std::fs::read_to_string(path) {
            Ok(content) => content,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                return self.remove_disk_source_path_blocking(path);
            }
            Err(error) => return Err(error.into()),
        };

        let context = Self::resolve_analysis_context_for_snapshot(model.as_ref(), Some(path));
        let origin = ClassOrigin::SourceFile(Arc::from(uri.as_str()));
        let salsa_file = self.get_or_create_salsa_file(&uri, &content, language_id);
        let classes = {
            let db = self.salsa_db.lock();
            let _ = crate::salsa_queries::index::extract_classes(&*db, salsa_file);
            self.extract_salsa_classes_for_index_context(
                &*db,
                salsa_file,
                &origin,
                index_snapshot.as_ref(),
                context,
            )
        };

        let changed = self.index.update(|index| {
            index.update_source_in_context(context.module, context.source_root, origin, classes)
        });
        {
            let db = self.salsa_db.lock();
            self.refresh_java_module_descriptor_for_salsa_file(&*db, salsa_file);
        }
        self.track_indexed_uri(&uri);

        Ok(if changed {
            FileApplyState::Applied
        } else {
            FileApplyState::Unchanged
        })
    }

    fn remove_disk_source_path_blocking(&self, path: &Path) -> Result<FileApplyState> {
        let Some(uri) = file_url_from_path(path) else {
            return Ok(FileApplyState::SkippedUntracked);
        };
        if self.documents.with_doc(&uri, |_| ()).is_some() {
            return Ok(FileApplyState::SkippedOpenDocument);
        }

        Ok(if self.remove_source_origin_for_uri_blocking(&uri) {
            FileApplyState::Applied
        } else {
            FileApplyState::Unchanged
        })
    }

    fn remove_source_origin_for_uri_blocking(&self, uri: &Url) -> bool {
        let (_, model) = self.index.snapshot();
        let path = uri.to_file_path().ok();
        let context = Self::resolve_analysis_context_for_snapshot(model.as_ref(), path.as_deref());
        let origin = ClassOrigin::SourceFile(Arc::from(uri.as_str()));
        let changed = self.index.update(|index| {
            index.remove_source_origin_in_context(context.module, context.source_root, &origin)
        });
        self.untrack_indexed_uri(uri);
        self.remove_salsa_file(uri);
        changed
    }

    fn track_indexed_uri(&self, uri: &Url) {
        self.indexed_salsa_uris.write().insert(uri.clone());
    }

    fn untrack_indexed_uri(&self, uri: &Url) {
        self.indexed_salsa_uris.write().remove(uri);
    }
}

impl Default for Workspace {
    fn default() -> Self {
        Self::new()
    }
}

fn scan_mode_for_root_kind(kind: WorkspaceRootKind) -> SourceScanMode {
    if matches!(kind, WorkspaceRootKind::Generated) {
        SourceScanMode::IncludeGenerated
    } else {
        SourceScanMode::Default
    }
}

fn language_id_for_path(path: &Path) -> Option<&'static str> {
    crate::language::infer_language_id_from_path(path)
}

fn file_url_from_path(path: &Path) -> Option<Url> {
    let absolute = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    Url::from_file_path(absolute).ok()
}

fn matches_watched_root<'a>(
    path: &Path,
    roots: &'a [WatchedSourceRoot],
) -> Option<&'a WatchedSourceRoot> {
    roots
        .iter()
        .filter(|root| path.starts_with(&root.path))
        .max_by_key(|root| root.path.components().count())
}

fn merge_classpath<'a>(paths: impl Iterator<Item = &'a PathBuf>) -> Vec<&'a PathBuf> {
    let mut seen = HashSet::new();
    let mut merged = Vec::new();
    for path in paths {
        if seen.insert(path.clone()) {
            merged.push(path);
        }
    }
    merged
}

struct IndexedWorkspaceSource {
    language_id: Arc<str>,
    content: String,
    origin: ClassOrigin,
    salsa_file: Option<crate::salsa_db::SourceFile>,
}

impl IndexedWorkspaceSource {
    fn discover_internal_names(&self, db: &crate::salsa_db::Database) -> Vec<Arc<str>> {
        let Some(language) = crate::language::lookup_language(self.language_id.as_ref()) else {
            return vec![];
        };

        if let Some(file) = self.salsa_file
            && let Some(tree) = crate::salsa_queries::parse::parse_tree(db, file)
        {
            return language.discover_internal_names(self.content.as_str(), Some(&tree));
        }

        language.discover_internal_names(self.content.as_str(), None)
    }

    fn extract_classes(
        self,
        db: &crate::salsa_db::Database,
        name_table: Option<Arc<crate::index::NameTable>>,
    ) -> Vec<ClassMetadata> {
        let Some(language) = crate::language::lookup_language(self.language_id.as_ref()) else {
            return vec![];
        };

        if let Some(file) = self.salsa_file
            && let Some(tree) = crate::salsa_queries::parse::parse_tree(db, file)
        {
            return language.extract_classes_from_source(
                self.content.as_str(),
                &self.origin,
                Some(&tree),
                name_table,
                None,
            );
        }

        language.extract_classes_from_source(
            self.content.as_str(),
            &self.origin,
            None,
            name_table,
            None,
        )
    }
}

fn overlay_open_document_inputs(
    source_inputs: &mut [SourceTextInput],
    open_doc_overlays: &HashMap<String, (String, String)>,
) {
    for source in source_inputs {
        if let Some((language_id, content)) = open_doc_overlays.get(source.uri.as_ref()) {
            source.language_id = Arc::from(language_id.as_str());
            source.content = content.clone();
        }
    }
}

impl Workspace {
    fn resolve_analysis_context_for_path(&self, path: Option<&Path>) -> AnalysisContext {
        let (_, model) = self.index.snapshot();
        Self::resolve_analysis_context_for_snapshot(model.as_ref(), path)
    }

    fn resolve_analysis_context_for_snapshot(
        model: Option<&WorkspaceModelSnapshot>,
        path: Option<&Path>,
    ) -> AnalysisContext {
        if let Some(path) = path
            && let Some(model) = model
        {
            let best_root = model
                .modules
                .iter()
                .flat_map(|module| module.roots.iter().map(move |root| (module.id, root)))
                .filter(|(_, root)| path.starts_with(&root.path))
                .max_by_key(|(_, root)| root.path.components().count());

            if let Some((module, root)) = best_root {
                return AnalysisContext {
                    module,
                    classpath: root.classpath,
                    source_root: Some(root.id),
                    root_kind: Some(root.kind),
                };
            }

            if let Some(module) = model.module_for_path(path) {
                tracing::debug!(
                    path = %path.display(),
                    module = module.id.0,
                    "managed workspace path fell back to module-level analysis context"
                );
                return AnalysisContext {
                    module: module.id,
                    classpath: ClasspathId::Main,
                    source_root: None,
                    root_kind: None,
                };
            }
        }

        AnalysisContext {
            module: ModuleId::ROOT,
            classpath: ClasspathId::Main,
            source_root: None,
            root_kind: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::SystemTime;

    use crate::build_integration::{
        DetectedBuildToolKind, JavaToolchainInfo, ModelFidelity, ModelFreshness,
        WorkspaceModelProvenance, WorkspaceModule, WorkspaceRoot, WorkspaceSourceRoot,
    };
    use crate::language::LanguageRegistry;
    use crate::salsa_db::ParseTreeOrigin;
    use crate::workspace::document::Document;
    use tempfile::tempdir;

    fn make_bytecode_module(name: &str, origin: ClassOrigin) -> IndexedJavaModule {
        IndexedJavaModule {
            descriptor: Arc::new(JavaModuleDescriptor {
                name: Arc::from(name),
                is_open: false,
                requires: vec![],
                exports: vec![],
                opens: vec![],
                uses: vec![],
                provides: vec![],
            }),
            origin,
        }
    }

    #[test]
    fn managed_workspace_prefers_imported_source_root_context() {
        let workspace = Workspace::new();
        workspace.index.replace_model(Some(WorkspaceModelSnapshot {
            generation: 1,
            root: WorkspaceRoot {
                path: PathBuf::from("/workspace"),
            },
            name: "demo".into(),
            modules: vec![WorkspaceModule {
                id: ModuleId(1),
                name: "app".into(),
                directory: PathBuf::from("/workspace/app"),
                roots: vec![
                    WorkspaceSourceRoot {
                        id: SourceRootId(11),
                        path: PathBuf::from("/workspace/app/src/main/java"),
                        kind: WorkspaceRootKind::Sources,
                        classpath: ClasspathId::Main,
                    },
                    WorkspaceSourceRoot {
                        id: SourceRootId(12),
                        path: PathBuf::from("/workspace/app/src/test/java"),
                        kind: WorkspaceRootKind::Tests,
                        classpath: ClasspathId::Test,
                    },
                ],
                compile_classpath: vec![],
                test_classpath: vec![],
                dependency_modules: vec![],
                java: JavaToolchainInfo {
                    language_version: None,
                },
            }],
            provenance: WorkspaceModelProvenance {
                tool: DetectedBuildToolKind::Gradle,
                tool_version: Some("9.2.0".into()),
                imported_at: SystemTime::UNIX_EPOCH,
            },
            freshness: ModelFreshness::Fresh,
            fidelity: ModelFidelity::Full,
        }));

        let test_file = PathBuf::from("/workspace/app/src/test/java/demo/AppTest.java");
        let ctx = workspace.resolve_analysis_context_for_path(Some(&test_file));
        assert_eq!(ctx.module, ModuleId(1));
        assert_eq!(ctx.classpath, ClasspathId::Test);
        assert_eq!(ctx.source_root, Some(SourceRootId(12)));
        assert_eq!(ctx.root_kind, Some(WorkspaceRootKind::Tests));
    }

    #[test]
    fn infer_java_package_for_uri_uses_managed_source_root() {
        let workspace = Workspace::new();
        workspace.index.replace_model(Some(WorkspaceModelSnapshot {
            generation: 1,
            root: WorkspaceRoot {
                path: PathBuf::from("/workspace"),
            },
            name: "demo".into(),
            modules: vec![WorkspaceModule {
                id: ModuleId(1),
                name: "app".into(),
                directory: PathBuf::from("/workspace/app"),
                roots: vec![WorkspaceSourceRoot {
                    id: SourceRootId(11),
                    path: PathBuf::from("/workspace/app/src/main/java"),
                    kind: WorkspaceRootKind::Sources,
                    classpath: ClasspathId::Main,
                }],
                compile_classpath: vec![],
                test_classpath: vec![],
                dependency_modules: vec![],
                java: JavaToolchainInfo {
                    language_version: None,
                },
            }],
            provenance: WorkspaceModelProvenance {
                tool: DetectedBuildToolKind::Gradle,
                tool_version: Some("9.2.0".into()),
                imported_at: SystemTime::UNIX_EPOCH,
            },
            freshness: ModelFreshness::Fresh,
            fidelity: ModelFidelity::Full,
        }));

        let uri = Url::parse("file:///workspace/app/src/main/java/org/example/foo/Bar.java")
            .expect("valid uri");
        let inferred = workspace
            .infer_java_package_for_uri(&uri, Some(SourceRootId(11)))
            .expect("package");

        assert_eq!(inferred.as_ref(), "org.example.foo");
    }

    #[test]
    fn get_or_update_salsa_file_keeps_language_and_content_in_sync() {
        let workspace = Workspace::new();
        let uri = Url::parse("file:///workspace/Test.java").expect("valid uri");
        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                "class Test {}",
                None,
            )));

        let file = workspace
            .get_or_update_salsa_file(&uri)
            .expect("salsa file should exist");
        {
            let db = workspace.salsa_db.lock();
            assert_eq!(file.content(&*db), "class Test {}");
            assert_eq!(file.language_id(&*db).as_ref(), "java");
        }

        workspace.documents.with_doc_mut(&uri, |doc| {
            doc.update_source(crate::workspace::SourceFile::new(
                uri.clone(),
                "kotlin",
                2,
                "class Test",
                None,
            ));
        });

        let same_file = workspace
            .get_or_update_salsa_file(&uri)
            .expect("salsa file should stay addressable");
        assert!(file == same_file);

        let db = workspace.salsa_db.lock();
        assert_eq!(same_file.content(&*db), "class Test");
        assert_eq!(same_file.language_id(&*db).as_ref(), "kotlin");
    }

    #[test]
    fn get_or_update_salsa_file_seeds_parse_cache_from_document_tree() {
        let workspace = Workspace::new();
        let uri = Url::parse("file:///workspace/Test.java").expect("valid uri");
        let text = "class Test { void demo() {} }";
        let mut parser = crate::language::java::make_java_parser();
        let tree = parser.parse(text, None).expect("tree");

        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                text,
                Some(tree),
            )));

        let file = workspace
            .get_or_update_salsa_file(&uri)
            .expect("salsa file should exist");

        let db = workspace.salsa_db.lock();
        let file_id = file.file_id(&*db);
        let snapshot = db
            .cached_parse_tree(&file_id)
            .expect("parse snapshot should be seeded");

        assert_eq!(snapshot.origin, ParseTreeOrigin::Seeded);
        assert_eq!(snapshot.content.as_ref(), text);
    }

    #[test]
    fn java_module_registry_tracks_module_info_source_files() {
        let workspace = Workspace::new();
        let uri = Url::parse("file:///workspace/module-info.java").expect("valid uri");
        let salsa_file = workspace.get_or_create_salsa_file(
            &uri,
            "module com.example.app { requires com.example.shared; }",
            "java",
        );

        {
            let db = workspace.salsa_db.lock();
            workspace.refresh_java_module_descriptor_for_salsa_file(&*db, salsa_file);
        }

        assert_eq!(
            workspace.java_module_names(),
            vec![Arc::from("com.example.app")]
        );
        assert_eq!(
            workspace
                .java_module_descriptor_for_uri(&uri)
                .expect("module descriptor")
                .name
                .as_ref(),
            "com.example.app"
        );
        assert_eq!(
            workspace
                .java_module_uri("com.example.app")
                .expect("module uri"),
            uri
        );
    }

    #[test]
    fn visible_java_module_names_merge_source_and_bytecode_modules() {
        let workspace = Workspace::new();
        let uri = Url::parse("file:///workspace/module-info.java").expect("valid uri");
        let salsa_file =
            workspace.get_or_create_salsa_file(&uri, "module com.example.app { }", "java");

        {
            let db = workspace.salsa_db.lock();
            workspace.refresh_java_module_descriptor_for_salsa_file(&*db, salsa_file);
        }

        workspace.index.update(|index| {
            index.add_jdk_archive(IndexedArchiveData {
                classes: vec![],
                modules: vec![make_bytecode_module(
                    "com.example.lib",
                    ClassOrigin::Jar(Arc::from("jdk://builtin")),
                )],
            });
        });

        let names = workspace.visible_java_module_names(AnalysisContext {
            module: ModuleId::ROOT,
            classpath: ClasspathId::Main,
            source_root: None,
            root_kind: None,
        });

        assert_eq!(
            names,
            vec![
                Arc::<str>::from("com.example.app"),
                Arc::<str>::from("com.example.lib")
            ]
        );
    }

    #[test]
    fn resolve_java_module_target_prefers_source_over_bytecode() {
        let workspace = Workspace::new();
        let uri = Url::parse("file:///workspace/shared/module-info.java").expect("valid uri");
        let salsa_file =
            workspace.get_or_create_salsa_file(&uri, "module com.example.shared { }", "java");

        {
            let db = workspace.salsa_db.lock();
            workspace.refresh_java_module_descriptor_for_salsa_file(&*db, salsa_file);
        }

        workspace.index.update(|index| {
            index.add_jdk_archive(IndexedArchiveData {
                classes: vec![],
                modules: vec![make_bytecode_module(
                    "com.example.shared",
                    ClassOrigin::Jar(Arc::from("/tmp/shared.jar")),
                )],
            });
        });

        let target = workspace
            .resolve_java_module_target(
                AnalysisContext {
                    module: ModuleId::ROOT,
                    classpath: ClasspathId::Main,
                    source_root: None,
                    root_kind: None,
                },
                "com.example.shared",
            )
            .expect("module target");

        match target {
            JavaModuleTarget::Source { uri: target_uri } => assert_eq!(target_uri, uri),
            JavaModuleTarget::Bytecode { .. } => {
                panic!("source module should win over bytecode module")
            }
        }
    }

    #[test]
    fn ensure_tree_materializes_tree_on_document_snapshot_boundary() {
        let workspace = Workspace::new();
        let registry = LanguageRegistry::new();
        let uri = Url::parse("file:///workspace/Test.java").expect("valid uri");
        let text = "class Test { void demo() {} }";
        let lang = registry.find("java").expect("java language");

        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                text,
                None,
            )));

        let snapshot = workspace
            .ensure_tree(&uri, lang)
            .expect("snapshot with tree should exist");

        assert!(
            snapshot.tree.is_some(),
            "returned snapshot should have a tree"
        );
        assert!(
            workspace
                .document_snapshot(&uri)
                .expect("stored snapshot")
                .tree
                .is_some(),
            "document snapshot boundary should publish the tree back to the workspace"
        );
    }

    #[tokio::test]
    async fn fallback_root_indexing_reuses_shared_salsa_parse_tree() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("Demo.java");
        fs::write(&path, "class Demo { int value; }").expect("write source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("first index");

        let file = workspace.get_salsa_file(&uri).expect("tracked salsa file");
        {
            let db = workspace.salsa_db.lock();
            let snapshot = db
                .cached_parse_tree(&file.file_id(&*db))
                .expect("cached parse tree");
            assert_eq!(snapshot.origin, ParseTreeOrigin::Full);
        }

        fs::write(&path, "class Demo { String value; }").expect("rewrite source");

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("second index");

        let file = workspace.get_salsa_file(&uri).expect("tracked salsa file");
        let db = workspace.salsa_db.lock();
        let snapshot = db
            .cached_parse_tree(&file.file_id(&*db))
            .expect("cached parse tree");
        assert_eq!(snapshot.origin, ParseTreeOrigin::Incremental);
    }

    #[tokio::test]
    async fn fallback_root_indexing_prefers_open_document_snapshot() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("Demo.java");
        fs::write(&path, "class Demo { int disk; }").expect("write source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                "class Demo { String live; }",
                None,
            )));

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("index root");

        let file = workspace.get_salsa_file(&uri).expect("tracked salsa file");
        let db = workspace.salsa_db.lock();
        assert_eq!(file.content(&*db), "class Demo { String live; }");
    }

    #[tokio::test]
    async fn filesystem_apply_skips_open_document_snapshot() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("Demo.java");
        fs::write(&path, "class Demo { int disk; }").expect("write source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                "class Demo { String live; }",
                None,
            )));

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("index root");

        fs::write(&path, "class Demo { boolean disk; }").expect("rewrite source");

        let summary = workspace
            .apply_filesystem_changes_blocking(vec![FilesystemChange::upsert(path.clone())])
            .expect("apply filesystem change");
        assert_eq!(summary.applied, 0);
        assert_eq!(summary.skipped_open_documents, 1);

        let file = workspace.get_salsa_file(&uri).expect("tracked salsa file");
        let db = workspace.salsa_db.lock();
        assert_eq!(file.content(&*db), "class Demo { String live; }");
    }

    #[tokio::test]
    async fn watched_root_subscription_publishes_fallback_root() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let mut roots_rx = workspace.subscribe_watched_source_roots();

        workspace.set_workspace_root(dir.path().to_path_buf());

        roots_rx.changed().await.expect("watched root update");
        assert_eq!(
            roots_rx.borrow().clone(),
            vec![WatchedSourceRoot {
                path: dir.path().to_path_buf(),
                scan_mode: SourceScanMode::Default,
            }]
        );
    }

    #[tokio::test]
    async fn rescan_watched_roots_removes_deleted_disk_source() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("Demo.java");
        fs::write(&path, "class Demo { int disk; }").expect("write source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("index root");
        assert!(
            workspace.get_salsa_file(&uri).is_some(),
            "tracked before delete"
        );

        fs::remove_file(&path).expect("remove source");

        let summary = workspace
            .rescan_watched_roots_blocking(workspace.watched_source_roots())
            .expect("rescan roots");
        assert_eq!(summary.applied, 1);
        assert_eq!(summary.removed, 1);
        assert!(
            workspace.get_salsa_file(&uri).is_none(),
            "deleted file should be removed from tracked state"
        );
    }

    #[tokio::test]
    async fn reconcile_closed_document_restores_disk_snapshot() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let path = dir.path().join("Demo.java");
        fs::write(&path, "class Demo { int disk; }").expect("write source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                uri.clone(),
                "java",
                1,
                "class Demo { String live; }",
                None,
            )));

        workspace
            .index_fallback_root(dir.path().to_path_buf())
            .await
            .expect("index root");

        workspace.documents.close(&uri);
        workspace
            .reconcile_closed_document_blocking(&uri)
            .expect("reconcile closed document");

        let file = workspace.get_salsa_file(&uri).expect("tracked salsa file");
        let db = workspace.salsa_db.lock();
        assert_eq!(file.content(&*db), "class Demo { int disk; }");
    }

    #[tokio::test]
    async fn managed_workspace_indexes_generated_root_sources() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let app_dir = dir.path().join("app");
        let generated_root = app_dir.join("build/generated/sources/annotationProcessor/java/main");
        let pkg_dir = generated_root.join("org/example");
        fs::create_dir_all(&pkg_dir).expect("create generated package dir");
        let path = pkg_dir.join("GeneratedDemo.java");
        fs::write(
            &path,
            "package org.example; public class GeneratedDemo { int value; }",
        )
        .expect("write generated source");
        let uri =
            Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri");

        workspace
            .apply_workspace_model(WorkspaceModelSnapshot {
                generation: 1,
                root: WorkspaceRoot {
                    path: dir.path().to_path_buf(),
                },
                name: "demo".into(),
                modules: vec![WorkspaceModule {
                    id: ModuleId(1),
                    name: "app".into(),
                    directory: app_dir.clone(),
                    roots: vec![WorkspaceSourceRoot {
                        id: SourceRootId(11),
                        path: generated_root.clone(),
                        kind: WorkspaceRootKind::Generated,
                        classpath: ClasspathId::Main,
                    }],
                    compile_classpath: vec![],
                    test_classpath: vec![],
                    dependency_modules: vec![],
                    java: JavaToolchainInfo {
                        language_version: None,
                    },
                }],
                provenance: WorkspaceModelProvenance {
                    tool: DetectedBuildToolKind::Gradle,
                    tool_version: Some("8.10".into()),
                    imported_at: SystemTime::UNIX_EPOCH,
                },
                freshness: ModelFreshness::Fresh,
                fidelity: ModelFidelity::Full,
            })
            .await
            .expect("apply workspace model");

        assert!(
            workspace.get_salsa_file(&uri).is_some(),
            "generated Java source roots should be indexed"
        );
    }

    #[tokio::test]
    async fn managed_workspace_open_document_reindex_preserves_fully_qualified_super_name() {
        let workspace = Workspace::new();
        let dir = tempdir().expect("tempdir");
        let app_dir = dir.path().join("app");
        let src_root = app_dir.join("src/main/java");
        let pkg_dir = src_root.join("org/example");
        fs::create_dir_all(&pkg_dir).expect("create package dir");

        fs::write(
            pkg_dir.join("SomeRandomClass.java"),
            "package org.example; public class SomeRandomClass {}",
        )
        .expect("write base source");

        let main_source = "package org.example; public class Main extends SomeRandomClass {}";
        let main_path = pkg_dir.join("Main.java");
        fs::write(&main_path, main_source).expect("write main source");

        let main_uri = Url::from_file_path(main_path.canonicalize().expect("canonical path"))
            .expect("main uri");
        workspace
            .documents
            .open(Document::new(crate::workspace::SourceFile::new(
                main_uri,
                "java",
                1,
                main_source,
                None,
            )));

        workspace
            .apply_workspace_model(WorkspaceModelSnapshot {
                generation: 1,
                root: WorkspaceRoot {
                    path: dir.path().to_path_buf(),
                },
                name: "demo".into(),
                modules: vec![WorkspaceModule {
                    id: ModuleId(1),
                    name: "app".into(),
                    directory: app_dir,
                    roots: vec![WorkspaceSourceRoot {
                        id: SourceRootId(11),
                        path: src_root,
                        kind: WorkspaceRootKind::Sources,
                        classpath: ClasspathId::Main,
                    }],
                    compile_classpath: vec![],
                    test_classpath: vec![],
                    dependency_modules: vec![],
                    java: JavaToolchainInfo {
                        language_version: None,
                    },
                }],
                provenance: WorkspaceModelProvenance {
                    tool: DetectedBuildToolKind::Gradle,
                    tool_version: Some("9.2.0".into()),
                    imported_at: SystemTime::UNIX_EPOCH,
                },
                freshness: ModelFreshness::Fresh,
                fidelity: ModelFidelity::Full,
            })
            .await
            .expect("apply workspace model");

        let index = workspace.index.load();
        let view =
            index.view_for_analysis_context(ModuleId(1), ClasspathId::Main, Some(SourceRootId(11)));
        let main = view.get_class("org/example/Main").expect("indexed Main");

        assert_eq!(
            main.super_name.as_deref(),
            Some("org/example/SomeRandomClass"),
            "open-document reindex should preserve the fully qualified superclass from the analysis view",
        );
    }
}
pub mod source_file;
pub use source_file::SourceFile;
