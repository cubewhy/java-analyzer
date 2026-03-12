use anyhow::Result;
use parking_lot::RwLock as ParkingRwLock;
use std::collections::HashSet;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Url;
use tracing::info;

use crate::build_integration::{SourceRootId, WorkspaceModelSnapshot, WorkspaceRootKind};
use crate::index::codebase::{index_codebase, index_codebase_paths, index_source_text};
use crate::index::{ClassMetadata, ClassOrigin, ClasspathId, IndexScope, ModuleId, WorkspaceIndex};
use document::DocumentStore;

pub mod document;

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

pub struct Workspace {
    pub documents: DocumentStore,
    pub index: Arc<RwLock<WorkspaceIndex>>,
    model: ParkingRwLock<Option<WorkspaceModelSnapshot>>,
    jdk_classes: RwLock<Vec<ClassMetadata>>,
}

impl Workspace {
    pub fn new() -> Self {
        Self {
            documents: DocumentStore::new(),
            index: Arc::new(RwLock::new(WorkspaceIndex::new())),
            model: ParkingRwLock::new(None),
            jdk_classes: RwLock::new(Vec::new()),
        }
    }

    pub fn scope_for_uri(&self, uri: &Url) -> IndexScope {
        self.resolve_analysis_context_for_path(uri.to_file_path().ok().as_deref())
            .scope()
    }

    pub fn analysis_context_for_uri(&self, uri: &Url) -> AnalysisContext {
        let ctx = self.resolve_analysis_context_for_path(uri.to_file_path().ok().as_deref());
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
        self.model.read().clone()
    }

    pub async fn set_jdk_classes(&self, classes: Vec<ClassMetadata>) {
        *self.jdk_classes.write().await = classes.clone();
        self.index.write().await.add_jdk_classes(classes);
    }

    pub async fn apply_workspace_model(&self, snapshot: WorkspaceModelSnapshot) -> Result<()> {
        let jdk_classes = self.jdk_classes.read().await.clone();
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
                        Some((module.id, root.id, root.classpath, root.path.clone()))
                    } else {
                        None
                    }
                })
            })
            .collect::<Vec<_>>();

        let indexed_roots = tokio::task::spawn_blocking(move || {
            root_inputs
                .into_iter()
                .map(|(module_id, root_id, classpath, root_path)| {
                    (
                        module_id,
                        root_id,
                        classpath,
                        root_path.clone(),
                        index_codebase_paths([root_path], None).classes,
                    )
                })
                .collect::<Vec<_>>()
        })
        .await?;

        let new_index = WorkspaceIndex::new();
        if !jdk_classes.is_empty() {
            new_index.add_jdk_classes(jdk_classes);
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

        for (module_id, root_id, classpath, root_path, classes) in indexed_roots {
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

        {
            let mut guard = self.index.write().await;
            *guard = new_index;
        }
        *self.model.write() = Some(snapshot.clone());

        let open_docs = self.documents.snapshot_documents();
        for (uri, language_id, content) in open_docs {
            let context = self.analysis_context_for_uri(&uri);
            let name_table = self
                .index
                .read()
                .await
                .build_name_table_for_analysis_context(
                    context.module,
                    context.classpath,
                    context.source_root,
                );
            let classes =
                index_source_text(&uri.to_string(), &content, &language_id, Some(name_table));
            let origin = ClassOrigin::SourceFile(Arc::from(uri.to_string().as_str()));
            tracing::debug!(
                uri = %uri,
                module = context.module.0,
                classpath = ?context.classpath,
                source_root = ?context.source_root.map(|id| id.0),
                "reindexing open document against workspace model"
            );
            self.index.write().await.update_source_in_context(
                context.module,
                context.source_root,
                origin,
                classes,
            );
        }

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
        if let Some(model) = self.model.write().as_mut() {
            model.freshness = crate::build_integration::ModelFreshness::Stale;
        }
    }

    pub async fn index_fallback_root(&self, root: PathBuf) -> Result<()> {
        let result = tokio::task::spawn_blocking(move || index_codebase(root, None)).await?;
        let scope = IndexScope {
            module: ModuleId::ROOT,
        };
        let index = WorkspaceIndex::new();
        let jdk_classes = self.jdk_classes.read().await.clone();
        if !jdk_classes.is_empty() {
            index.add_jdk_classes(jdk_classes);
        }
        let mut by_origin: std::collections::HashMap<ClassOrigin, Vec<_>> =
            std::collections::HashMap::new();
        for class in result.classes {
            by_origin
                .entry(class.origin.clone())
                .or_default()
                .push(class);
        }
        for (origin, classes) in by_origin {
            index.update_source(scope, origin, classes);
        }
        *self.index.write().await = index;
        *self.model.write() = None;
        Ok(())
    }
}

impl Default for Workspace {
    fn default() -> Self {
        Self::new()
    }
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

impl Workspace {
    fn resolve_analysis_context_for_path(&self, path: Option<&Path>) -> AnalysisContext {
        if let Some(path) = path
            && let Some(model) = self.model.read().as_ref()
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
    use std::time::SystemTime;

    use crate::build_integration::{
        DetectedBuildToolKind, JavaToolchainInfo, ModelFidelity, ModelFreshness,
        WorkspaceModelProvenance, WorkspaceModule, WorkspaceRoot, WorkspaceSourceRoot,
    };

    #[test]
    fn managed_workspace_prefers_imported_source_root_context() {
        let workspace = Workspace::new();
        *workspace.model.write() = Some(WorkspaceModelSnapshot {
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
        });

        let test_file = PathBuf::from("/workspace/app/src/test/java/demo/AppTest.java");
        let ctx = workspace.resolve_analysis_context_for_path(Some(&test_file));
        assert_eq!(ctx.module, ModuleId(1));
        assert_eq!(ctx.classpath, ClasspathId::Test);
        assert_eq!(ctx.source_root, Some(SourceRootId(12)));
        assert_eq!(ctx.root_kind, Some(WorkspaceRootKind::Tests));
    }
}
