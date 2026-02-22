use anyhow::Result;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{error, info, warn};

use crate::index::{ClassMetadata, GlobalIndex, index_jar};
use document::DocumentStore;

pub mod document;

pub struct Workspace {
    pub documents: DocumentStore,
    pub index: Arc<RwLock<GlobalIndex>>,
    loaded_jars: RwLock<Vec<PathBuf>>,
    pub source_roots: RwLock<Vec<PathBuf>>,
}

impl Workspace {
    pub fn new() -> Self {
        Self {
            documents: DocumentStore::new(),
            index: Arc::new(RwLock::new(GlobalIndex::new())),
            loaded_jars: RwLock::new(Vec::new()),
            source_roots: RwLock::new(Vec::new()),
        }
    }

    /// 注册 source root（由 LSP initialize / didChangeConfiguration 调用）
    pub async fn add_source_root(&self, root: PathBuf) {
        let mut roots = self.source_roots.write().await;
        if !roots.contains(&root) {
            roots.push(root);
        }
    }

    /// 根据文件 URI 推断 Java 包名（slash 格式，如 "org/cubewhy/a"）
    /// 遍历所有 source roots，找到最长匹配前缀
    pub async fn infer_package_from_uri(&self, file_uri: &str) -> Option<Arc<str>> {
        // TODO: infer source root from Build tools result
        // 把 URI 转成文件路径
        let url = tower_lsp::lsp_types::Url::parse(file_uri).ok()?;
        let file_path = url.to_file_path().ok()?;
        let parent = file_path.parent()?;

        let roots = self.source_roots.read().await;
        // 找最长匹配的 source root
        let best = roots
            .iter()
            .filter_map(|root| parent.strip_prefix(root).ok().map(|rel| (root, rel)))
            .max_by_key(|(_, rel)| rel.components().count());

        if let Some((_, rel)) = best {
            let pkg = rel
                .components()
                .filter_map(|c| c.as_os_str().to_str())
                .collect::<Vec<_>>()
                .join("/");
            if pkg.is_empty() {
                None
            } else {
                Some(Arc::from(pkg.as_str()))
            }
        } else {
            None
        }
    }

    pub async fn load_jar_async(&self, path: PathBuf) -> Result<()> {
        let path_clone = path.clone();
        let classes = tokio::task::spawn_blocking(move || {
            info!(path = %path_clone.display(), "indexing jar");
            index_jar(&path_clone)
        })
        .await??;

        info!(
            path = %path.display(),
            count = classes.len(),
            "jar indexed"
        );

        self.index.write().await.add_classes(classes);
        self.loaded_jars.write().await.push(path);
        Ok(())
    }

    /// Scan the directory and load all JAR files concurrently
    pub async fn load_jars_from_dir(&self, dir: PathBuf) {
        // let index_arc = Arc::clone(&self.index);

        let result = tokio::task::spawn_blocking(move || {
            let entries: Vec<PathBuf> = std::fs::read_dir(&dir)
                .into_iter()
                .flatten()
                .flatten()
                .map(|e| e.path())
                .filter(|p| p.extension().is_some_and(|e| e == "jar"))
                .collect();

            info!(dir = %dir.display(), count = entries.len(), "found jars, parsing in parallel");

            let all_classes: Vec<ClassMetadata> = entries
                .par_iter()
                .flat_map(|jar| match index_jar(jar) {
                    Ok(classes) => {
                        info!(path = %jar.display(), count = classes.len(), "jar indexed");
                        classes
                    }
                    Err(e) => {
                        warn!(path = %jar.display(), error = %e, "failed");
                        vec![]
                    }
                })
                .collect();

            all_classes
        })
        .await;

        match result {
            Ok(classes) => {
                info!(total = classes.len(), "all jars parsed, writing to index");
                self.index.write().await.add_classes(classes);
            }
            Err(e) => error!(error = %e, "jar indexing panicked"),
        }
    }
}

impl Default for Workspace {
    fn default() -> Self {
        Self::new()
    }
}
