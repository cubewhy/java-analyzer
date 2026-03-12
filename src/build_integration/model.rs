use std::path::PathBuf;
use std::time::SystemTime;

use serde::{Deserialize, Serialize};

use crate::index::{ClasspathId, ModuleId};

use super::detection::DetectedBuildToolKind;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ModelFreshness {
    Fresh,
    Stale,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ModelFidelity {
    Full,
    Partial,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceModelProvenance {
    pub tool: DetectedBuildToolKind,
    pub tool_version: Option<String>,
    pub imported_at: SystemTime,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JavaToolchainInfo {
    pub language_version: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceRoot {
    pub path: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceRootId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkspaceRootKind {
    Sources,
    Tests,
    Resources,
    Generated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceSourceRoot {
    pub id: SourceRootId,
    pub path: PathBuf,
    pub kind: WorkspaceRootKind,
    pub classpath: ClasspathId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceModule {
    pub id: ModuleId,
    pub name: String,
    pub directory: PathBuf,
    pub roots: Vec<WorkspaceSourceRoot>,
    pub compile_classpath: Vec<PathBuf>,
    pub test_classpath: Vec<PathBuf>,
    pub dependency_modules: Vec<ModuleId>,
    pub java: JavaToolchainInfo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceModelSnapshot {
    pub generation: u64,
    pub root: WorkspaceRoot,
    pub name: String,
    pub modules: Vec<WorkspaceModule>,
    pub provenance: WorkspaceModelProvenance,
    pub freshness: ModelFreshness,
    pub fidelity: ModelFidelity,
}

impl WorkspaceModelSnapshot {
    pub fn module_for_path(&self, path: &std::path::Path) -> Option<&WorkspaceModule> {
        self.modules.iter().find(|module| {
            module.directory == path
                || path.starts_with(&module.directory)
                || module.roots.iter().any(|root| path.starts_with(&root.path))
        })
    }

    pub fn scope_for_path(
        &self,
        path: &std::path::Path,
    ) -> Option<(ModuleId, ClasspathId, Option<SourceRootId>)> {
        self.modules.iter().find_map(|module| {
            if let Some(root) = module
                .roots
                .iter()
                .filter(|root| path.starts_with(&root.path))
                .max_by_key(|root| root.path.components().count())
            {
                return Some((module.id, root.classpath, Some(root.id)));
            }

            if module.directory == path || path.starts_with(&module.directory) {
                return Some((module.id, ClasspathId::Main, None));
            }

            None
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_test_roots_to_test_classpath() {
        let snapshot = WorkspaceModelSnapshot {
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
                        id: SourceRootId(1),
                        path: PathBuf::from("/workspace/app/src/main/java"),
                        kind: WorkspaceRootKind::Sources,
                        classpath: ClasspathId::Main,
                    },
                    WorkspaceSourceRoot {
                        id: SourceRootId(2),
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
                tool_version: Some("8.10".into()),
                imported_at: SystemTime::UNIX_EPOCH,
            },
            freshness: ModelFreshness::Fresh,
            fidelity: ModelFidelity::Full,
        };

        assert_eq!(
            snapshot.scope_for_path(std::path::Path::new("/workspace/app/src/test/java/A.java")),
            Some((ModuleId(1), ClasspathId::Test, Some(SourceRootId(2))))
        );
    }
}
