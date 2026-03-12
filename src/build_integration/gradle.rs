use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::{Context, Result, anyhow, bail};
use async_trait::async_trait;
use serde::Deserialize;
use tempfile::{Builder, NamedTempFile};
use tokio::process::Command;

use crate::index::{ClasspathId, ModuleId};

use super::detection::{
    BuildToolDetector, BuildWatchInterest, DetectedBuildTool, DetectedBuildToolKind,
};
use super::model::{
    JavaToolchainInfo, ModelFidelity, ModelFreshness, SourceRootId, WorkspaceModelProvenance,
    WorkspaceModelSnapshot, WorkspaceModule, WorkspaceRoot, WorkspaceRootKind, WorkspaceSourceRoot,
};

const GRADLE_MODEL_BEGIN: &str = "JAVA_ANALYZER_MODEL_BEGIN";
const GRADLE_MODEL_END: &str = "JAVA_ANALYZER_MODEL_END";
const GRADLE_EXPORT_SCRIPT_LEGACY: &str = include_str!("gradle/export.legacy.init.gradle");
const GRADLE_EXPORT_SCRIPT_MODERN: &str = include_str!("gradle/export.modern.init.gradle");

#[derive(Debug, Clone)]
pub struct GradleVersion {
    pub raw: String,
    pub major: Option<u32>,
    pub minor: Option<u32>,
    pub patch: Option<u32>,
}

impl GradleVersion {
    fn parse(raw: impl Into<String>) -> Self {
        let raw = raw.into();
        let parts = raw
            .split(|c: char| !(c.is_ascii_digit() || c == '.'))
            .find(|part| part.chars().any(|ch| ch.is_ascii_digit()))
            .unwrap_or("")
            .split('.')
            .filter_map(|part| part.parse::<u32>().ok())
            .collect::<Vec<_>>();

        Self {
            raw,
            major: parts.first().copied(),
            minor: parts.get(1).copied(),
            patch: parts.get(2).copied(),
        }
    }

    pub fn major_or_default(&self, default: u32) -> u32 {
        self.major.unwrap_or(default)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GradleExportStrategyKind {
    Legacy,
    Modern,
}

#[derive(Debug, Clone, Copy)]
pub struct GradleExportStrategy {
    pub kind: GradleExportStrategyKind,
    pub script: &'static str,
}

impl GradleExportStrategyKind {
    pub fn as_str(self) -> &'static str {
        match self {
            GradleExportStrategyKind::Legacy => "legacy-init-script",
            GradleExportStrategyKind::Modern => "modern-init-script",
        }
    }
}

impl GradleExportStrategy {
    pub fn select(version: &GradleVersion) -> Option<Self> {
        match version.major_or_default(0) {
            4..7 => Some(Self {
                kind: GradleExportStrategyKind::Legacy,
                script: GRADLE_EXPORT_SCRIPT_LEGACY,
            }),
            v if v >= 7 => Some(Self {
                kind: GradleExportStrategyKind::Modern,
                script: GRADLE_EXPORT_SCRIPT_MODERN,
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GradleVersionProbe;

impl GradleVersionProbe {
    pub async fn probe(&self, root: &Path) -> Result<GradleVersion> {
        let executable = gradle_executable(root);
        let output = Command::new(&executable)
            .current_dir(root)
            .arg("--version")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .with_context(|| {
                format!(
                    "failed to execute Gradle version probe via {}",
                    executable.to_string_lossy()
                )
            })?;

        if !output.status.success() {
            bail!(
                "Gradle version probe failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            );
        }

        let stdout = String::from_utf8(output.stdout)
            .context("Gradle version output was not valid UTF-8")?;
        let raw = stdout
            .lines()
            .find_map(|line| line.strip_prefix("Gradle ").map(str::trim))
            .filter(|line| !line.is_empty())
            .map(str::to_string)
            .or_else(|| {
                stdout
                    .lines()
                    .find(|line| line.contains("Gradle"))
                    .map(|line| line.trim().to_string())
            })
            .ok_or_else(|| anyhow!("could not parse Gradle version from --version output"))?;

        Ok(GradleVersion::parse(raw))
    }
}

#[derive(Debug, Clone)]
pub struct GradleDetector;

impl BuildToolDetector for GradleDetector {
    fn detect(&self, root: &Path) -> Option<DetectedBuildTool> {
        let markers = [
            "settings.gradle",
            "settings.gradle.kts",
            "build.gradle",
            "build.gradle.kts",
        ];
        if markers.iter().any(|marker| root.join(marker).exists()) {
            Some(DetectedBuildTool {
                kind: DetectedBuildToolKind::Gradle,
                root: root.to_path_buf(),
                watch_interest: BuildWatchInterest {
                    file_names: vec![
                        "build.gradle",
                        "build.gradle.kts",
                        "settings.gradle",
                        "settings.gradle.kts",
                        "gradle.properties",
                        "libs.versions.toml",
                    ],
                },
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct GradleImportRequest {
    pub root: PathBuf,
    pub generation: u64,
    pub version: GradleVersion,
    pub strategy: GradleExportStrategy,
}

#[derive(Debug, Clone)]
pub struct ImportedGradleWorkspace {
    pub root: PathBuf,
    pub version: GradleVersion,
    pub export: GradleWorkspaceExport,
    pub generated_at: SystemTime,
}

#[async_trait]
pub trait WorkspaceImporter: Send + Sync {
    type Output;

    async fn import_workspace(&self, request: GradleImportRequest) -> Result<Self::Output>;
}

#[derive(Debug, Clone)]
pub struct GradleImporter;

#[async_trait]
impl WorkspaceImporter for GradleImporter {
    type Output = ImportedGradleWorkspace;

    async fn import_workspace(&self, request: GradleImportRequest) -> Result<Self::Output> {
        let executable = gradle_executable(&request.root);
        let script_file = write_gradle_script(request.strategy)?;
        tracing::debug!(
            workspace = %request.root.display(),
            generation = request.generation,
            gradle_version = %request.version.raw,
            strategy = %request.strategy.kind.as_str(),
            script_path = %script_file.path().display(),
            "running Gradle workspace import"
        );

        let output = Command::new(&executable)
            .current_dir(&request.root)
            .env(
                "JAVA_ANALYZER_GRADLE_DEBUG",
                if tracing::enabled!(tracing::Level::DEBUG) {
                    "1"
                } else {
                    "0"
                },
            )
            .arg("--no-daemon")
            .arg("--console=plain")
            .arg("-q")
            .arg("-I")
            .arg(script_file.path())
            .arg("javaAnalyzerExportModel")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .await
            .with_context(|| {
                format!(
                    "failed to execute Gradle importer via {}",
                    executable.to_string_lossy()
                )
            })?;

        if !output.status.success() {
            bail!(
                "Gradle import failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            );
        }

        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stderr.trim().is_empty() {
            tracing::debug!(
                workspace = %request.root.display(),
                stderr = %stderr.trim(),
                "Gradle importer debug stderr"
            );
        }

        let stdout = String::from_utf8(output.stdout)
            .context("Gradle importer output was not valid UTF-8")?;
        let json = extract_model_json(&stdout)?;
        let export = serde_json::from_str::<GradleWorkspaceExport>(&json)
            .context("failed to parse Gradle workspace export")?;
        tracing::debug!(
            workspace = %request.root.display(),
            projects = export.projects.len(),
            gradle_version = %request.version.raw,
            strategy = %request.strategy.kind.as_str(),
            "Gradle importer produced workspace export"
        );
        for project in &export.projects {
            tracing::info!(
                project = %project.path,
                name = %project.name,
                source_roots = ?project.source_roots,
                test_roots = ?project.test_roots,
                compile_classpath_count = project.compile_classpath.len(),
                test_classpath_count = project.test_classpath.len(),
                compile_classpath = ?project.compile_classpath,
                test_classpath = ?project.test_classpath,
                module_dependencies = ?project.module_dependencies,
                "raw Gradle import payload"
            );
        }

        Ok(ImportedGradleWorkspace {
            root: request.root,
            version: request.version,
            export,
            generated_at: SystemTime::now(),
        })
    }
}

#[derive(Debug, Clone, Default)]
pub struct GradleWorkspaceNormalizer;

impl GradleWorkspaceNormalizer {
    pub fn normalize(
        &self,
        imported: ImportedGradleWorkspace,
        generation: u64,
    ) -> Result<WorkspaceModelSnapshot> {
        tracing::debug!(
            workspace = %imported.root.display(),
            projects = imported.export.projects.len(),
            gradle_version = %imported.version.raw,
            "normalizing imported Gradle workspace"
        );
        let module_ids: BTreeMap<Arc<str>, ModuleId> = imported
            .export
            .projects
            .iter()
            .enumerate()
            .map(|(idx, project)| {
                (
                    Arc::<str>::from(project.path.as_str()),
                    ModuleId(idx as u32 + 1),
                )
            })
            .collect();

        let mut fidelity = ModelFidelity::Full;
        let modules =
            imported
                .export
                .projects
                .iter()
                .enumerate()
                .map(|(module_idx, project)| {
                    if project.compile_classpath.is_empty() {
                        fidelity = ModelFidelity::Partial;
                    }

                    tracing::debug!(
                        project = %project.path,
                        source_roots = project.source_roots.len(),
                        test_roots = project.test_roots.len(),
                        resource_roots = project.resource_roots.len(),
                        generated_roots = project.generated_roots.len(),
                        compile_classpath = project.compile_classpath.len(),
                        test_classpath = project.test_classpath.len(),
                        module_deps = project.module_dependencies.len(),
                        "Gradle importer project payload"
                    );

                    let mut raw_roots = Vec::new();
                    raw_roots.extend(project.source_roots.iter().map(|path| {
                        (path.as_str(), WorkspaceRootKind::Sources, ClasspathId::Main)
                    }));
                    raw_roots.extend(
                        project.test_roots.iter().map(|path| {
                            (path.as_str(), WorkspaceRootKind::Tests, ClasspathId::Test)
                        }),
                    );
                    raw_roots.extend(project.resource_roots.iter().map(|path| {
                        (
                            path.as_str(),
                            WorkspaceRootKind::Resources,
                            ClasspathId::Main,
                        )
                    }));
                    raw_roots.extend(project.generated_roots.iter().map(|path| {
                        (
                            path.as_str(),
                            WorkspaceRootKind::Generated,
                            ClasspathId::Main,
                        )
                    }));

                    let roots = dedupe_workspace_roots(raw_roots.into_iter().enumerate().map(
                        |(root_idx, (path, kind, classpath))| WorkspaceSourceRoot {
                            id: SourceRootId(
                                ((module_idx as u32 + 1) * 10_000) + root_idx as u32 + 1,
                            ),
                            path: normalize_path(&imported.root, path),
                            kind,
                            classpath,
                        },
                    ));

                    Ok(WorkspaceModule {
                        id: *module_ids.get(project.path.as_str()).ok_or_else(|| {
                            anyhow!("missing normalized module id for {}", project.path)
                        })?,
                        name: project.name.clone(),
                        directory: normalize_path(&imported.root, &project.project_dir),
                        roots,
                        compile_classpath: dedupe_paths(
                            project
                                .compile_classpath
                                .iter()
                                .map(|path| normalize_path(&imported.root, path)),
                        ),
                        test_classpath: dedupe_paths(
                            project
                                .test_classpath
                                .iter()
                                .map(|path| normalize_path(&imported.root, path)),
                        ),
                        dependency_modules: project
                            .module_dependencies
                            .iter()
                            .filter_map(|path| module_ids.get(path.as_str()).copied())
                            .collect(),
                        java: JavaToolchainInfo {
                            language_version: project.java_language_version.clone(),
                        },
                    })
                })
                .collect::<Result<Vec<_>>>()?;

        Ok(WorkspaceModelSnapshot {
            generation,
            root: WorkspaceRoot {
                path: imported.root.clone(),
            },
            name: imported.export.workspace_name,
            modules,
            provenance: WorkspaceModelProvenance {
                tool: DetectedBuildToolKind::Gradle,
                tool_version: Some(imported.version.raw.clone()),
                imported_at: imported.generated_at,
            },
            freshness: ModelFreshness::Fresh,
            fidelity,
        })
    }
}

fn write_gradle_script(strategy: GradleExportStrategy) -> Result<NamedTempFile> {
    let mut file = Builder::new()
        .suffix(".gradle")
        .tempfile()
        .context("failed to create temporary Gradle init script")?;
    std::io::Write::write_all(&mut file, strategy.script.as_bytes())
        .context("failed to write embedded Gradle init script")?;
    Ok(file)
}

fn gradle_executable(root: &Path) -> OsString {
    if cfg!(windows) {
        let wrapper = root.join("gradlew.bat");
        if wrapper.exists() {
            return wrapper.into_os_string();
        }
        return OsString::from("gradle.bat");
    }

    let wrapper = root.join("gradlew");
    if wrapper.exists() {
        wrapper.into_os_string()
    } else {
        OsString::from("gradle")
    }
}

fn extract_model_json(stdout: &str) -> Result<String> {
    let Some(begin) = stdout.find(GRADLE_MODEL_BEGIN) else {
        bail!("Gradle importer did not emit model start marker");
    };
    let Some(end) = stdout.find(GRADLE_MODEL_END) else {
        bail!("Gradle importer did not emit model end marker");
    };
    if end <= begin {
        bail!("Gradle importer emitted invalid model markers");
    }
    Ok(stdout[begin + GRADLE_MODEL_BEGIN.len()..end]
        .trim()
        .to_string())
}

fn normalize_path(root: &Path, raw: &str) -> PathBuf {
    let path = PathBuf::from(raw);
    if path.is_absolute() {
        path
    } else {
        root.join(path)
    }
}

fn dedupe_paths(paths: impl Iterator<Item = PathBuf>) -> Vec<PathBuf> {
    let mut deduped = BTreeSet::new();
    for path in paths {
        if path.exists() {
            deduped.insert(path);
        }
    }
    deduped.into_iter().collect()
}

fn dedupe_workspace_roots(
    roots: impl Iterator<Item = WorkspaceSourceRoot>,
) -> Vec<WorkspaceSourceRoot> {
    let mut deduped = BTreeMap::new();
    for root in roots {
        if root.path.exists() {
            deduped.entry(root.path.clone()).or_insert(root);
        }
    }
    deduped.into_values().collect()
}

#[derive(Debug, Clone, Deserialize)]
pub struct GradleWorkspaceExport {
    pub workspace_name: String,
    pub projects: Vec<GradleProjectExport>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct GradleProjectExport {
    pub path: String,
    pub name: String,
    pub project_dir: String,
    #[serde(default)]
    pub source_roots: Vec<String>,
    #[serde(default)]
    pub test_roots: Vec<String>,
    #[serde(default)]
    pub resource_roots: Vec<String>,
    #[serde(default)]
    pub generated_roots: Vec<String>,
    #[serde(default)]
    pub compile_classpath: Vec<String>,
    #[serde(default)]
    pub test_classpath: Vec<String>,
    #[serde(default)]
    pub module_dependencies: Vec<String>,
    pub java_language_version: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_gradle_workspace() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("settings.gradle.kts"), "").unwrap();

        let detected = GradleDetector.detect(dir.path()).unwrap();
        assert_eq!(detected.kind, DetectedBuildToolKind::Gradle);
        assert!(
            detected
                .watch_interest
                .matches_path(&dir.path().join("build.gradle"))
        );
    }

    #[test]
    fn extracts_marked_json() {
        let stdout = "noise\nJAVA_ANALYZER_MODEL_BEGIN\n{\"workspace_name\":\"demo\",\"projects\":[]}\nJAVA_ANALYZER_MODEL_END\n";
        let json = extract_model_json(stdout).unwrap();
        assert_eq!(json, "{\"workspace_name\":\"demo\",\"projects\":[]}");
    }

    #[test]
    fn parses_gradle_version() {
        let version = GradleVersion::parse("8.10.2");
        assert_eq!(version.major, Some(8));
        assert_eq!(version.minor, Some(10));
        assert_eq!(version.patch, Some(2));
    }

    #[test]
    fn selects_legacy_strategy_for_old_gradle() {
        let strategy = GradleExportStrategy::select(&GradleVersion::parse("4.14.1")).unwrap();
        assert_eq!(strategy.kind, GradleExportStrategyKind::Legacy);
    }
}
