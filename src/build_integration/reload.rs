use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use tokio::sync::{RwLock, mpsc};
use tokio::task::JoinHandle;
use tower_lsp::Client;
use tower_lsp::lsp_types::MessageType;

use crate::build_integration::detection::BuildToolDetector;
use crate::build_integration::progress::ImportProgress;
use crate::workspace::Workspace;

use super::detection::{BuildWatchInterest, DetectedBuildTool, DetectedBuildToolKind};
use super::gradle::{
    GradleDetector, GradleExportStrategy, GradleImportRequest, GradleImporter, GradleVersionProbe,
    GradleWorkspaceNormalizer, WorkspaceImporter,
};
use super::status::{BuildCapability, BuildIntegrationStatus, BuildReloadState};

const DEFAULT_RELOAD_DEBOUNCE: Duration = Duration::from_millis(900);

#[derive(Debug, Clone)]
pub enum ReloadReason {
    Initialize,
    FileChanged(PathBuf),
    Manual,
}

enum ReloadCommand {
    Trigger(ReloadReason),
}

#[derive(Clone)]
pub struct BuildIntegrationService {
    root: PathBuf,
    tx: mpsc::UnboundedSender<ReloadCommand>,
    status: Arc<RwLock<BuildIntegrationStatus>>,
    watch_interest: Arc<RwLock<Option<BuildWatchInterest>>>,
}

impl BuildIntegrationService {
    pub fn new(root: PathBuf, workspace: Arc<Workspace>, client: Client) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let status = Arc::new(RwLock::new(BuildIntegrationStatus::default()));
        let watch_interest = Arc::new(RwLock::new(None));

        tokio::spawn(run_reload_loop(
            root.clone(),
            workspace,
            client,
            rx,
            Arc::clone(&status),
            Arc::clone(&watch_interest),
        ));

        Self {
            root,
            tx,
            status,
            watch_interest,
        }
    }

    pub fn schedule_reload(&self, reason: ReloadReason) {
        let _ = self.tx.send(ReloadCommand::Trigger(reason));
    }

    pub async fn notify_paths_changed<I>(&self, paths: I)
    where
        I: IntoIterator<Item = PathBuf>,
    {
        let watch_interest = self.watch_interest.read().await.clone();
        for path in paths {
            let is_relevant = watch_interest
                .as_ref()
                .map(|interest| interest.matches_path(&path))
                .unwrap_or_else(|| is_known_build_file(&path));
            if is_relevant {
                self.schedule_reload(ReloadReason::FileChanged(path));
            }
        }
    }

    pub async fn status(&self) -> BuildIntegrationStatus {
        self.status.read().await.clone()
    }

    pub fn root(&self) -> &Path {
        &self.root
    }
}

async fn run_reload_loop(
    root: PathBuf,
    workspace: Arc<Workspace>,
    client: Client,
    mut rx: mpsc::UnboundedReceiver<ReloadCommand>,
    status: Arc<RwLock<BuildIntegrationStatus>>,
    watch_interest: Arc<RwLock<Option<BuildWatchInterest>>>,
) {
    let detector: Arc<dyn BuildToolDetector> = Arc::new(GradleDetector);
    let importer = Arc::new(GradleImporter);
    let version_probe = Arc::new(GradleVersionProbe);
    let normalizer = Arc::new(GradleWorkspaceNormalizer);

    let mut generation = 0_u64;
    let mut dirty = false;
    let mut debounce: Option<tokio::time::Instant> = None;
    let mut in_flight: Option<
        JoinHandle<Result<crate::build_integration::WorkspaceModelSnapshot>>,
    > = None;

    loop {
        let debounce_sleep = async {
            if let Some(deadline) = debounce {
                tokio::time::sleep_until(deadline).await;
            } else {
                std::future::pending::<()>().await;
            }
        };

        tokio::select! {
            Some(command) = rx.recv() => {
                match command {
                    ReloadCommand::Trigger(reason) => {
                        dirty = true;
                        debounce = Some(tokio::time::Instant::now() + DEFAULT_RELOAD_DEBOUNCE);
                        let mut guard = status.write().await;
                        guard.reload_state = if in_flight.is_some() {
                            BuildReloadState::Importing
                        } else {
                            BuildReloadState::Debouncing
                        };
                        if let ReloadReason::FileChanged(path) = &reason {
                            tracing::debug!(path = %path.display(), "build-relevant file change queued");
                        }
                    }
                }
            }
            _ = debounce_sleep, if debounce.is_some() && in_flight.is_none() => {
                if !dirty {
                    debounce = None;
                    continue;
                }

                dirty = false;
                debounce = None;
                generation = generation.wrapping_add(1);

                let detected = detector.detect(&root);
                publish_detection_status(&status, &watch_interest, detected.as_ref()).await;

                let Some(detected) = detected else {
                    let mut guard = status.write().await;
                    guard.capability = BuildCapability::Unsupported;
                    guard.reload_state = BuildReloadState::Unmanaged;
                    guard.detected_tool = None;
                    guard.last_error = None;
                    drop(guard);
                    if let Err(err) = workspace.index_fallback_root(root.clone()).await {
                        let mut guard = status.write().await;
                        guard.reload_state = BuildReloadState::Failed;
                        guard.last_error = Some(err.to_string());
                        client.log_message(MessageType::ERROR, format!("Fallback indexing failed: {err:#}")).await;
                    } else {
                        client.semantic_tokens_refresh().await.ok();
                    }
                    continue;
                };

                let importer = Arc::clone(&importer);
                let version_probe = Arc::clone(&version_probe);
                let normalizer = Arc::clone(&normalizer);
                let root = root.clone();
                let progress_client = client.clone();
                let status_for_task = Arc::clone(&status);

                {
                    let mut guard = status.write().await;
                    guard.capability = BuildCapability::Supported;
                    guard.detected_tool = Some(DetectedBuildToolKind::Gradle);
                    guard.tool_version = None;
                    guard.reload_state = BuildReloadState::Importing;
                    guard.last_error = None;
                }

                client
                    .log_message(MessageType::INFO, "Importing Gradle workspace")
                    .await;

                in_flight = Some(tokio::spawn(async move {
                    let progress = ImportProgress::begin(
                        progress_client.clone(),
                        format!("java-analyzer/build-import/{generation}"),
                        "Importing Gradle workspace",
                        "Detecting build state",
                    )
                    .await?;
                    let outcome = async {
                        progress.report("Probing Gradle version").await;
                        let version = version_probe.probe(&root).await?;
                        {
                            let mut guard = status_for_task.write().await;
                            guard.tool_version = Some(version.raw.clone());
                        }
                        let strategy = GradleExportStrategy::select(&version).context(format!("Gradle {} is not supported", version.raw))?;
                        progress
                            .report(&format!(
                                "Running Gradle import with Gradle {} ({})",
                                version.raw,
                                strategy.kind.as_str()
                            ))
                            .await;
                        let result = importer
                            .import_workspace(GradleImportRequest {
                                root,
                                generation,
                                version,
                                strategy,
                            })
                            .await?;
                        progress.report("Normalizing workspace model").await;
                        normalizer.normalize(result, generation)
                    }
                    .await;

                    if outcome.is_ok() {
                        progress.finish("Gradle import complete").await;
                    } else {
                        progress.finish("Gradle import failed").await;
                    }

                    outcome
                }));

                let _ = detected;
            }
            result = async { in_flight.as_mut().unwrap().await }, if in_flight.is_some() => {
                in_flight = None;
                match result {
                    Ok(Ok(snapshot)) => {
                        let version = snapshot.provenance.tool_version.clone();
                        let progress = ImportProgress::begin(
                            client.clone(),
                            format!("java-analyzer/build-apply/{}", snapshot.generation),
                            "Applying workspace model",
                            "Applying imported workspace model",
                        )
                        .await
                        .ok();
                        if let Err(err) = workspace.apply_workspace_model(snapshot.clone()).await {
                            let mut guard = status.write().await;
                            guard.reload_state = BuildReloadState::Failed;
                            guard.last_error = Some(err.to_string());
                            workspace.mark_model_stale().await;
                            if let Some(progress) = progress {
                                progress.finish("Workspace model apply failed").await;
                            }
                            client.log_message(MessageType::ERROR, format!("Workspace reload failed: {err:#}")).await;
                        } else {
                            let mut guard = status.write().await;
                            guard.capability = BuildCapability::Supported;
                            guard.detected_tool = Some(snapshot.provenance.tool);
                            guard.tool_version = version;
                            guard.reload_state = if dirty { BuildReloadState::Debouncing } else { BuildReloadState::Idle };
                            guard.generation = snapshot.generation;
                            guard.freshness = Some(snapshot.freshness);
                            guard.fidelity = Some(snapshot.fidelity);
                            guard.last_error = None;
                            if let Some(progress) = progress {
                                progress.finish("Workspace model applied").await;
                            }
                            client.semantic_tokens_refresh().await.ok();
                        }
                    }
                    Ok(Err(err)) => {
                        workspace.mark_model_stale().await;
                        let mut guard = status.write().await;
                        guard.reload_state = BuildReloadState::Failed;
                        guard.last_error = Some(err.to_string());
                        client.log_message(MessageType::ERROR, format!("Workspace import failed: {err:#}")).await;
                    }
                    Err(err) => {
                        workspace.mark_model_stale().await;
                        let mut guard = status.write().await;
                        guard.reload_state = BuildReloadState::Failed;
                        guard.last_error = Some(err.to_string());
                        client.log_message(MessageType::ERROR, format!("Workspace import task failed: {err}")).await;
                    }
                }

                if dirty {
                    debounce = Some(tokio::time::Instant::now() + DEFAULT_RELOAD_DEBOUNCE);
                    let mut guard = status.write().await;
                    guard.reload_state = BuildReloadState::Debouncing;
                }
            }
        }
    }
}

async fn publish_detection_status(
    status: &Arc<RwLock<BuildIntegrationStatus>>,
    watch_interest: &Arc<RwLock<Option<BuildWatchInterest>>>,
    detection: Option<&DetectedBuildTool>,
) {
    *watch_interest.write().await = detection.map(|tool| tool.watch_interest.clone());

    let mut guard = status.write().await;
    guard.detected_tool = detection.map(|tool| tool.kind);
    if detection.is_none() {
        guard.tool_version = None;
    }
    guard.capability = if detection.is_some() {
        BuildCapability::Supported
    } else {
        BuildCapability::Unsupported
    };
}

fn is_known_build_file(path: &Path) -> bool {
    matches!(
        path.file_name().and_then(|name| name.to_str()),
        Some("build.gradle")
            | Some("build.gradle.kts")
            | Some("settings.gradle")
            | Some("settings.gradle.kts")
            | Some("gradle.properties")
            | Some("libs.versions.toml")
    )
}
