use serde::{Deserialize, Serialize};

use super::detection::DetectedBuildToolKind;
use super::model::{ModelFidelity, ModelFreshness};

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum BuildCapability {
    Unsupported,
    Supported,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum BuildReloadState {
    Unmanaged,
    Idle,
    Debouncing,
    Importing,
    Stale,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildIntegrationStatus {
    pub detected_tool: Option<DetectedBuildToolKind>,
    pub tool_version: Option<String>,
    pub capability: BuildCapability,
    pub reload_state: BuildReloadState,
    pub generation: u64,
    pub freshness: Option<ModelFreshness>,
    pub fidelity: Option<ModelFidelity>,
    pub last_error: Option<String>,
}

impl Default for BuildIntegrationStatus {
    fn default() -> Self {
        Self {
            detected_tool: None,
            tool_version: None,
            capability: BuildCapability::Unsupported,
            reload_state: BuildReloadState::Unmanaged,
            generation: 0,
            freshness: None,
            fidelity: None,
            last_error: None,
        }
    }
}
