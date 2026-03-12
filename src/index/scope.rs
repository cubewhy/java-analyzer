use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleId(pub u32);

impl ModuleId {
    pub const ROOT: ModuleId = ModuleId(0);
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct IndexScope {
    pub module: ModuleId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub enum ClasspathId {
    #[default]
    Main,
    Test,
}
