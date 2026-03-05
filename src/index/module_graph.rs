use std::collections::HashMap;

use crate::index::ModuleId;

pub struct ModuleGraph {
    version: u64,
    deps: HashMap<ModuleId, Vec<ModuleId>>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self {
            version: 0,
            deps: HashMap::new(),
        }
    }

    pub fn set_deps(&mut self, module: ModuleId, deps: Vec<ModuleId>) {
        self.deps.insert(module, deps);
        self.version = self.version.wrapping_add(1);
    }

    pub fn deps_of(&self, module: ModuleId) -> &[ModuleId] {
        self.deps.get(&module).map(|v| v.as_slice()).unwrap_or(&[])
    }

    pub fn version(&self) -> u64 {
        self.version
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}
