use std::sync::Arc;

use arc_swap::ArcSwap;
use parking_lot::Mutex;

use crate::index::WorkspaceIndex;

#[derive(Clone)]
pub struct WorkspaceIndexHandle {
    current: Arc<ArcSwap<WorkspaceIndex>>,
    write_serial: Arc<Mutex<()>>,
}

impl WorkspaceIndexHandle {
    pub fn new(index: WorkspaceIndex) -> Self {
        Self {
            current: Arc::new(ArcSwap::from_pointee(index)),
            write_serial: Arc::new(Mutex::new(())),
        }
    }

    pub fn load(&self) -> Arc<WorkspaceIndex> {
        self.current.load_full()
    }

    pub fn update<R>(&self, f: impl FnOnce(&WorkspaceIndex) -> R) -> R {
        let _guard = self.write_serial.lock();
        let index = self.load();
        f(index.as_ref())
    }

    pub fn replace(&self, index: WorkspaceIndex) {
        let _guard = self.write_serial.lock();
        self.current.store(Arc::new(index));
    }
}

impl Default for WorkspaceIndexHandle {
    fn default() -> Self {
        Self::new(WorkspaceIndex::new())
    }
}
