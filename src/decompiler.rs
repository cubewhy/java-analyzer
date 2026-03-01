use anyhow::Result;
use std::path::{Path, PathBuf};

pub mod backend;
pub mod cache;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecompilerBackend {
    Vineflower,
    Cfr,
}

pub struct DecompilerOptions {
    pub java_home: PathBuf,
    pub decompiler_jar: PathBuf,
    pub backend: DecompilerBackend,
}

#[async_trait::async_trait]
pub trait Decompiler: Send + Sync {
    /// Perform the decompilation task
    /// class_path: The path to the .class files on disk or a temporary extraction path
    /// output_dir: The output directory for the decompiled results
    async fn decompile(
        &self,
        java_bin: &Path,
        decompiler_jar: &Path,
        class_data: &[u8],
        output_path: &Path,
    ) -> Result<()>;
}
