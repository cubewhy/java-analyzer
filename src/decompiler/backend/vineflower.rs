use anyhow::Result;
use std::path::Path;
use std::process::Stdio;
use tokio::process::Command;
use tracing::{info, warn};

use crate::decompiler::Decompiler;

pub struct VineflowerDecompiler;

#[async_trait::async_trait]
impl Decompiler for VineflowerDecompiler {
    async fn decompile(
        &self,
        java_bin: &Path,
        decompiler_jar: &Path,
        class_data: &[u8],
        output_path: &Path,
    ) -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let input_class = temp_dir.path().join("Input.class");
        std::fs::write(&input_class, class_data)?;

        let out_dir = temp_dir.path().join("out");
        std::fs::create_dir_all(&out_dir)?;

        let output = Command::new(java_bin)
            .arg("-jar")
            .arg(decompiler_jar)
            .arg(&input_class)
            .arg(&out_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?
            .wait_with_output()
            .await?;

        if !output.status.success() {
            let err = String::from_utf8_lossy(&output.stderr);
            warn!(decompiler_error = %err, "Vineflower failed");
            return Err(anyhow::anyhow!("Decompiler failed: {}", err));
        }

        let result_file = out_dir.join("Input.java");
        std::fs::copy(result_file, output_path)?;

        info!(target = ?output_path, "Decompilation successful");
        Ok(())
    }
}
