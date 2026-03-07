use anyhow::{Context, Result};
use rust_asm::class_reader::ClassReader;
use std::path::Path;
use std::process::Stdio;
use tokio::process::Command;

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
        // parse class metadata
        let cr = ClassReader::new(class_data);
        let cn = cr.to_class_node()?;
        let simple_name = cn.name.rsplit_once("/").context("Bad class name")?.1;

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
            .spawn()
            .context("Failed to spawn Vineflower process")?
            .wait_with_output()
            .await?;

        if !output.status.success() {
            let err = String::from_utf8_lossy(&output.stderr);
            tracing::warn!(decompiler_error = %err, "Vineflower failed");
            return Err(anyhow::anyhow!("Decompiler failed: {}", err));
        }

        let result_file = out_dir.join(format!("{simple_name}.java"));
        tracing::info!(?result_file);
        for entry in walkdir::WalkDir::new(&out_dir) {
            let entry = entry?;
            tracing::debug!(path = ?entry.path(), "decompiler output");
        }
        std::fs::copy(result_file, output_path).context("Output not found")?;

        tracing::info!(target = ?output_path, "Decompilation successful");
        Ok(())
    }
}
