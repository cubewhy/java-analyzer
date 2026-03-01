use super::super::Decompiler;
use anyhow::{Result, anyhow};
use async_trait::async_trait;
use std::path::Path;
use std::process::Stdio;
use tokio::process::Command;
use tracing::instrument;

pub struct CfrDecompiler;

#[async_trait]
impl Decompiler for CfrDecompiler {
    #[instrument(skip(self, class_data), fields(class_size = class_data.len()))]
    async fn decompile(
        &self,
        java_bin: &Path,
        decompiler_jar: &Path,
        class_data: &[u8],
        output_path: &Path,
    ) -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let input_class = temp_dir.path().join("Input.class");
        tokio::fs::write(&input_class, class_data).await?;

        tracing::info!(?decompiler_jar, "Executing CFR decompiler");

        let output = Command::new(java_bin)
            .arg("-jar")
            .arg(decompiler_jar)
            .arg(&input_class)
            // .arg("--comments")
            // .arg("false")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?
            .wait_with_output()
            .await?;

        if !output.status.success() {
            let err_msg = String::from_utf8_lossy(&output.stderr);
            tracing::error!(error = %err_msg, "CFR failed to decompile");
            return Err(anyhow!("CFR error: {}", err_msg));
        }

        // CFR will print the results to stdout by default (if --outputdir is not specified).
        let decompiled_code = String::from_utf8(output.stdout)
            .map_err(|e| anyhow!("Failed to parse CFR output as UTF-8: {}", e))?;

        if decompiled_code.trim().is_empty() {
            return Err(anyhow!("CFR returned empty output"));
        }

        // write result to cache
        tokio::fs::write(output_path, decompiled_code).await?;

        tracing::info!(?output_path, "Successfully cached decompiled code");
        Ok(())
    }
}
