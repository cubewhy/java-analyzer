use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use tracing::{debug, warn};

use crate::index::ClassMetadata;

const CACHE_VERSION: u32 = 1;

#[derive(serde::Serialize, serde::Deserialize)]
struct CacheFile {
    version: u32,
    source_hash: u64,
    classes: Vec<ClassMetadata>,
}

fn source_hash(path: &Path) -> u64 {
    let mut h = DefaultHasher::new();
    path.hash(&mut h);
    if let Ok(meta) = std::fs::metadata(path) {
        if let Ok(modified) = meta.modified()
            && let Ok(dur) = modified.duration_since(SystemTime::UNIX_EPOCH)
        {
            dur.as_secs().hash(&mut h);
        }
        meta.len().hash(&mut h);
    }
    h.finish()
}

pub fn cache_dir() -> Option<PathBuf> {
    Some(dirs::cache_dir()?.join("java-analyzer"))
}

fn cache_path_for(source_path: &Path) -> Option<PathBuf> {
    let mut h = DefaultHasher::new();
    source_path.hash(&mut h);
    Some(cache_dir()?.join(format!("{:016x}.postcard", h.finish())))
}

pub fn load_cached(source_path: &Path) -> Option<Vec<ClassMetadata>> {
    let cache_path = cache_path_for(source_path)?;
    let expected_hash = source_hash(source_path);

    let mut file = std::fs::File::open(&cache_path).ok()?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).ok()?;

    let cached: CacheFile = postcard::from_bytes(&buf).ok()?;

    if cached.version != CACHE_VERSION || cached.source_hash != expected_hash {
        debug!(path = %cache_path.display(), "cache stale or version mismatch");
        let _ = std::fs::remove_file(&cache_path);
        return None;
    }

    debug!(
        path = %source_path.display(),
        count = cached.classes.len(),
        "loaded from cache"
    );
    Some(cached.classes)
}

pub fn save_cache(source_path: &Path, classes: &[ClassMetadata]) {
    let Some(cache_path) = cache_path_for(source_path) else {
        return;
    };
    let Some(dir) = cache_dir() else { return };

    if let Err(e) = std::fs::create_dir_all(&dir) {
        warn!(error = %e, "failed to create cache dir");
        return;
    }

    let entry = CacheFile {
        version: CACHE_VERSION,
        source_hash: source_hash(source_path),
        classes: classes.to_vec(),
    };

    match postcard::to_allocvec(&entry) {
        Ok(buf) => {
            let tmp = cache_path.with_extension("tmp");
            match std::fs::File::create(&tmp) {
                Ok(mut f) => {
                    if f.write_all(&buf).is_ok() {
                        if let Err(e) = std::fs::rename(&tmp, &cache_path) {
                            warn!(error = %e, "failed to rename cache file");
                        } else {
                            debug!(
                                path = %cache_path.display(),
                                bytes = buf.len(),
                                "cache saved"
                            );
                        }
                    }
                    let _ = std::fs::remove_file(&tmp);
                }
                Err(e) => warn!(error = %e, "failed to create cache tmp file"),
            }
        }
        Err(e) => warn!(error = %e, "failed to serialize cache"),
    }
}
