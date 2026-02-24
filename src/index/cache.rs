use crate::index::{ClassMetadata, ClassOrigin};
use rusqlite::{Connection, OptionalExtension, params};
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::SystemTime;
use tracing::{debug, warn};

const CACHE_VERSION: u32 = 2;

fn intern_str(s: &str) -> Arc<str> {
    static POOL: OnceLock<Mutex<HashSet<Arc<str>>>> = OnceLock::new();
    let mut pool = POOL
        .get_or_init(|| Mutex::new(HashSet::new()))
        .lock()
        .unwrap();
    if let Some(arc) = pool.get(s) {
        Arc::clone(arc)
    } else {
        let arc: Arc<str> = Arc::from(s);
        pool.insert(Arc::clone(&arc));
        arc
    }
}

fn intern_metadata(meta: &mut ClassMetadata) {
    if let Some(pkg) = &meta.package {
        meta.package = Some(intern_str(pkg));
    }
    meta.name = intern_str(&meta.name);
    meta.internal_name = intern_str(&meta.internal_name);

    for m in &mut meta.methods {
        m.name = intern_str(&m.name);
        m.descriptor = intern_str(&m.descriptor);
        if let Some(gs) = &m.generic_signature {
            m.generic_signature = Some(intern_str(gs));
        }
        if let Some(rt) = &m.return_type {
            m.return_type = Some(intern_str(rt));
        }
    }

    for f in &mut meta.fields {
        f.name = intern_str(&f.name);
        f.descriptor = intern_str(&f.descriptor);
    }

    if let Some(ic) = &meta.inner_class_of {
        meta.inner_class_of = Some(intern_str(ic));
    }

    match &mut meta.origin {
        ClassOrigin::Jar(j) => *j = intern_str(j),
        ClassOrigin::SourceFile(s) => *s = intern_str(s),
        ClassOrigin::Unknown => {}
    }
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

fn get_db_path() -> Option<PathBuf> {
    let dir = dirs::cache_dir()?.join("java-analyzer");
    if std::fs::create_dir_all(&dir).is_err() {
        warn!("failed to create cache dir");
    }
    Some(dir.join("index.db"))
}

fn get_connection() -> rusqlite::Result<Connection> {
    let path = get_db_path().ok_or(rusqlite::Error::InvalidPath(PathBuf::from("No cache dir")))?;
    let conn = Connection::open(&path)?;

    conn.busy_timeout(std::time::Duration::from_secs(5))?;

    conn.execute_batch(
        "
        PRAGMA journal_mode = WAL;
        PRAGMA synchronous = NORMAL;
        PRAGMA temp_store = MEMORY;
        CREATE TABLE IF NOT EXISTS file_cache (
            path TEXT PRIMARY KEY,
            version INTEGER NOT NULL,
            hash INTEGER NOT NULL,
            data BLOB NOT NULL
        );
    ",
    )?;
    Ok(conn)
}

pub fn load_cached(source_path: &Path) -> Option<Vec<ClassMetadata>> {
    let expected_hash = source_hash(source_path);
    let path_str = source_path.to_string_lossy().to_string();

    let conn = get_connection().ok()?;
    let mut stmt = conn
        .prepare("SELECT version, hash, data FROM file_cache WHERE path = ?")
        .ok()?;

    let row = stmt
        .query_row(params![path_str], |row| {
            Ok((
                row.get::<_, u32>(0)?,
                row.get::<_, i64>(1)? as u64,
                row.get::<_, Vec<u8>>(2)?,
            ))
        })
        .optional()
        .ok()??;

    let (version, hash, data) = row;

    if version != CACHE_VERSION || hash != expected_hash {
        debug!(path = %path_str, "cache stale or version mismatch");
        let _ = conn.execute("DELETE FROM file_cache WHERE path = ?", params![path_str]);
        return None;
    }

    let mut classes: Vec<ClassMetadata> = postcard::from_bytes(&data).ok()?;

    for c in &mut classes {
        intern_metadata(c);
    }

    debug!(
        path = %path_str,
        count = classes.len(),
        "loaded from sqlite cache"
    );
    Some(classes)
}

pub fn save_cache(source_path: &Path, classes: &[ClassMetadata]) {
    let Some(conn) = get_connection().ok() else {
        return;
    };
    let hash = source_hash(source_path);
    let path_str = source_path.to_string_lossy().to_string();

    let data = match postcard::to_allocvec(classes) {
        Ok(d) => d,
        Err(e) => {
            warn!(error = %e, "failed to serialize classes");
            return;
        }
    };

    match conn.execute(
        "INSERT OR REPLACE INTO file_cache (path, version, hash, data) VALUES (?, ?, ?, ?)",
        params![path_str, CACHE_VERSION, hash as i64, data],
    ) {
        Ok(_) => debug!(path = %path_str, classes = classes.len(), "saved to sqlite cache"),
        Err(e) => warn!(error = %e, "failed to save sqlite cache"),
    }
}
