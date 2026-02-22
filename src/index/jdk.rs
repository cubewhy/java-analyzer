use crate::index::ClassMetadata;

pub struct JdkIndexer;

impl JdkIndexer {
    /// Try to find and index JDK classes.
    /// Returns empty vec if JAVA_HOME is not set or JDK cannot be parsed.
    pub async fn index() -> Vec<ClassMetadata> {
        if let Ok(java_home) = std::env::var("JAVA_HOME") {
            let path = std::path::PathBuf::from(java_home);
            // JDK 9+: lib/modules (jimage)
            let modules = path.join("lib").join("modules");
            if modules.exists() {
                return Self::index_jimage(modules).await;
            }
            // JDK 8: jre/lib/rt.jar
            let rt_jar = path.join("jre").join("lib").join("rt.jar");
            if rt_jar.exists() {
                return tokio::task::spawn_blocking(move || {
                    crate::index::index_jar(&rt_jar).unwrap_or_default()
                })
                .await
                .unwrap_or_default();
            }
        }
        vec![]
    }

    async fn index_jimage(path: std::path::PathBuf) -> Vec<ClassMetadata> {
        tokio::task::spawn_blocking(move || Self::index_jimage_blocking(&path))
            .await
            .unwrap_or_default()
    }

    fn index_jimage_blocking(path: &std::path::Path) -> Vec<ClassMetadata> {
        // TODO: implement with jimage_rs
        // needs answers to the path format question
        vec![]
    }
}
