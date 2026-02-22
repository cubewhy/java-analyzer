use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info, warn};

use crate::index::parse_class_data_bytes;

use super::{ClassMetadata, index_jar};

const JDK_ORIGIN: &str = "jdk://builtin";

pub struct JdkIndexer;

impl JdkIndexer {
    /// Try to find and index JDK classes.
    /// Returns empty vec if JAVA_HOME is not set or JDK cannot be parsed.
    pub fn index() -> Vec<ClassMetadata> {
        let java_home = match std::env::var("JAVA_HOME") {
            Ok(h) => PathBuf::from(h),
            Err(_) => {
                warn!("JAVA_HOME not set, JDK classes will not be indexed");
                return vec![];
            }
        };

        info!(java_home = %java_home.display(), "indexing JDK");

        // JDK 9+: lib/modules (jimage)
        let modules = java_home.join("lib").join("modules");
        if modules.exists() {
            info!("detected JDK 9+ (jimage)");
            return Self::index_jimage(&modules);
        }

        // JDK 8: jre/lib/rt.jar
        let rt_jar = java_home.join("jre").join("lib").join("rt.jar");
        if rt_jar.exists() {
            info!("detected JDK 8 (rt.jar)");
            return index_jar(&rt_jar).unwrap_or_else(|e| {
                warn!(error = %e, "failed to index rt.jar");
                vec![]
            });
        }

        // JDK 8 alternative layout (some distros)
        let rt_jar_alt = java_home.join("lib").join("rt.jar");
        if rt_jar_alt.exists() {
            info!("detected JDK 8 alt layout (lib/rt.jar)");
            return index_jar(&rt_jar_alt).unwrap_or_else(|e| {
                warn!(error = %e, "failed to index rt.jar");
                vec![]
            });
        }

        warn!(
            java_home = %java_home.display(),
            "could not find JDK class library (neither lib/modules nor jre/lib/rt.jar)"
        );
        vec![]
    }

    fn index_jimage(path: &Path) -> Vec<ClassMetadata> {
        let origin = Arc::from(JDK_ORIGIN);

        let jimage = match jimage_rs::JImage::open(path) {
            Ok(j) => j,
            Err(e) => {
                warn!(error = %e, path = %path.display(), "failed to open jimage");
                return vec![];
            }
        };

        let mut results = Vec::new();
        let resource_names = jimage.resource_names_iter();

        for entry in resource_names {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    debug!(error = %e, "skipping jimage entry");
                    continue;
                }
            };

            let (module, path_in_module) = entry.get_full_name();

            // Only process .class files, skip module-info
            if !path_in_module.ends_with(".class") {
                continue;
            }
            if path_in_module.ends_with("module-info.class") {
                continue;
            }

            let resource_path = format!("/{}/{}", module, path_in_module);
            let bytes = match jimage.find_resource(&resource_path) {
                Ok(Some(b)) => b,
                Ok(None) => {
                    debug!(path = resource_path, "resource not found in jimage");
                    continue;
                }
                Err(e) => {
                    debug!(error = %e, path = resource_path, "failed to read jimage resource");
                    continue;
                }
            };

            // path_in_module: "java/lang/String.class"
            // file_name for parse_class_data is the path within the module
            match parse_class_data_bytes(&path_in_module, &bytes, Arc::clone(&origin)) {
                Some(meta) => results.push(meta),
                None => debug!(path = resource_path, "failed to parse class"),
            }
        }

        info!(count = results.len(), "JDK jimage indexed");
        results
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Only runs if JAVA_HOME is set and valid.
    /// Use: JAVA_HOME=/path/to/jdk cargo test test_jdk_index -- --nocapture
    #[test]
    fn test_jdk_index_finds_string() {
        let classes = JdkIndexer::index();
        if classes.is_empty() {
            eprintln!("JAVA_HOME not set or JDK not found, skipping");
            return;
        }
        eprintln!("JDK classes indexed: {}", classes.len());

        let string_class = classes
            .iter()
            .find(|c| c.internal_name.as_ref() == "java/lang/String");
        assert!(
            string_class.is_some(),
            "java/lang/String should be in JDK index. \
             First 10 classes: {:?}",
            classes
                .iter()
                .take(10)
                .map(|c| c.internal_name.as_ref())
                .collect::<Vec<_>>()
        );

        let string = string_class.unwrap();
        assert_eq!(string.package.as_deref(), Some("java/lang"));
        assert!(
            string.methods.iter().any(|m| m.name.as_ref() == "length"),
            "String should have length() method"
        );
        assert!(
            string
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "substring"),
            "String should have substring() method"
        );
    }

    #[test]
    fn test_jdk_index_object_methods() {
        let classes = JdkIndexer::index();
        if classes.is_empty() {
            return;
        }
        let object = classes
            .iter()
            .find(|c| c.internal_name.as_ref() == "java/lang/Object");
        assert!(object.is_some(), "java/lang/Object should be indexed");
        let object = object.unwrap();
        for method in &["equals", "hashCode", "toString"] {
            assert!(
                object.methods.iter().any(|m| m.name.as_ref() == *method),
                "Object should have {}()",
                method
            );
        }
    }

    #[test]
    fn test_jdk_skips_module_info() {
        let classes = JdkIndexer::index();
        assert!(
            classes.iter().all(|c| c.name.as_ref() != "module-info"),
            "module-info should be filtered out"
        );
    }

    #[test]
    fn test_jdk_classes_have_valid_internal_names() {
        let classes = JdkIndexer::index();
        for cls in classes.iter().take(100) {
            assert!(
                !cls.internal_name.contains('.'),
                "internal_name should use '/' not '.': {}",
                cls.internal_name
            );
        }
    }
}
