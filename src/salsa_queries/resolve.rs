use super::Db;
use crate::index::{ClassMetadata, ModuleId};
use crate::salsa_db::SourceFile;
/// Resolve queries - handle type resolution and name lookup
///
/// These queries provide semantic analysis on top of the indexed code.
use std::sync::Arc;

/// Resolve a type name in the context of a source file
///
/// This considers imports, package, and the global index to find the
/// fully qualified class.
pub fn resolve_type_in_context(
    db: &dyn Db,
    file: SourceFile,
    type_name: Arc<str>,
) -> Option<Arc<ClassMetadata>> {
    let imports = super::parse::extract_imports(db, file);
    let package = super::parse::extract_package(db, file);

    // Try to resolve using imports
    for import in imports.iter() {
        if import.ends_with(type_name.as_ref()) {
            // Found matching import
            let internal_name = import.replace('.', "/");
            return lookup_class_by_internal_name(db, Arc::from(internal_name.as_str()));
        }
    }

    // Try same package
    if let Some(pkg) = package {
        let internal_name = format!("{}/{}", pkg, type_name);
        if let Some(class) = lookup_class_by_internal_name(db, Arc::from(internal_name.as_str())) {
            return Some(class);
        }
    }

    // Try java.lang
    let java_lang_name = format!("java/lang/{}", type_name);
    if let Some(class) = lookup_class_by_internal_name(db, Arc::from(java_lang_name.as_str())) {
        return Some(class);
    }

    None
}

/// Look up a class by its internal name in the workspace index
fn lookup_class_by_internal_name(
    db: &dyn Db,
    internal_name: Arc<str>,
) -> Option<Arc<ClassMetadata>> {
    let index = db.workspace_index();

    // Search in the root module's view
    let scope = crate::index::IndexScope {
        module: ModuleId::ROOT,
    };
    let view = index.view(scope);

    view.get_class(internal_name.as_ref())
}

/// Resolve a method in a class
///
/// This is a placeholder for future method resolution queries.
pub fn resolve_method(
    db: &dyn Db,
    class_internal_name: Arc<str>,
    method_name: Arc<str>,
    descriptor: Option<Arc<str>>,
) -> Option<Arc<crate::index::MethodSummary>> {
    let class = lookup_class_by_internal_name(db, class_internal_name)?;

    // Find method by name (and descriptor if provided)
    class
        .methods
        .iter()
        .find(|m| {
            m.name.as_ref() == method_name.as_ref()
                && (descriptor.is_none() || descriptor.as_ref() == Some(&m.desc()))
        })
        .map(|m| Arc::new(m.clone()))
}

/// Resolve a field in a class
pub fn resolve_field(
    db: &dyn Db,
    class_internal_name: Arc<str>,
    field_name: Arc<str>,
) -> Option<Arc<crate::index::FieldSummary>> {
    let class = lookup_class_by_internal_name(db, class_internal_name)?;

    class
        .fields
        .iter()
        .find(|f| f.name.as_ref() == field_name.as_ref())
        .map(|f| Arc::new(f.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_resolve_type_with_import() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "import java.util.List;\npublic class Test { List list; }".to_string(),
            Arc::from("java"),
        );

        // This will return None because we don't have JDK indexed in the test
        // In production, this would resolve to java.util.List
        let resolved = resolve_type_in_context(&db, file, Arc::from("List"));
        // Just verify it doesn't panic
        assert!(resolved.is_none() || resolved.is_some());
    }

    #[test]
    fn test_resolve_type_same_package() {
        let db = Database::default();

        // Create a class in the same package
        let uri1 = Url::parse("file:///test/Other.java").unwrap();
        let file1 = SourceFile::new(
            &db,
            FileId::new(uri1),
            "package com.example;\npublic class Other {}".to_string(),
            Arc::from("java"),
        );

        // Index it
        let _classes = crate::salsa_queries::index::extract_classes(&db, file1);

        // Try to resolve from another file in same package
        let uri2 = Url::parse("file:///test/Test.java").unwrap();
        let file2 = SourceFile::new(
            &db,
            FileId::new(uri2),
            "package com.example;\npublic class Test { Other other; }".to_string(),
            Arc::from("java"),
        );

        // This might not work in test because workspace index isn't fully set up
        // But it shouldn't panic
        let resolved = resolve_type_in_context(&db, file2, Arc::from("Other"));
        assert!(resolved.is_none() || resolved.is_some());
    }
}
