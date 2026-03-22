/// Salsa-based incremental computation database for multi-language analysis
///
/// This module provides the foundation for incremental parsing and analysis
/// using the Salsa framework. It's designed to integrate with the existing
/// Language trait and workspace infrastructure.
use std::collections::HashMap;
use std::sync::Arc;
use tower_lsp::lsp_types::Url;
use tree_sitter::Tree;

use crate::index::WorkspaceIndexHandle;

/// File identifier - wraps a URI for type safety
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileId(Arc<Url>);

impl FileId {
    pub fn new(uri: Url) -> Self {
        Self(Arc::new(uri))
    }

    pub fn from_arc(uri: Arc<Url>) -> Self {
        Self(uri)
    }

    pub fn uri(&self) -> &Url {
        &self.0
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// Input: A source file with its URI, content, and language
///
/// This is the primary input to the Salsa system. When the content changes,
/// all derived queries will be automatically invalidated.
#[salsa::input]
pub struct SourceFile {
    /// File URI
    pub file_id: FileId,

    /// The source code content
    #[returns(ref)]
    pub content: String,

    /// Language identifier (e.g., "java", "kotlin")
    pub language_id: Arc<str>,
}

/// Input: A module in the workspace
///
/// Represents a compilation unit (e.g., a Gradle module, Maven module, etc.)
#[salsa::input]
pub struct Module {
    /// Module identifier
    pub id: crate::index::ModuleId,

    /// Module name
    pub name: Arc<str>,

    /// Source files in this module
    #[returns(ref)]
    pub source_files: Vec<SourceFile>,

    /// Dependencies (other modules this module depends on)
    #[returns(ref)]
    pub dependencies: Vec<Module>,
}

/// Input: A JAR file dependency
#[salsa::input]
pub struct JarFile {
    /// Path to the JAR file
    pub path: Arc<str>,
}

/// Latest parse snapshot retained for incremental tree-sitter reparses.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseTreeOrigin {
    Seeded,
    Full,
    Incremental,
}

/// Latest parse snapshot retained for incremental tree-sitter reparses.
#[derive(Clone)]
pub struct ParseTreeSnapshot {
    pub content: Arc<str>,
    pub language_id: Arc<str>,
    pub tree: Tree,
    pub origin: ParseTreeOrigin,
}

/// Tracked: Parsed syntax tree metadata for a source file
///
/// We store metadata about the tree rather than the tree itself,
/// since tree-sitter Tree doesn't implement the required Salsa traits.
#[salsa::tracked]
pub struct ParsedTree<'db> {
    /// The source file this tree was parsed from
    pub source: SourceFile,

    /// Root node kind (for debugging)
    pub root_kind: Arc<str>,

    /// Whether the tree has errors
    pub has_error: bool,

    /// Language of the parsed tree
    pub language_id: Arc<str>,
}

/// Tracked: Package/namespace declaration
#[salsa::tracked]
pub struct PackageInfo<'db> {
    pub source: SourceFile,

    /// Package name (Java: com/example, Kotlin: com.example)
    pub package: Option<Arc<str>>,
}

/// Tracked: Import declarations
#[salsa::tracked]
pub struct ImportsInfo<'db> {
    pub source: SourceFile,

    /// List of imports
    #[returns(ref)]
    pub imports: Vec<Arc<str>>,

    /// Static imports (Java only)
    #[returns(ref)]
    pub static_imports: Vec<Arc<str>>,
}

/// Tracked: Parsed classes/types from a source file
///
/// We only store the count here to avoid Hash requirements on ClassMetadata
#[salsa::tracked]
pub struct ParsedClasses<'db> {
    pub source: SourceFile,

    /// Number of classes parsed (for change detection)
    pub class_count: usize,
}

/// Tracked: All classes visible in a module (including dependencies)
#[salsa::tracked]
pub struct ModuleClassIndex<'db> {
    pub module: Module,

    /// Number of classes in this module (for debugging)
    pub class_count: usize,
}

/// Default database implementation
#[salsa::db]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
    /// Reference to the workspace index for queries
    workspace_index: Option<WorkspaceIndexHandle>,
    parse_trees: parking_lot::RwLock<HashMap<FileId, ParseTreeSnapshot>>,
}

impl Database {
    /// Create a new database with a workspace index reference
    pub fn with_workspace_index(workspace_index: WorkspaceIndexHandle) -> Self {
        Self {
            storage: Default::default(),
            workspace_index: Some(workspace_index),
            parse_trees: Default::default(),
        }
    }

    pub fn cached_parse_tree(&self, file_id: &FileId) -> Option<ParseTreeSnapshot> {
        self.parse_trees.read().get(file_id).cloned()
    }

    pub fn store_parse_tree(&self, file_id: FileId, snapshot: ParseTreeSnapshot) {
        self.parse_trees.write().insert(file_id, snapshot);
    }

    pub fn remove_parse_tree(&self, file_id: &FileId) {
        self.parse_trees.write().remove(file_id);
    }
}

#[salsa::db]
impl salsa::Database for Database {}

/// Implement the Db trait for query access to workspace index
#[salsa::db]
impl crate::salsa_queries::Db for Database {
    fn workspace_index(&self) -> Arc<crate::index::WorkspaceIndex> {
        self.workspace_index
            .clone()
            .unwrap_or_else(|| {
                // Fallback: create a temporary empty index
                // This should only happen in tests
                WorkspaceIndexHandle::new(crate::index::WorkspaceIndex::new())
            })
            .load()
    }

    fn cached_parse_tree(&self, file_id: &FileId) -> Option<ParseTreeSnapshot> {
        Database::cached_parse_tree(self, file_id)
    }

    fn store_parse_tree(&self, file_id: FileId, snapshot: ParseTreeSnapshot) {
        Database::store_parse_tree(self, file_id, snapshot);
    }

    fn remove_parse_tree(&self, file_id: &FileId) {
        Database::remove_parse_tree(self, file_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    #[test]
    fn test_database_creation() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri.clone()),
            "public class Test {}".to_string(),
            Arc::from("java"),
        );

        assert_eq!(file.file_id(&db).uri(), &uri);
        assert_eq!(file.content(&db), "public class Test {}");
        assert_eq!(file.language_id(&db).as_ref(), "java");
    }

    #[test]
    fn test_input_mutation() {
        let mut db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let file = SourceFile::new(
            &db,
            FileId::new(uri),
            "public class Test {}".to_string(),
            Arc::from("java"),
        );

        // Mutate the content
        file.set_content(&mut db)
            .to("public class Test { int x; }".to_string());

        assert_eq!(file.content(&db), "public class Test { int x; }");
    }

    #[test]
    fn test_module_creation() {
        let db = Database::default();
        let module = Module::new(
            &db,
            crate::index::ModuleId::ROOT,
            Arc::from("root"),
            vec![],
            vec![],
        );

        assert_eq!(module.name(&db).as_ref(), "root");
        assert_eq!(module.source_files(&db).len(), 0);
    }
}
