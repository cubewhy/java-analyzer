use std::sync::Arc;

use ropey::Rope;
use tower_lsp::lsp_types::Url;
use tree_sitter::{Node, Tree};

use crate::syntax::SyntaxSnapshot;

/// A versioned, fully-parsed snapshot of a single source file.
///
/// `SourceFile` is the canonical input value for all language-analysis
/// operations.  It is cheap to clone (the heavy fields — text, rope, tree —
/// are stored behind an `Arc`) and intentionally immutable after construction:
/// a new `SourceFile` is created on every document change.
///
/// # Relationship to `Document`
///
/// `Document` (in `workspace::document`) wraps `Arc<SourceFile>` and adds
/// LSP-level mutable state (semantic-token cache, etc.) that must survive
/// across `SourceFile` replacements.
///
/// # Language neutrality
///
/// `SourceFile` carries no language-specific logic.  The `language_id` field
/// is a plain string tag (`"java"`, `"kotlin"`, …) used by the
/// `LanguageRegistry` to route requests to the right `Language` impl.
#[derive(Clone)]
pub struct SourceFile {
    /// The document URI.
    pub uri: Arc<Url>,

    /// LSP language identifier (e.g. `"java"`, `"kotlin"`).
    pub language_id: Arc<str>,

    /// Monotonically increasing LSP document version.
    pub version: i32,

    /// Full source text.
    pub text: Arc<str>,

    /// Rope view over `text` for efficient line/column ↔ byte offset
    /// conversion without re-allocating the source string.
    pub rope: Arc<Rope>,

    /// Parsed syntax tree produced by tree-sitter.  `None` only when the
    /// parser failed to produce a tree (should be rare in practice because
    /// tree-sitter is error-tolerant).
    pub tree: Option<Arc<Tree>>,

    /// Unified immutable syntax snapshot derived from the parser output.
    pub syntax: Option<Arc<SyntaxSnapshot>>,
}

impl SourceFile {
    /// Construct a `SourceFile` from raw parts, building the `Rope` in place.
    ///
    /// `tree` may be `None` when the caller has not yet parsed the file or
    /// when parsing failed.
    pub fn new(
        uri: Url,
        language_id: impl Into<Arc<str>>,
        version: i32,
        text: impl Into<Arc<str>>,
        tree: Option<Tree>,
    ) -> Self {
        let text: Arc<str> = text.into();
        let language_id: Arc<str> = language_id.into();
        let rope = Arc::new(Rope::from_str(&text));
        let syntax = tree
            .as_ref()
            .map(|tree| Arc::new(SyntaxSnapshot::from_tree(language_id.as_ref(), &text, tree)));
        Self {
            uri: Arc::new(uri),
            language_id,
            version,
            rope,
            tree: tree.map(Arc::new),
            syntax,
            text,
        }
    }

    /// Convenience: source text as a `&str`.
    #[inline]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Convenience: source text as a byte slice.
    #[inline]
    pub fn bytes(&self) -> &[u8] {
        self.text.as_bytes()
    }

    /// Return the root node of the syntax tree, if available.
    #[inline]
    pub fn root_node(&self) -> Option<Node<'_>> {
        self.tree.as_deref().map(|t| t.root_node())
    }

    /// Produce a new `SourceFile` that is identical to `self` except the
    /// `tree` field is replaced.  Used by the server after an incremental
    /// re-parse to attach the updated tree without cloning the source text.
    pub fn with_tree(self, tree: Option<Tree>) -> Self {
        let syntax = tree.as_ref().map(|tree| {
            Arc::new(SyntaxSnapshot::from_tree(
                self.language_id.as_ref(),
                &self.text,
                tree,
            ))
        });
        Self {
            tree: tree.map(Arc::new),
            syntax,
            ..self
        }
    }

    #[inline]
    pub fn syntax(&self) -> Option<&Arc<SyntaxSnapshot>> {
        self.syntax.as_ref()
    }

    #[inline]
    pub fn has_unified_syntax(&self) -> bool {
        self.syntax.is_some()
    }
}

impl std::fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("uri", &self.uri)
            .field("language_id", &self.language_id)
            .field("version", &self.version)
            .field("text_len", &self.text.len())
            .field("has_tree", &self.tree.is_some())
            .field("has_syntax", &self.syntax.is_some())
            .finish()
    }
}
