use dashmap::DashMap;
use ropey::Rope;
use tower_lsp::lsp_types::{SemanticToken, Url};
use tree_sitter::Tree;

#[derive(Debug)]
pub struct Document {
    pub uri: Url,
    pub language_id: String,
    pub version: i32,

    pub text: String,
    pub rope: Rope,

    /// Cached tree for this doc's language (java/kotlin)
    pub tree: Option<Tree>,

    /// Cached semantic token result: (result_id, flat SemanticToken data).
    /// Invalidated whenever the document text changes.
    pub semantic_token_cache: Option<(String, Vec<SemanticToken>)>,
}

impl Document {
    pub fn new(uri: Url, language_id: String, version: i32, content: String) -> Self {
        let rope = Rope::from_str(&content);
        Self {
            uri,
            language_id,
            version,
            text: content,
            rope,
            tree: None,
            semantic_token_cache: None,
        }
    }

    pub fn apply_full_change(&mut self, version: i32, new_content: String) {
        self.version = version;
        self.text = new_content;
        self.rope = Rope::from_str(&self.text);
        self.tree = None;
        self.semantic_token_cache = None;
    }
}

pub struct DocumentStore {
    docs: DashMap<Url, Document>,
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            docs: DashMap::new(),
        }
    }

    pub fn open(&self, doc: Document) {
        self.docs.insert(doc.uri.clone(), doc);
    }

    pub fn update(&self, uri: &Url, version: i32, content: String) {
        if let Some(mut doc) = self.docs.get_mut(uri) {
            doc.apply_full_change(version, content);
        }
    }

    pub fn close(&self, uri: &Url) {
        self.docs.remove(uri);
    }

    /// Read-only access without cloning the whole doc
    pub fn with_doc<R>(&self, uri: &Url, f: impl FnOnce(&Document) -> R) -> Option<R> {
        self.docs.get(uri).map(|d| f(&d))
    }

    /// Mutable access without cloning; do NOT .await inside f
    pub fn with_doc_mut<R>(&self, uri: &Url, f: impl FnOnce(&mut Document) -> R) -> Option<R> {
        self.docs.get_mut(uri).map(|mut d| f(&mut d))
    }

    pub fn snapshot_documents(&self) -> Vec<(Url, String, String)> {
        self.docs
            .iter()
            .map(|entry| {
                (
                    entry.key().clone(),
                    entry.value().language_id.clone(),
                    entry.value().text.clone(),
                )
            })
            .collect()
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}
