use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::lsp_types::Url;

#[derive(Debug, Clone)]
pub struct Document {
    pub uri: Url,
    pub language_id: String,
    pub version: i32,
    pub content: Arc<str>,
}

impl Document {
    pub fn new(uri: Url, language_id: String, version: i32, content: String) -> Self {
        Self {
            uri,
            language_id,
            version,
            content: Arc::from(content.as_str()),
        }
    }

    pub fn apply_full_change(&mut self, version: i32, new_content: String) {
        self.version = version;
        self.content = Arc::from(new_content.as_str());
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

    pub fn get(&self, uri: &Url) -> Option<Document> {
        self.docs.get(uri).map(|d| d.clone())
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}
