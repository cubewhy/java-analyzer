use serde::Serialize;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum CandidateKind {
    ClassName,
    Method {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    Field {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    StaticMethod {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    StaticField {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    LocalVariable {
        type_descriptor: Arc<str>,
    },
    Constructor {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    Keyword,
}

#[derive(Debug, Clone, Serialize)]
pub struct CompletionCandidate {
    /// The text displayed to the user (method name, field name, class name, etc.)
    pub label: Arc<str>,
    /// The actual text inserted (may differ from the label, for example, if the method requires parentheses).
    pub insert_text: String,
    /// Candidate Types
    pub kind: CandidateKind,
    /// Overall score, the higher the score, the higher the ranking.
    pub score: f32,
    /// Optional: Documentation comments / Signature
    pub detail: Option<String>,
    /// Marks which provider the candidate was generated from (for debugging/statistics purposes)
    pub source: &'static str,
    /// If this candidate needs to be automatically imported, record it here.
    pub required_import: Option<String>,
}

impl CompletionCandidate {
    pub fn new(
        label: impl Into<Arc<str>>,
        insert_text: impl Into<String>,
        kind: CandidateKind,
        source: &'static str,
    ) -> Self {
        Self {
            label: label.into(),
            insert_text: insert_text.into(),
            kind,
            score: 0.0,
            detail: None,
            source,
            required_import: None,
        }
    }

    pub fn with_score(mut self, score: f32) -> Self {
        self.score = score;
        self
    }

    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    pub fn with_import(mut self, import: impl Into<String>) -> Self {
        self.required_import = Some(import.into());
        self
    }
}
