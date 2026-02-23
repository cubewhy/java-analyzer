use std::sync::Arc;

use crate::completion::{CompletionCandidate, CompletionContext};
use tree_sitter::Parser;

pub(crate) mod rope_utils;
pub(crate) mod ts_utils;

pub mod java;
pub mod kotlin;
pub use java::JavaLanguage;
pub use kotlin::KotlinLanguage;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LanguageId(pub Arc<str>);

impl LanguageId {
    pub fn new(id: impl Into<Arc<str>>) -> Self {
        Self(id.into())
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

pub trait Language: Send + Sync + std::fmt::Debug {
    fn id(&self) -> &'static str;
    fn supports(&self, language_id: &str) -> bool;

    fn make_parser(&self) -> Parser;

    fn parse_completion_context(
        &self,
        source: &str,
        line: u32,
        character: u32,
        trigger_char: Option<char>,
    ) -> Option<CompletionContext>;

    fn post_process_candidates(
        &self,
        candidates: Vec<CompletionCandidate>,
        _ctx: &CompletionContext,
    ) -> Vec<CompletionCandidate> {
        candidates
    }

    fn class_file_extensions(&self) -> &[&str] {
        &["jar", "class"]
    }
}

pub struct LanguageRegistry {
    languages: Vec<Box<dyn Language>>,
}

impl LanguageRegistry {
    pub fn new() -> Self {
        Self {
            languages: vec![Box::new(JavaLanguage), Box::new(KotlinLanguage)],
        }
    }

    pub fn find(&self, language_id: &str) -> Option<&dyn Language> {
        self.languages
            .iter()
            .find(|l| l.supports(language_id))
            .map(|l| l.as_ref())
    }

    pub fn register(&mut self, lang: Box<dyn Language>) {
        self.languages.push(lang);
    }
}

impl Default for LanguageRegistry {
    fn default() -> Self {
        Self::new()
    }
}
