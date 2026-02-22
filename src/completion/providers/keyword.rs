use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
use std::sync::Arc;

#[rustfmt::skip]
const JAVA_KEYWORDS: &[&str] = &[
    "abstract", "assert", "boolean", "break", "byte",
    "case", "catch", "char", "class", "const", "continue",
    "default", "do", "double", "else", "enum", "extends",
    "final", "finally", "float", "for", "goto", "if",
    "implements", "import", "instanceof", "int", "interface",
    "long", "native", "new", "package", "private", "protected",
    "public", "return", "short", "static", "strictfp", "super",
    "switch", "synchronized", "this", "throw", "throws", "transient",
    "try", "var", "void", "volatile", "while",
    // Java 17+
    "record", "sealed", "permits", "yield", "text",
];

pub struct KeywordProvider;

impl CompletionProvider for KeywordProvider {
    fn name(&self) -> &'static str {
        "keyword"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        _index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::Expression { prefix } => prefix.as_str(),
            _ => return vec![],
        };

        // TODO: context based completation

        let prefix_lower = prefix.to_lowercase();

        JAVA_KEYWORDS
            .iter()
            .filter(|&&kw| kw.starts_with(&prefix_lower))
            .map(|&kw| {
                CompletionCandidate::new(
                    Arc::from(kw),
                    kw.to_string(),
                    CandidateKind::Keyword,
                    self.name(),
                )
            })
            .collect()
    }
}
