use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;
use std::sync::Arc;

pub struct NameSuggestionProvider;

impl CompletionProvider for NameSuggestionProvider {
    fn name(&self) -> &'static str {
        "name_suggestion"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        _index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let type_name = match &ctx.location {
            CursorLocation::VariableName { type_name } => type_name.as_str(),
            _ => return vec![],
        };

        if type_name.is_empty() {
            return vec![];
        }

        let suggestions = generate_name_suggestions(type_name);
        suggestions
            .into_iter()
            .enumerate()
            .map(|(i, name)| {
                // Higher index = lower score so first suggestion ranks highest
                let score = 100.0 - i as f32;
                CompletionCandidate::new(
                    Arc::from(name.as_str()),
                    name,
                    CandidateKind::NameSuggestion,
                    self.name(),
                )
                .with_detail(format!("{} variable name", type_name))
                .with_score(score)
            })
            .collect()
    }
}

/// Generate variable name suggestions from a type name.
/// e.g. "StringBuilder" → ["sb", "builder", "stringBuilder", "StringBuilder"]  
pub fn generate_name_suggestions(type_name: &str) -> Vec<String> {
    // Strip array suffix
    let base = type_name.trim_end_matches("[]").trim_end_matches(';');
    // Use only the simple name (strip package path)
    let simple = base
        .rsplit('/')
        .next()
        .unwrap_or(base)
        .rsplit('.')
        .next()
        .unwrap_or(base);

    if simple.is_empty() {
        return vec![];
    }

    let mut results: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    let mut add = |s: String| {
        if !s.is_empty() && is_valid_identifier(&s) && seen.insert(s.clone()) {
            results.push(s);
        }
    };

    // 1. Acronym: StringBuilder → sb, HttpServletRequest → hsr
    let acronym = acronym_of(simple);
    add(acronym);

    // 2. Last word (camelCase split): StringBuilder → builder, HttpServletRequest → request
    if let Some(last) = camel_words(simple).last().map(|w| to_lower_camel(w)) {
        add(last);
    }

    // 3. Full name in lowerCamelCase: StringBuilder → stringBuilder
    let lower_camel = to_lower_camel(simple);
    add(lower_camel);

    // 4. For short names (≤4 chars), also suggest as-is lowercased
    if simple.len() <= 4 {
        add(simple.to_lowercase());
    }

    results
}

/// Split a CamelCase identifier into words.
/// "StringBuilder" → ["String", "Builder"]
/// "HTTPSConnection" → ["H","T","T","P","S","Connection"] (consecutive caps each become a word)
fn camel_words(s: &str) -> Vec<&str> {
    let mut words = Vec::new();
    let mut start = 0;
    let chars: Vec<(usize, char)> = s.char_indices().collect();

    for i in 1..chars.len() {
        let (_, prev) = chars[i - 1];
        let (pos, cur) = chars[i];
        // Split before an uppercase letter that follows a lowercase letter
        // or before an uppercase letter followed by a lowercase (e.g. "HTTPSConn" → "HTTPS","Conn")
        let next_is_lower = chars.get(i + 1).is_some_and(|(_, c)| c.is_lowercase());
        if cur.is_uppercase() && (prev.is_lowercase() || (prev.is_uppercase() && next_is_lower)) {
            words.push(&s[start..pos]);
            start = pos;
        }
    }
    words.push(&s[start..]);
    words.into_iter().filter(|w| !w.is_empty()).collect()
}

/// Build acronym from CamelCase words: "StringBuilder" → "sb"
fn acronym_of(s: &str) -> String {
    camel_words(s)
        .iter()
        .filter_map(|w| w.chars().next())
        .map(|c| c.to_ascii_lowercase())
        .collect()
}

/// Convert a word to lowerCamelCase (just lowercase the first char).
fn to_lower_camel(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().collect::<String>() + chars.as_str(),
    }
}

fn is_valid_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        None => false,
        Some(c) => {
            (c.is_alphabetic() || c == '_') && chars.all(|c| c.is_alphanumeric() || c == '_')
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::completion::providers::CompletionProvider;
    use crate::index::GlobalIndex;

    fn ctx(type_name: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::VariableName {
                type_name: type_name.to_string(),
            },
            "",
            vec![],
            None,
            None,
            None,
            vec![],
        )
    }

    #[test]
    fn test_string_builder_suggestions() {
        let mut idx = GlobalIndex::new();
        let results = NameSuggestionProvider.provide(&ctx("StringBuilder"), &mut idx);
        let names: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert!(
            names.contains(&"sb"),
            "should suggest acronym 'sb': {:?}",
            names
        );
        assert!(
            names.contains(&"builder"),
            "should suggest last word 'builder': {:?}",
            names
        );
        assert!(
            names.contains(&"stringBuilder"),
            "should suggest lowerCamel: {:?}",
            names
        );
    }

    #[test]
    fn test_http_servlet_request() {
        let suggestions = generate_name_suggestions("HttpServletRequest");
        assert!(
            suggestions.contains(&"hsr".to_string()),
            "{:?}",
            suggestions
        );
        assert!(
            suggestions.contains(&"request".to_string()),
            "{:?}",
            suggestions
        );
        assert!(
            suggestions.contains(&"httpServletRequest".to_string()),
            "{:?}",
            suggestions
        );
    }

    #[test]
    fn test_simple_type_string() {
        let suggestions = generate_name_suggestions("String");
        // "s" as acronym, "string" as lower camel, short name lowercased
        assert!(!suggestions.is_empty(), "{:?}", suggestions);
        assert!(
            suggestions.contains(&"string".to_string()) || suggestions.contains(&"s".to_string()),
            "{:?}",
            suggestions
        );
    }

    #[test]
    fn test_array_type_stripped() {
        let suggestions = generate_name_suggestions("String[]");
        // Should treat as "String"
        assert!(!suggestions.is_empty());
        assert!(suggestions.iter().all(|s| !s.contains('[')));
    }

    #[test]
    fn test_internal_name_uses_simple() {
        // "java/util/ArrayList" → suggestions based on "ArrayList"
        let suggestions = generate_name_suggestions("java/util/ArrayList");
        assert!(
            suggestions.contains(&"al".to_string())
                || suggestions.contains(&"arrayList".to_string()),
            "{:?}",
            suggestions
        );
    }

    #[test]
    fn test_no_duplicates() {
        // "List" → acronym "l", lower camel "list", short "list" — "list" should appear once
        let suggestions = generate_name_suggestions("List");
        let unique: std::collections::HashSet<_> = suggestions.iter().collect();
        assert_eq!(
            suggestions.len(),
            unique.len(),
            "no duplicates: {:?}",
            suggestions
        );
    }

    #[test]
    fn test_empty_type_returns_empty() {
        let mut idx = GlobalIndex::new();
        let results = NameSuggestionProvider.provide(&ctx(""), &mut idx);
        assert!(results.is_empty());
    }

    #[test]
    fn test_camel_words_split() {
        assert_eq!(camel_words("StringBuilder"), vec!["String", "Builder"]);
        assert_eq!(
            camel_words("HttpServletRequest"),
            vec!["Http", "Servlet", "Request"]
        );
        assert_eq!(camel_words("simple"), vec!["simple"]);
        assert_eq!(camel_words("URL"), vec!["URL"]);
    }

    #[test]
    fn test_acronym_of() {
        assert_eq!(acronym_of("StringBuilder"), "sb");
        assert_eq!(acronym_of("HttpServletRequest"), "hsr");
        assert_eq!(acronym_of("List"), "l");
    }
}
