/// The parsed type structure, stripped of generics and package paths
pub struct ParsedType<'a> {
    pub is_array: bool,
    // The generics wrapper, e.g List in List<User>
    pub wrapper: Option<&'a str>,
    // The generics type, e.g User in List<User>
    pub simple_name: &'a str,
}

impl<'a> ParsedType<'a> {
    pub fn parse(type_name: &'a str) -> Self {
        let type_name = type_name.trim_end_matches(';');
        let is_array = type_name.ends_with("[]");
        let base = type_name.trim_end_matches("[]");

        let mut wrapper = None;
        let mut inner = base;

        if let Some(start) = base.find('<')
            && base.ends_with('>')
        {
            wrapper = Some(&base[..start]);
            inner = &base[start + 1..base.len() - 1];
        }

        fn clean(s: &str) -> &str {
            s.rsplit('/')
                .next()
                .unwrap_or(s)
                .rsplit('.')
                .next()
                .unwrap_or(s)
        }

        Self {
            is_array,
            wrapper: wrapper.map(clean),
            simple_name: clean(inner),
        }
    }
}

fn keyword_rule(parsed: &ParsedType) -> Option<Vec<String>> {
    match parsed.simple_name {
        "Class" => Some(vec!["clazz".to_string(), "klass".to_string()]),
        "Interface" => Some(vec!["interface".to_string()]),
        _ => None,
    }
}

fn wrapper_rule(parsed: &ParsedType) -> Option<Vec<String>> {
    let wrapper = parsed.wrapper?;
    let inner = parsed.simple_name;

    // TODO: process Map<K, V>
    if inner.contains(',') {
        return None;
    }

    let inner_lower = to_lower_camel(inner);

    match wrapper {
        "List" | "Set" | "Collection" | "Queue" | "Iterable" => Some(vec![
            pluralize(&inner_lower),
            format!("{}{}", inner_lower, wrapper),
        ]),
        "Optional" => Some(vec![inner_lower.clone(), format!("{}Opt", inner_lower)]),
        _ => None,
    }
}

fn default_rule(parsed: &ParsedType) -> Option<Vec<String>> {
    let simple = parsed.simple_name;
    if simple.is_empty() {
        return None;
    }

    let mut results = Vec::new();

    // 1. Acronym: StringBuilder → sb
    results.push(acronym_of(simple));

    // 2. Last word: StringBuilder → builder
    if let Some(last) = camel_words(simple).last().map(|w| to_lower_camel(w)) {
        results.push(last);
    }

    // 3. Full lowerCamelCase: StringBuilder → stringBuilder
    results.push(to_lower_camel(simple));

    // 4. Short names as-is
    if simple.len() <= 4 {
        results.push(simple.to_lowercase());
    }

    Some(results)
}

pub fn pluralize(s: &str) -> String {
    if s.ends_with('s')
        || s.ends_with('x')
        || s.ends_with('z')
        || s.ends_with("ch")
        || s.ends_with("sh")
    {
        format!("{}es", s)
    } else if let Some(stripped) = s.strip_suffix("y") {
        // y -> ies
        format!("{}ies", stripped)
    } else {
        format!("{}s", s)
    }
}

/// Split a CamelCase identifier into words.
/// "StringBuilder" → ["String", "Builder"]
/// "HTTPSConnection" → ["H","T","T","P","S","Connection"] (consecutive caps each become a word)
pub fn camel_words(s: &str) -> Vec<&str> {
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
pub fn acronym_of(s: &str) -> String {
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

/// Rule function signature: Receives the parsed type; if a match is found, returns Some (a list of suggestions); otherwise, returns None and passes it to the next rule.
type SuggestionRule = fn(&ParsedType) -> Option<Vec<String>>;

/// Register all enabled rules (arranged in order of priority)
pub const BASE_RULES: &[SuggestionRule] = &[keyword_rule, wrapper_rule, default_rule];

#[cfg(test)]
mod tests {
    use crate::completion::providers::name_suggestion::rules::{acronym_of, camel_words};

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
