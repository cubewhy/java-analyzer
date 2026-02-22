use nucleo_matcher::{
    Config, Matcher, Utf32Str,
    pattern::{CaseMatching, Normalization, Pattern},
};

/// Performs a fuzzy match on a single candidate string and returns a score (None indicates no match)
pub fn fuzzy_match(pattern: &str, candidate: &str) -> Option<u32> {
    if pattern.is_empty() {
        return Some(0);
    }

    let mut matcher = Matcher::new(Config::DEFAULT);
    // Ignore instead of Smart, always case-insensitive
    let pat = Pattern::parse(pattern, CaseMatching::Ignore, Normalization::Smart);

    let mut buf = Vec::new();
    let haystack = Utf32Str::new(candidate, &mut buf);

    pat.score(haystack, &mut matcher)
}

/// Filter and sort the candidate list by fuzzy score
/// Returns a pair of (candidate, score) pairs
pub fn fuzzy_filter_sort<T, F>(
    pattern: &str,
    items: impl IntoIterator<Item = T>,
    name_fn: F,
) -> Vec<(T, u32)>
where
    F: Fn(&T) -> &str,
{
    let mut scored: Vec<(T, u32)> = items
        .into_iter()
        .filter_map(|item| {
            let score = fuzzy_match(pattern, name_fn(&item))?;
            Some((item, score))
        })
        .collect();

    // Those with higher scores are ranked first
    scored.sort_by(|a, b| b.1.cmp(&a.1));
    scored
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match_scores() {
        assert!(fuzzy_match("aVar", "aVar").is_some());
    }

    #[test]
    fn test_substring_fuzzy() {
        // "var" should fuzzy match "aVar"
        assert!(
            fuzzy_match("var", "aVar").is_some(),
            "fuzzy: 'var' should match 'aVar'"
        );
    }

    #[test]
    fn test_empty_pattern_matches_all() {
        assert!(fuzzy_match("", "anything").is_some());
        assert!(fuzzy_match("", "").is_some());
    }

    #[test]
    fn test_no_match() {
        assert!(fuzzy_match("xyz", "aVar").is_none());
    }

    #[test]
    fn test_case_insensitive() {
        assert!(fuzzy_match("AVAR", "aVar").is_some());
        assert!(fuzzy_match("avar", "aVar").is_some());
    }

    #[test]
    fn test_filter_sort_order() {
        // Exact matches should be listed first
        let items = ["aVar", "var", "variable", "unrelated"];
        let results = fuzzy_filter_sort("var", items.iter(), |s| s);
        let names: Vec<&str> = results.iter().map(|(s, _)| **s).collect();
        // "var" has the highest score for exact matching and should be ranked first.
        assert_eq!(names[0], "var", "exact match should rank first");
        // "unrelated" does not match and should not appear.
        assert!(names.iter().all(|&s| s != "unrelated"));
    }

    #[test]
    fn test_subsequence_match() {
        // "fn" is a subsequence of "fun" and should match
        assert!(
            fuzzy_match("fn", "fun").is_some(),
            "'fn' is a subsequence of 'fun'"
        );
        // "fc" is a subsequence of "func"
        assert!(fuzzy_match("fc", "func").is_some());
        // "func" is not a subsequence of "fun" (fun does not contain 'c')
        assert!(fuzzy_match("func", "fun").is_none());
    }

    #[test]
    fn test_prefix_always_matches() {
        // Prefix matching is a special case of subsequence matching and should have the highest score.
        assert!(fuzzy_match("fun", "fun").is_some());
        assert!(fuzzy_match("fun", "func").is_some());
        assert!(fuzzy_match("pri", "priFunc").is_some());
    }
}
