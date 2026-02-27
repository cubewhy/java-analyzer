use crate::completion::{CandidateKind, CompletionCandidate, scorer::Scorer};

pub fn process(mut candidates: Vec<CompletionCandidate>, query: &str) -> Vec<CompletionCandidate> {
    // Score
    let scorer = Scorer::new(query);
    for c in &mut candidates {
        c.score += scorer.score(c);
    }

    // Dedup
    let mut candidates = dedup(candidates);

    // Sort results
    candidates.sort_unstable_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| a.label.cmp(&b.label))
    });

    candidates
}

fn dedup(mut candidates: Vec<CompletionCandidate>) -> Vec<CompletionCandidate> {
    candidates.sort_unstable_by(|a, b| {
        a.label
            .cmp(&b.label)
            .then_with(|| a.source.cmp(b.source))
            .then_with(|| {
                a.required_import
                    .is_some()
                    .cmp(&b.required_import.is_some())
            })
            .then_with(|| {
                b.score
                    .partial_cmp(&a.score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    });

    let mut result: Vec<CompletionCandidate> = Vec::with_capacity(candidates.len());
    for c in candidates {
        let duplicate = result
            .iter()
            .rev()
            .take_while(|last| last.label == c.label)
            .any(|last| {
                if std::mem::discriminant(&last.kind) != std::mem::discriminant(&c.kind) {
                    return false;
                }
                match (&last.kind, &c.kind) {
                    (
                        CandidateKind::Method { descriptor: d1, .. },
                        CandidateKind::Method { descriptor: d2, .. },
                    )
                    | (
                        CandidateKind::StaticMethod { descriptor: d1, .. },
                        CandidateKind::StaticMethod { descriptor: d2, .. },
                    )
                    | (
                        CandidateKind::Constructor { descriptor: d1, .. },
                        CandidateKind::Constructor { descriptor: d2, .. },
                    ) => d1 == d2,
                    (CandidateKind::ClassName, CandidateKind::ClassName) => {
                        last.detail == c.detail && last.required_import == c.required_import
                    }
                    _ => true,
                }
            });

        if !duplicate {
            result.push(c);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::completion::{CandidateKind, CompletionCandidate, post_processor::dedup};

    #[test]
    fn test_dedup_allows_same_label_different_package() {
        let c1 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());
        let c2 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.awt.List".to_string());

        let results = dedup(vec![c1, c2]);
        assert_eq!(
            results.len(),
            2,
            "Should keep both List candidates because FQNs are different"
        );
    }

    #[test]
    fn test_dedup_removes_actual_duplicates() {
        let c1 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());
        let c2 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());

        let results = dedup(vec![c1, c2]);
        assert_eq!(results.len(), 1, "Should remove identical class candidates");
    }

    #[test]
    fn test_dedup_prefers_no_import_candidate() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;
        let with_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        )
        .with_import("org.cubewhy.RandomClass");
        let without_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        );
        let result = dedup(vec![with_import, without_import]);
        assert_eq!(result.len(), 1, "should dedup to one candidate");
        assert!(
            result[0].required_import.is_none(),
            "dedup should prefer the candidate without required_import, got: {:?}",
            result[0].required_import
        );
    }
}
