use std::sync::Arc;

use crate::{
    completion::{CandidateKind, CompletionCandidate, fuzzy, provider::CompletionProvider},
    index::{IndexScope, IndexView},
    semantic::context::{CursorLocation, SemanticContext, StatementLabelCompletionKind},
};

pub struct StatementLabelProvider;

impl CompletionProvider for StatementLabelProvider {
    fn name(&self) -> &'static str {
        "statement_label"
    }

    fn is_applicable(&self, ctx: &SemanticContext) -> bool {
        matches!(ctx.location, CursorLocation::StatementLabel { .. })
    }

    fn provide(
        &self,
        _scope: IndexScope,
        ctx: &SemanticContext,
        _index: &IndexView,
    ) -> Vec<CompletionCandidate> {
        let CursorLocation::StatementLabel { kind, ref prefix } = ctx.location else {
            return vec![];
        };

        let mut scored = Vec::new();
        for (idx, label) in ctx.visible_statement_labels().iter().enumerate() {
            let legal = match kind {
                StatementLabelCompletionKind::Break => label.target_kind.is_break_target(),
                StatementLabelCompletionKind::Continue => label.target_kind.is_continue_target(),
            };
            if !legal {
                continue;
            }
            let Some(score) = fuzzy::fuzzy_match(prefix, label.name.as_ref()) else {
                continue;
            };
            scored.push((idx, label, score));
        }

        scored.sort_by(|a, b| b.2.cmp(&a.2).then_with(|| a.0.cmp(&b.0)));

        scored
            .into_iter()
            .map(|(_, label, score)| {
                CompletionCandidate::new(
                    Arc::clone(&label.name),
                    label.name.to_string(),
                    CandidateKind::StatementLabel,
                    self.name(),
                )
                .with_detail(format!("{:?}", label.target_kind))
                .with_score(55.0 + score as f32 * 0.1)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::{IndexScope, ModuleId, WorkspaceIndex};
    use crate::semantic::context::{StatementLabel, StatementLabelTargetKind};

    fn root_scope() -> IndexScope {
        IndexScope {
            module: ModuleId::ROOT,
        }
    }

    fn make_ctx(
        kind: StatementLabelCompletionKind,
        prefix: &str,
        labels: Vec<(&str, StatementLabelTargetKind)>,
    ) -> SemanticContext {
        SemanticContext::new(
            CursorLocation::StatementLabel {
                kind,
                prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            None,
            None,
            None,
            vec![],
        )
        .with_statement_labels(
            labels
                .into_iter()
                .map(|(name, target_kind)| StatementLabel {
                    name: Arc::from(name),
                    target_kind,
                })
                .collect(),
        )
    }

    #[test]
    fn test_continue_filters_non_loop_labels() {
        let idx = WorkspaceIndex::new();
        let ctx = make_ctx(
            StatementLabelCompletionKind::Continue,
            "",
            vec![
                ("innerLoop", StatementLabelTargetKind::While),
                ("outerBlock", StatementLabelTargetKind::Block),
            ],
        );

        let labels: Vec<String> = StatementLabelProvider
            .provide(root_scope(), &ctx, &idx.view(root_scope()))
            .into_iter()
            .map(|c| c.label.to_string())
            .collect();

        assert_eq!(labels, vec!["innerLoop".to_string()]);
    }

    #[test]
    fn test_break_preserves_nearest_order_on_empty_prefix() {
        let idx = WorkspaceIndex::new();
        let ctx = make_ctx(
            StatementLabelCompletionKind::Break,
            "",
            vec![
                ("inner", StatementLabelTargetKind::For),
                ("outer", StatementLabelTargetKind::Block),
            ],
        );

        let labels: Vec<String> = StatementLabelProvider
            .provide(root_scope(), &ctx, &idx.view(root_scope()))
            .into_iter()
            .map(|c| c.label.to_string())
            .collect();

        assert_eq!(labels, vec!["inner".to_string(), "outer".to_string()]);
    }
}
