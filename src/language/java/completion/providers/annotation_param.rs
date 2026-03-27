use std::collections::HashSet;
use std::sync::Arc;

use rust_asm::constants::ACC_ANNOTATION;

use crate::completion::candidate::CandidateKind;
use crate::completion::provider::{CompletionProvider, ProviderCompletionResult};
use crate::completion::{CompletionCandidate, fuzzy};
use crate::index::{ClassMetadata, IndexScope, IndexView, MethodSummary};
use crate::language::java::completion_context::resolve_source_like_type_with_scope;
use crate::language::java::type_ctx::SourceTypeCtx;
use crate::semantic::context::{CursorLocation, SemanticContext};
use crate::semantic::types::symbol_resolver::SymbolResolver;

pub struct AnnotationParamProvider;

impl CompletionProvider for AnnotationParamProvider {
    fn name(&self) -> &'static str {
        "annotation_param"
    }

    fn is_applicable(&self, ctx: &SemanticContext) -> bool {
        matches!(ctx.location, CursorLocation::AnnotationParam { .. })
    }

    fn provide(
        &self,
        _scope: IndexScope,
        ctx: &SemanticContext,
        index: &IndexView,
        request: Option<&crate::lsp::request_context::RequestContext>,
        _limit: Option<usize>,
    ) -> crate::lsp::request_cancellation::RequestResult<ProviderCompletionResult> {
        let (prefix, annotation_name, used_keys, fresh_slot) = match &ctx.location {
            CursorLocation::AnnotationParam {
                prefix,
                annotation_name,
                used_keys,
                fresh_slot,
            } => (
                prefix.as_str(),
                annotation_name.as_deref(),
                used_keys.as_slice(),
                *fresh_slot,
            ),
            _ => return Ok(ProviderCompletionResult::default()),
        };

        let Some(annotation_meta) = resolve_annotation_metadata(ctx, index, annotation_name)
            .filter(|meta| is_annotation_class(meta))
        else {
            return Ok(ProviderCompletionResult::default());
        };

        let used: HashSet<&str> = used_keys.iter().map(|key| key.as_ref()).collect();
        let prefix_lower = prefix.to_lowercase();
        let detail = format!("annotation element of {}", annotation_meta.source_name());
        let mut results = Vec::new();

        for (idx, method) in annotation_elements(&annotation_meta).enumerate() {
            if idx % 16 == 0
                && let Some(request) = request
            {
                request.check_cancelled("completion.annotation_param")?;
            }

            let name = method.name.as_ref();
            if used.contains(name) {
                continue;
            }

            let Some(fuzzy_score) = fuzzy::fuzzy_match(&prefix_lower, &name.to_lowercase()) else {
                continue;
            };

            let insert_text = if fresh_slot {
                format!("{name} = ")
            } else {
                name.to_string()
            };

            let mut candidate = CompletionCandidate::new(
                Arc::clone(&method.name),
                insert_text,
                CandidateKind::AnnotationElement,
                self.name(),
            )
            .with_detail(detail.clone())
            .with_filter_text(name.to_string())
            .with_score(70.0 + fuzzy_score as f32 * 0.1);

            if !fresh_slot {
                candidate = candidate.with_insert_text(name.to_string());
            }

            results.push(candidate);
        }

        Ok(results.into())
    }
}

fn resolve_annotation_metadata(
    ctx: &SemanticContext,
    index: &IndexView,
    annotation_name: Option<&str>,
) -> Option<Arc<ClassMetadata>> {
    let annotation_name = annotation_name?.trim();
    if annotation_name.is_empty() {
        return None;
    }

    let resolver = SymbolResolver::new(index);
    if let Some(type_ctx) = ctx.extension::<SourceTypeCtx>()
        && let Some(annotation_ty) =
            resolve_source_like_type_with_scope(ctx, type_ctx, &resolver, annotation_name)
        && let Some(meta) = index.get_class(annotation_ty.erased_internal())
    {
        return Some(meta);
    }

    if let Some(internal) = resolver.resolve_type_name(ctx, annotation_name)
        && let Some(meta) = index.get_class(&internal)
    {
        return Some(meta);
    }

    if let Some(pkg) = ctx.effective_package() {
        let mut matches = index
            .classes_in_package(pkg)
            .into_iter()
            .filter(|meta| meta.matches_simple_name(annotation_name));
        if let Some(first) = matches.next()
            && matches.next().is_none()
        {
            return Some(first);
        }
    }

    let mut imported_matches = index
        .resolve_imports(&ctx.existing_imports)
        .into_iter()
        .filter(|meta| meta.matches_simple_name(annotation_name));
    if let Some(first) = imported_matches.next()
        && imported_matches.next().is_none()
    {
        return Some(first);
    }

    if annotation_name.contains('/') {
        return index.get_class(annotation_name);
    }

    if annotation_name.contains('.') {
        let internal = annotation_name.replace('.', "/");
        if let Some(meta) = index.get_class(&internal) {
            return Some(meta);
        }
    }

    index.get_unique_class_by_simple_name(annotation_name)
}

fn is_annotation_class(meta: &ClassMetadata) -> bool {
    meta.access_flags & ACC_ANNOTATION != 0
}

fn annotation_elements(meta: &ClassMetadata) -> impl Iterator<Item = &MethodSummary> {
    meta.methods.iter().filter(|method| {
        !method.is_synthetic && !method.name.starts_with('<') && method.params.is_empty()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::{ClassOrigin, IndexScope, MethodParams, ModuleId, WorkspaceIndex};
    use crate::language::java::type_ctx::SourceTypeCtx;
    use crate::semantic::context::SemanticContext;

    fn root_scope() -> IndexScope {
        IndexScope {
            module: ModuleId::ROOT,
        }
    }

    fn annotation_meta(pkg: &str, name: &str, elements: &[&str]) -> ClassMetadata {
        ClassMetadata {
            package: Some(Arc::from(pkg)),
            name: Arc::from(name),
            internal_name: Arc::from(format!("{pkg}/{name}")),
            super_name: None,
            interfaces: vec![],
            annotations: vec![],
            methods: elements
                .iter()
                .map(|element| MethodSummary {
                    name: Arc::from(*element),
                    params: MethodParams::empty(),
                    annotations: vec![],
                    access_flags: 0,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("java/lang/String")),
                })
                .collect(),
            fields: vec![],
            access_flags: ACC_ANNOTATION,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }
    }

    fn ctx(
        prefix: &str,
        annotation_name: &str,
        used_keys: Vec<Arc<str>>,
        fresh_slot: bool,
        package: &str,
        imports: Vec<Arc<str>>,
        view: &IndexView,
    ) -> SemanticContext {
        SemanticContext::new(
            CursorLocation::AnnotationParam {
                prefix: prefix.to_string(),
                annotation_name: Some(Arc::from(annotation_name)),
                used_keys,
                fresh_slot,
            },
            prefix,
            vec![],
            None,
            None,
            Some(Arc::from(package)),
            imports.clone(),
        )
        .with_extension(Arc::new(SourceTypeCtx::from_view(
            Some(Arc::from(package)),
            imports,
            view.clone(),
        )))
    }

    #[test]
    fn test_annotation_param_provider_resolves_imported_annotation() {
        let idx = WorkspaceIndex::new();
        idx.add_classes(vec![annotation_meta(
            "org/junit/jupiter/api",
            "TagAnno",
            &["value", "name"],
        )]);
        let view = idx.view(root_scope());
        let ctx = ctx(
            "",
            "TagAnno",
            vec![],
            true,
            "com/example",
            vec!["org.junit.jupiter.api.TagAnno".into()],
            &view,
        );

        let results = AnnotationParamProvider
            .provide_test(root_scope(), &ctx, &view, None)
            .candidates;

        assert!(
            results.iter().any(|c| c.label.as_ref() == "value"),
            "{:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
        let value = results
            .iter()
            .find(|c| c.label.as_ref() == "value")
            .expect("value candidate");
        assert_eq!(value.insert_text, "value = ");
        assert_eq!(value.kind, CandidateKind::AnnotationElement);
    }

    #[test]
    fn test_annotation_param_provider_filters_used_keys() {
        let idx = WorkspaceIndex::new();
        idx.add_classes(vec![annotation_meta(
            "com/example",
            "ConfigAnno",
            &["name", "enabled", "value"],
        )]);
        let view = idx.view(root_scope());
        let ctx = ctx(
            "",
            "ConfigAnno",
            vec![Arc::from("name"), Arc::from("value")],
            true,
            "com/example",
            vec![],
            &view,
        );

        let results = AnnotationParamProvider
            .provide_test(root_scope(), &ctx, &view, None)
            .candidates;
        let labels: Vec<&str> = results.iter().map(|c| c.label.as_ref()).collect();
        assert_eq!(labels, vec!["enabled"]);
    }

    #[test]
    fn test_annotation_param_provider_existing_key_replaces_identifier_only() {
        let idx = WorkspaceIndex::new();
        idx.add_classes(vec![annotation_meta(
            "com/example",
            "ConfigAnno",
            &["name", "enabled"],
        )]);
        let view = idx.view(root_scope());
        let ctx = ctx(
            "na",
            "ConfigAnno",
            vec![Arc::from("enabled")],
            false,
            "com/example",
            vec![],
            &view,
        );

        let results = AnnotationParamProvider
            .provide_test(root_scope(), &ctx, &view, None)
            .candidates;
        let name = results
            .iter()
            .find(|c| c.label.as_ref() == "name")
            .expect("name candidate");
        assert_eq!(name.insert_text, "name");
    }
}
