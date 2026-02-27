use super::super::{
    candidate::CompletionCandidate,
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::index::GlobalIndex;

pub struct ImportStaticProvider;

impl CompletionProvider for ImportStaticProvider {
    fn name(&self) -> &'static str {
        "import_static"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let prefix = match &ctx.location {
            CursorLocation::ImportStatic { prefix } => prefix.as_str(),
            _ => return vec![],
        };
        // 复用已有的 import 路径补全逻辑
        crate::completion::import_completion::candidates_for_import(prefix, index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex, MethodSummary};
    use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};
    use std::sync::Arc;

    fn make_index() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/lang")),
            name: Arc::from("Math"),
            internal_name: Arc::from("java/lang/Math"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("abs"),
                descriptor: Arc::from("(I)I"),
                access_flags: ACC_PUBLIC | ACC_STATIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: Some(Arc::from("int")),
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        idx
    }

    #[test]
    fn test_non_import_static_location_returns_empty() {
        let mut idx = make_index();
        let ctx = CompletionContext::new(
            CursorLocation::Expression {
                prefix: "abs".to_string(),
            },
            "abs",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        assert!(ImportStaticProvider.provide(&ctx, &mut idx).is_empty());
    }

    #[test]
    fn test_completes_class_path_for_import_static() {
        let mut idx = make_index();
        let ctx = CompletionContext::new(
            CursorLocation::ImportStatic {
                prefix: "java.lang.Ma".to_string(),
            },
            "Ma",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        let results = ImportStaticProvider.provide(&ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref().contains("Math")),
            "should suggest java.lang.Math: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }
}
