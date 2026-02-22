use rust_asm::constants::ACC_STATIC;

use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
    scorer,
};
use super::CompletionProvider;
use crate::{completion::scorer::AccessFilter, index::GlobalIndex};
use std::sync::Arc;

pub struct StaticMemberProvider;

impl CompletionProvider for StaticMemberProvider {
    fn name(&self) -> &'static str {
        "static_member"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let (class_name_raw, member_prefix) = match &ctx.location {
            CursorLocation::StaticAccess {
                class_internal_name,
                member_prefix,
            } => (class_internal_name.as_ref(), member_prefix.as_str()),
            _ => return vec![],
        };

        // class_name_raw could be a simple name ("Main") or an internal name ("org/cubewhy/Main")
        // Try searching directly first, then search by simple name if it's not found
        let class_meta = if let Some(m) = index.get_class(class_name_raw) {
            m
        } else {
            // Search by simple name (there may be multiple names, those in the same package will be prioritized)
            let mut candidates = index.get_classes_by_simple_name(class_name_raw);
            if candidates.is_empty() {
                return vec![];
            }
            // same package first
            if let Some(pkg) = ctx.enclosing_package.as_deref()
                && let Some(pos) = candidates
                    .iter()
                    .position(|c| c.package.as_deref() == Some(pkg))
            {
                candidates.swap(0, pos);
            }
            candidates.into_iter().next().unwrap()
        };

        let filter = AccessFilter::member_completion();
        let prefix_lower = member_prefix.to_lowercase();
        let class_name = class_meta.internal_name.as_ref();
        let mut results = Vec::new();

        for method in &class_meta.methods {
            if method.name.as_ref() == "<init>" || method.name.as_ref() == "<clinit>" {
                continue;
            }
            if method.access_flags & ACC_STATIC == 0 {
                continue;
            }
            if !filter.is_method_accessible(method.access_flags, method.is_synthetic) {
                continue;
            }
            if !prefix_lower.is_empty() && !method.name.to_lowercase().starts_with(&prefix_lower) {
                continue;
            }
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&method.name),
                    if ctx.has_paren_after_cursor() {
                        method.name.to_string()
                    } else {
                        format!("{}(", method.name)
                    },
                    CandidateKind::StaticMethod {
                        descriptor: Arc::clone(&method.descriptor),
                        defining_class: Arc::from(class_name),
                    },
                    self.name(),
                )
                .with_detail(scorer::method_detail(class_name, method)),
            );
        }

        for field in &class_meta.fields {
            if field.access_flags & ACC_STATIC == 0 {
                continue;
            }
            if !filter.is_field_accessible(field.access_flags, field.is_synthetic) {
                continue;
            }
            if !prefix_lower.is_empty() && !field.name.to_lowercase().starts_with(&prefix_lower) {
                continue;
            }
            results.push(
                CompletionCandidate::new(
                    Arc::clone(&field.name),
                    field.name.to_string(),
                    CandidateKind::StaticField {
                        descriptor: Arc::clone(&field.descriptor),
                        defining_class: Arc::from(class_name),
                    },
                    self.name(),
                )
                .with_detail(scorer::field_detail(class_name, field)),
            );
        }

        results
    }
}

#[cfg(test)]
mod tests {
    use rust_asm::constants::ACC_PUBLIC;

    use super::*;
    use crate::completion::context::{CompletionContext, CursorLocation};
    use crate::completion::providers::CompletionProvider;
    use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex, MethodSummary};
    use crate::language::{JavaLanguage, Language};
    use std::sync::Arc;

    fn at(src: &str, line: u32, col: u32) -> CompletionContext {
        JavaLanguage
            .parse_completion_context(src, line, col, None)
            .unwrap()
    }

    fn make_index_with_main() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy")),
            name: Arc::from("Main"),
            internal_name: Arc::from("org/cubewhy/Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("func"),
                descriptor: Arc::from("()V"),
                access_flags: ACC_PUBLIC | ACC_STATIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: None,
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        idx
    }

    fn static_ctx(class_raw: &str, prefix: &str, pkg: &str) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::StaticAccess {
                class_internal_name: Arc::from(class_raw),
                member_prefix: prefix.to_string(),
            },
            prefix,
            vec![],
            Some(Arc::from("Main")),
            None,
            Some(Arc::from(pkg)),
            vec![],
        )
    }

    #[test]
    fn test_static_access_by_simple_name() {
        // "Main.fun|" → class_internal_name="Main" (simple name),
        // It should be able to be found in the same package as org/cubewhy/Main
        let mut index = make_index_with_main();
        let ctx = static_ctx("Main", "fun", "org/cubewhy");
        let results = StaticMemberProvider.provide(&ctx, &mut index);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "func"),
            "should find func via simple name lookup: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_static_access_by_internal_name() {
        let mut index = make_index_with_main();
        let ctx = static_ctx("org/cubewhy/Main", "fun", "org/cubewhy");
        let results = StaticMemberProvider.provide(&ctx, &mut index);
        assert!(results.iter().any(|c| c.label.as_ref() == "func"));
    }

    #[test]
    fn test_static_access_empty_prefix_returns_all_static() {
        let mut index = make_index_with_main();
        let ctx = static_ctx("Main", "", "org/cubewhy");
        let results = StaticMemberProvider.provide(&ctx, &mut index);
        assert!(!results.is_empty());
        assert!(results.iter().any(|c| c.label.as_ref() == "func"));
    }

    #[test]
    fn test_locals_in_static_method_no_semicolon() {
        // Local variables can still be extracted in lines without semicolons.
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aVar = "test";
                String str = "a";
                s
            }
        }
    "#};
        let line = 4u32;
        let col = src.lines().nth(4).unwrap().len() as u32; // the 4th line content: s
        let ctx = at(src, line, col);
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "aVar"),
            "aVar should be extracted even without semicolon on current line: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
        assert!(
            ctx.local_variables.iter().any(|v| v.name.as_ref() == "str"),
            "str should be extracted: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_locals_in_method_argument_no_semicolon() {
        // System.out.println(var|) Scenario
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aVar = "test";
                System.out.println(aVar)
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        // The cursor is after r in println(aVar)
        let col = raw.find("aVar").unwrap() as u32 + 4;
        let ctx = at(src, line, col);
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "aVar"),
            "aVar should be visible inside method argument: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_char_after_cursor_paren() {
        // this.priFunc() places the cursor after (before) priFunc
        let src = indoc::indoc! {r#"
        class A {
            void fun() {
                this.priFunc()
            }
            private void priFunc() {}
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        // The cursor is after (before) priFunc
        let col = raw.find("priFunc").unwrap() as u32 + "priFunc".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.has_paren_after_cursor(),
            "char after cursor should be '(', got {:?}",
            ctx.char_after_cursor
        );
    }

    #[test]
    fn test_char_after_cursor_no_paren() {
        let src = indoc::indoc! {r#"
        class A {
            void fun() {
                this.priFunc
            }
            private void priFunc() {}
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("priFunc").unwrap() as u32 + "priFunc".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            !ctx.has_paren_after_cursor(),
            "no paren after cursor, got {:?}",
            ctx.char_after_cursor
        );
    }
}
