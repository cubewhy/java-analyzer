use std::sync::Arc;
use tree_sitter::Node;
use tree_sitter_utils::traversal::first_child_of_kind;

use crate::{
    index::{AnnotationSummary, FieldSummary, MethodParams, MethodSummary},
    language::java::{
        JavaContextExtractor,
        lombok::{
            config::LombokConfig,
            types::annotations,
            utils::{find_lombok_annotation, get_bool_param, get_string_array_param},
        },
        members::parse_annotations_in_node,
        synthetic::{
            SyntheticDefinition, SyntheticDefinitionKind, SyntheticInput, SyntheticMemberRule,
            SyntheticMemberSet, SyntheticOrigin,
        },
        type_ctx::SourceTypeCtx,
    },
};

pub struct ToStringRule;

impl SyntheticMemberRule for ToStringRule {
    fn synthesize(
        &self,
        input: &SyntheticInput<'_>,
        out: &mut SyntheticMemberSet,
        explicit_methods: &[MethodSummary],
        explicit_fields: &[FieldSummary],
    ) {
        // Only process class-like declarations
        if !matches!(
            input.decl.kind(),
            "class_declaration" | "enum_declaration" | "record_declaration"
        ) {
            return;
        }

        // Load Lombok configuration
        let config = LombokConfig::new();

        // Check for class-level @ToString annotation
        let class_annotations = extract_class_annotations(input.decl, input.ctx, input.type_ctx);
        let class_to_string = find_lombok_annotation(&class_annotations, annotations::TO_STRING);

        if let Some(class_annotation) = class_to_string {
            // Generate toString() method based on class-level annotation
            generate_to_string_method(
                input,
                &class_annotation,
                explicit_fields,
                explicit_methods,
                &config,
                out,
            );
        }
    }
}

/// Extract annotations from the class declaration
fn extract_class_annotations(
    decl: Node,
    ctx: &JavaContextExtractor,
    type_ctx: &SourceTypeCtx,
) -> Vec<AnnotationSummary> {
    if let Some(modifiers) = first_child_of_kind(decl, "modifiers") {
        parse_annotations_in_node(ctx, modifiers, type_ctx)
    } else {
        Vec::new()
    }
}

/// Generate the toString() method
fn generate_to_string_method(
    _input: &SyntheticInput<'_>,
    annotation: &crate::index::AnnotationSummary,
    explicit_fields: &[FieldSummary],
    explicit_methods: &[MethodSummary],
    config: &LombokConfig,
    out: &mut SyntheticMemberSet,
) {
    // Check if toString() already exists (check name and empty params, ignore return type format)
    if explicit_methods
        .iter()
        .any(|m| m.name.as_ref() == "toString" && m.params.is_empty())
    {
        return;
    }

    // Parse annotation parameters
    let _include_field_names = get_bool_param(annotation, "includeFieldNames", true);
    let _call_super = get_bool_param(annotation, "callSuper", false);
    let _do_not_use_getters = get_bool_param(
        annotation,
        "doNotUseGetters",
        config
            .get_bool("lombok.toString.doNotUseGetters")
            .unwrap_or(false),
    );
    let only_explicitly_included = get_bool_param(annotation, "onlyExplicitlyIncluded", false);

    // Get exclude and of parameters
    let exclude_fields = get_string_array_param(annotation, "exclude");
    let of_fields = get_string_array_param(annotation, "of");

    // Determine which fields to include
    let _fields_to_include = determine_fields_to_include(
        explicit_fields,
        &exclude_fields,
        &of_fields,
        only_explicitly_included,
    );
    let only_explicitly_included = get_bool_param(annotation, "onlyExplicitlyIncluded", false);

    // Get exclude and of parameters
    let exclude_fields = get_string_array_param(annotation, "exclude");
    let of_fields = get_string_array_param(annotation, "of");

    // Determine which fields to include
    let fields_to_include = determine_fields_to_include(
        explicit_fields,
        &exclude_fields,
        &of_fields,
        only_explicitly_included,
    );

    // Generate method signature
    let method_name: Arc<str> = Arc::from("toString");
    let return_type: Arc<str> = Arc::from("java/lang/String");

    // Add to methods
    out.methods.push(MethodSummary {
        name: method_name.clone(),
        params: MethodParams { items: Vec::new() },
        annotations: Vec::new(),
        access_flags: rust_asm::constants::ACC_PUBLIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(return_type),
    });

    // Add to definitions
    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: method_name,
        descriptor: Some(Arc::from("()Ljava/lang/String;")),
        origin: SyntheticOrigin::LombokToString,
    });
}

/// Determine which fields should be included in toString()
fn determine_fields_to_include(
    explicit_fields: &[FieldSummary],
    exclude_fields: &[Arc<str>],
    of_fields: &[Arc<str>],
    only_explicitly_included: bool,
) -> Vec<Arc<str>> {
    let mut result = Vec::new();

    for field in explicit_fields {
        let field_name = field.name.as_ref();

        // Skip static fields
        if field.access_flags & rust_asm::constants::ACC_STATIC != 0 {
            continue;
        }

        // If 'of' is specified, only include those fields
        if !of_fields.is_empty() {
            if of_fields.iter().any(|f| f.as_ref() == field_name) {
                result.push(field.name.clone());
            }
            continue;
        }

        // Skip excluded fields
        if exclude_fields.iter().any(|f| f.as_ref() == field_name) {
            continue;
        }

        // Check for @ToString.Include and @ToString.Exclude annotations
        let has_include = field.annotations.iter().any(|a| {
            a.internal_name.as_ref() == "lombok/ToString$Include"
                || a.internal_name.as_ref() == "ToString$Include"
        });

        let has_exclude = field.annotations.iter().any(|a| {
            a.internal_name.as_ref() == "lombok/ToString$Exclude"
                || a.internal_name.as_ref() == "ToString$Exclude"
        });

        if has_exclude {
            continue;
        }

        if only_explicitly_included {
            if has_include {
                result.push(field.name.clone());
            }
        } else {
            // Include by default (unless excluded)
            result.push(field.name.clone());
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_determine_fields_to_include_all() {
        let fields = vec![
            create_test_field("name", false),
            create_test_field("age", false),
            create_test_field("CONSTANT", true), // static
        ];

        let result = determine_fields_to_include(&fields, &[], &[], false);

        assert_eq!(result.len(), 2);
        assert!(result.iter().any(|f| f.as_ref() == "name"));
        assert!(result.iter().any(|f| f.as_ref() == "age"));
    }

    #[test]
    fn test_determine_fields_to_include_with_exclude() {
        let fields = vec![
            create_test_field("name", false),
            create_test_field("age", false),
            create_test_field("password", false),
        ];

        let exclude = vec![Arc::from("password")];
        let result = determine_fields_to_include(&fields, &exclude, &[], false);

        assert_eq!(result.len(), 2);
        assert!(result.iter().any(|f| f.as_ref() == "name"));
        assert!(result.iter().any(|f| f.as_ref() == "age"));
        assert!(!result.iter().any(|f| f.as_ref() == "password"));
    }

    #[test]
    fn test_determine_fields_to_include_with_of() {
        let fields = vec![
            create_test_field("name", false),
            create_test_field("age", false),
            create_test_field("email", false),
        ];

        let of = vec![Arc::from("name"), Arc::from("email")];
        let result = determine_fields_to_include(&fields, &[], &of, false);

        assert_eq!(result.len(), 2);
        assert!(result.iter().any(|f| f.as_ref() == "name"));
        assert!(result.iter().any(|f| f.as_ref() == "email"));
        assert!(!result.iter().any(|f| f.as_ref() == "age"));
    }

    fn create_test_field(name: &str, is_static: bool) -> FieldSummary {
        FieldSummary {
            name: Arc::from(name),
            descriptor: Arc::from("Ljava/lang/String;"),
            access_flags: if is_static {
                rust_asm::constants::ACC_STATIC
            } else {
                0
            },
            annotations: Vec::new(),
            is_synthetic: false,
            generic_signature: None,
        }
    }
}
