use std::sync::Arc;
use tree_sitter::Node;
use tree_sitter_utils::traversal::first_child_of_kind;

use crate::{
    index::{AnnotationSummary, FieldSummary, MethodParam, MethodParams, MethodSummary},
    language::java::{
        JavaContextExtractor,
        lombok::{
            config::LombokConfig,
            types::{AccessLevel, LombokConstructorType, annotations},
            utils::{
                compute_getter_name, find_lombok_annotation, get_string_param, parse_access_level,
            },
        },
        members::parse_annotations_in_node,
        synthetic::{
            SyntheticDefinition, SyntheticDefinitionKind, SyntheticInput, SyntheticMemberRule,
            SyntheticMemberSet, SyntheticOrigin,
        },
        type_ctx::SourceTypeCtx,
    },
};

pub struct ValueRule;

impl SyntheticMemberRule for ValueRule {
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

        // Check for class-level @Value annotation
        let class_annotations = extract_class_annotations(input.ctx, input.decl, input.type_ctx);
        let value_annotation = find_lombok_annotation(&class_annotations, annotations::VALUE);

        if let Some(value_anno) = value_annotation {
            // Load Lombok configuration
            let config = LombokConfig::new();

            // @Value = @Getter + @ToString + @EqualsAndHashCode + @AllArgsConstructor
            // + makes class final and fields private final (but we don't modify those here)

            // Check if component annotations are explicitly present (they take precedence)
            let has_explicit_getter =
                find_lombok_annotation(&class_annotations, annotations::GETTER).is_some();
            let has_explicit_to_string =
                find_lombok_annotation(&class_annotations, annotations::TO_STRING).is_some();
            let has_explicit_equals_hash_code =
                find_lombok_annotation(&class_annotations, annotations::EQUALS_AND_HASH_CODE)
                    .is_some();
            let has_explicit_constructor = has_any_explicit_constructor(&class_annotations)
                || has_any_constructor_in_source(explicit_methods);

            // Generate getters (NO setters for @Value - it's immutable)
            if !has_explicit_getter {
                generate_getters_for_value(input, explicit_fields, explicit_methods, &config, out);
            }

            // Generate toString (unless explicit @ToString exists)
            if !has_explicit_to_string {
                generate_to_string_for_value(input, explicit_fields, explicit_methods, out);
            }

            // Generate equals and hashCode (unless explicit @EqualsAndHashCode exists)
            if !has_explicit_equals_hash_code {
                generate_equals_and_hash_code_for_value(input, explicit_methods, out);
            }

            // Generate all-args constructor (unless explicit constructor exists)
            if !has_explicit_constructor {
                generate_all_args_constructor_for_value(
                    value_anno,
                    explicit_fields,
                    explicit_methods,
                    out,
                );
            }
        }
    }
}

/// Extract class-level annotations
fn extract_class_annotations(
    ctx: &JavaContextExtractor,
    decl: Node,
    type_ctx: &SourceTypeCtx,
) -> Vec<AnnotationSummary> {
    first_child_of_kind(decl, "modifiers")
        .map(|modifiers| parse_annotations_in_node(ctx, modifiers, type_ctx))
        .unwrap_or_default()
}

/// Check if any explicit constructor annotation exists
fn has_any_explicit_constructor(class_annotations: &[AnnotationSummary]) -> bool {
    find_lombok_annotation(class_annotations, annotations::NO_ARGS_CONSTRUCTOR).is_some()
        || find_lombok_annotation(class_annotations, annotations::REQUIRED_ARGS_CONSTRUCTOR)
            .is_some()
        || find_lombok_annotation(class_annotations, annotations::ALL_ARGS_CONSTRUCTOR).is_some()
}

/// Check if any constructor exists in the source code
fn has_any_constructor_in_source(explicit_methods: &[MethodSummary]) -> bool {
    explicit_methods.iter().any(|m| m.name.as_ref() == "<init>")
}

/// Generate getters for @Value (NO setters - immutable)
fn generate_getters_for_value(
    _input: &SyntheticInput<'_>,
    explicit_fields: &[FieldSummary],
    explicit_methods: &[MethodSummary],
    config: &LombokConfig,
    out: &mut SyntheticMemberSet,
) {
    use rust_asm::constants::ACC_STATIC;

    for field in explicit_fields {
        // Skip static fields
        if (field.access_flags & ACC_STATIC) != 0 {
            continue;
        }

        // Check for field-level @Getter annotation
        let field_getter = find_lombok_annotation(&field.annotations, annotations::GETTER);

        // Generate getter if not explicitly disabled
        if let Some(field_anno) = field_getter {
            // Field-level annotation exists, check if it's not NONE
            if parse_access_level(field_anno) != AccessLevel::None {
                generate_getter(field, Some(field_anno), config, explicit_methods, out);
            }
        } else {
            // No field-level annotation, generate with default access
            generate_getter(field, None, config, explicit_methods, out);
        }
    }
}

/// Generate a getter method for a field
fn generate_getter(
    field: &FieldSummary,
    annotation: Option<&AnnotationSummary>,
    config: &LombokConfig,
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    let access_level = annotation
        .map(parse_access_level)
        .unwrap_or(AccessLevel::Public);

    if access_level == AccessLevel::None {
        return;
    }

    let getter_name = compute_getter_name(field.name.as_ref(), field.descriptor.as_ref(), config);
    let descriptor = Arc::from(format!("(){}", field.descriptor));

    // Check if method already exists
    if has_method(explicit_methods, &getter_name, &descriptor) {
        return;
    }

    let is_static = (field.access_flags & rust_asm::constants::ACC_STATIC) != 0;
    let mut access_flags = access_level.to_access_flags();
    if is_static {
        access_flags |= rust_asm::constants::ACC_STATIC;
    }

    out.methods.push(MethodSummary {
        name: Arc::from(getter_name.clone()),
        params: MethodParams::empty(),
        annotations: vec![],
        access_flags,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::clone(&field.descriptor)),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: Arc::from(getter_name),
        descriptor: Some(descriptor),
        origin: SyntheticOrigin::LombokGetter {
            field_name: Arc::clone(&field.name),
        },
    });
}

/// Generate toString() method for @Value
fn generate_to_string_for_value(
    _input: &SyntheticInput<'_>,
    _explicit_fields: &[FieldSummary],
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    // Check if toString() already exists (check by name and empty params)
    if explicit_methods
        .iter()
        .any(|m| m.name.as_ref() == "toString" && m.params.is_empty())
    {
        return;
    }

    let method_name: Arc<str> = Arc::from("toString");
    let descriptor: Arc<str> = Arc::from("()Ljava/lang/String;");

    out.methods.push(MethodSummary {
        name: method_name.clone(),
        params: MethodParams::empty(),
        annotations: Vec::new(),
        access_flags: rust_asm::constants::ACC_PUBLIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::from("Ljava/lang/String;")),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: method_name,
        descriptor: Some(descriptor),
        origin: SyntheticOrigin::LombokToString,
    });
}

/// Generate equals() and hashCode() methods for @Value
fn generate_equals_and_hash_code_for_value(
    _input: &SyntheticInput<'_>,
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    let has_equals = has_method(explicit_methods, "equals", "(Ljava/lang/Object;)Z");
    let has_hash_code = has_method(explicit_methods, "hashCode", "()I");

    // If either exists, don't generate any (they must be in sync)
    if has_equals || has_hash_code {
        return;
    }

    // Generate equals() method
    let equals_name: Arc<str> = Arc::from("equals");
    let equals_descriptor: Arc<str> = Arc::from("(Ljava/lang/Object;)Z");

    out.methods.push(MethodSummary {
        name: equals_name.clone(),
        params: MethodParams {
            items: vec![MethodParam {
                descriptor: Arc::from("Ljava/lang/Object;"),
                name: Arc::from("other"),
                annotations: Vec::new(),
            }],
        },
        annotations: Vec::new(),
        access_flags: rust_asm::constants::ACC_PUBLIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::from("Z")),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: equals_name,
        descriptor: Some(equals_descriptor),
        origin: SyntheticOrigin::LombokEquals,
    });

    // Generate hashCode() method
    let hash_code_name: Arc<str> = Arc::from("hashCode");
    let hash_code_descriptor: Arc<str> = Arc::from("()I");

    out.methods.push(MethodSummary {
        name: hash_code_name.clone(),
        params: MethodParams::empty(),
        annotations: Vec::new(),
        access_flags: rust_asm::constants::ACC_PUBLIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::from("I")),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: hash_code_name,
        descriptor: Some(hash_code_descriptor),
        origin: SyntheticOrigin::LombokHashCode,
    });

    // Generate canEqual() method for proper inheritance support
    let can_equal_name: Arc<str> = Arc::from("canEqual");
    let can_equal_descriptor: Arc<str> = Arc::from("(Ljava/lang/Object;)Z");

    out.methods.push(MethodSummary {
        name: can_equal_name.clone(),
        params: MethodParams {
            items: vec![MethodParam {
                descriptor: Arc::from("Ljava/lang/Object;"),
                name: Arc::from("other"),
                annotations: Vec::new(),
            }],
        },
        annotations: Vec::new(),
        access_flags: rust_asm::constants::ACC_PROTECTED,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::from("Z")),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: can_equal_name,
        descriptor: Some(can_equal_descriptor),
        origin: SyntheticOrigin::LombokEquals,
    });
}

/// Generate all-args constructor for @Value
fn generate_all_args_constructor_for_value(
    annotation: &AnnotationSummary,
    explicit_fields: &[FieldSummary],
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};

    // Get all non-static fields
    let all_fields: Vec<&FieldSummary> = explicit_fields
        .iter()
        .filter(|f| (f.access_flags & ACC_STATIC) == 0)
        .collect();

    // Build parameters
    let params = build_constructor_params(&all_fields);

    // Build descriptor
    let descriptor = build_constructor_descriptor(&params);

    // Check if constructor already exists
    if constructor_exists(explicit_methods, &descriptor) {
        return;
    }

    // Generate the constructor
    let constructor = MethodSummary {
        name: Arc::from("<init>"),
        params: params.clone(),
        annotations: Vec::new(),
        access_flags: ACC_PUBLIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: None,
    };

    out.methods.push(constructor);

    // Add synthetic definition
    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: Arc::from("<init>"),
        descriptor: Some(Arc::from(descriptor.as_str())),
        origin: SyntheticOrigin::LombokConstructor {
            constructor_type: LombokConstructorType::AllArgs,
        },
    });

    // Generate static factory method if staticConstructor is specified
    let static_name = get_string_param(annotation, "staticConstructor");
    if let Some(static_name) = static_name {
        if !static_name.is_empty() {
            generate_static_factory_method(
                static_name,
                &params,
                explicit_methods,
                out,
                LombokConstructorType::AllArgs,
            );
        }
    }
}

/// Generate static factory method wrapper
fn generate_static_factory_method(
    static_name: &str,
    params: &MethodParams,
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
    constructor_type: LombokConstructorType,
) {
    use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};

    // Build descriptor for the static method
    let mut descriptor = String::from("(");
    for param in &params.items {
        descriptor.push_str(&param.descriptor);
    }
    // Return type is the class itself - we'll use Object as placeholder
    descriptor.push_str(")Ljava/lang/Object;");

    // Check if method already exists
    if explicit_methods
        .iter()
        .any(|m| m.name.as_ref() == static_name && m.params.len() == params.len())
    {
        return;
    }

    // Generate the static factory method
    let factory_method = MethodSummary {
        name: Arc::from(static_name),
        params: params.clone(),
        annotations: Vec::new(),
        access_flags: ACC_PUBLIC | ACC_STATIC,
        is_synthetic: false,
        generic_signature: None,
        return_type: Some(Arc::from("java/lang/Object")),
    };

    out.methods.push(factory_method);

    // Add synthetic definition
    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: Arc::from(static_name),
        descriptor: Some(Arc::from(descriptor.as_str())),
        origin: SyntheticOrigin::LombokConstructor { constructor_type },
    });
}

/// Check if a method with the given name and descriptor already exists
fn has_method(methods: &[MethodSummary], name: &str, descriptor: &str) -> bool {
    methods
        .iter()
        .any(|method| method.name.as_ref() == name && method.desc().as_ref() == descriptor)
}

/// Check if a constructor with the given descriptor already exists
fn constructor_exists(explicit_methods: &[MethodSummary], descriptor: &str) -> bool {
    explicit_methods
        .iter()
        .any(|m| m.name.as_ref() == "<init>" && m.desc().as_ref() == descriptor)
}

/// Build constructor parameters from fields
fn build_constructor_params(fields: &[&FieldSummary]) -> MethodParams {
    let items = fields
        .iter()
        .map(|f| MethodParam {
            descriptor: f.descriptor.clone(),
            name: f.name.clone(),
            annotations: f
                .annotations
                .iter()
                .filter(|a| is_copyable_annotation(a))
                .cloned()
                .collect(),
        })
        .collect();

    MethodParams { items }
}

/// Check if annotation should be copied to constructor parameter
fn is_copyable_annotation(annotation: &AnnotationSummary) -> bool {
    let name = annotation.internal_name.as_ref();
    name.contains("NonNull")
        || name.contains("NotNull")
        || name.contains("Nonnull")
        || name.contains("Nullable")
}

/// Build constructor descriptor from parameters
fn build_constructor_descriptor(params: &MethodParams) -> String {
    let mut descriptor = String::from("(");
    for param in &params.items {
        descriptor.push_str(&param.descriptor);
    }
    descriptor.push_str(")V");
    descriptor
}
