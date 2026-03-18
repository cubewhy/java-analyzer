use std::sync::Arc;
use tree_sitter::Node;
use tree_sitter_utils::traversal::first_child_of_kind;

use crate::{
    index::{AnnotationValue, FieldSummary, MethodParam, MethodParams, MethodSummary},
    language::java::{
        JavaContextExtractor,
        lombok::{
            types::annotations,
            utils::{find_lombok_annotation, get_annotation_value},
        },
        members::{extract_class_members_from_body, parse_annotations_in_node},
        synthetic::{
            SyntheticDefinition, SyntheticDefinitionKind, SyntheticInput, SyntheticMemberRule,
            SyntheticMemberSet, SyntheticOrigin,
        },
        type_ctx::SourceTypeCtx,
    },
    semantic::context::CurrentClassMember,
};

pub struct DelegateRule;

impl SyntheticMemberRule for DelegateRule {
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
            "class_declaration" | "interface_declaration"
        ) {
            return;
        }

        // Process each field with @Delegate annotation
        for field in explicit_fields {
            if let Some(delegate_anno) =
                find_lombok_annotation(&field.annotations, annotations::DELEGATE)
            {
                // Skip static fields
                if (field.access_flags & rust_asm::constants::ACC_STATIC) != 0 {
                    continue;
                }

                generate_delegate_methods(input, field, delegate_anno, explicit_methods, out);
            }
        }
    }
}

/// Generate delegate methods for a field
fn generate_delegate_methods(
    input: &SyntheticInput<'_>,
    field: &FieldSummary,
    annotation: &crate::index::AnnotationSummary,
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    // Parse types parameter (which interfaces/classes to delegate)
    let types_to_delegate = get_types_parameter(annotation);

    // Parse excludes parameter (which types to exclude)
    let _types_to_exclude = get_excludes_parameter(annotation);

    // If types are explicitly specified, try to find and parse them
    if !types_to_delegate.is_empty() {
        for type_name in &types_to_delegate {
            // Try to find the interface/class in the same file
            if let Some(methods) = find_type_methods(input, type_name) {
                // Generate delegate methods for each method in the interface
                for method in methods {
                    generate_delegate_method(field, &method, explicit_methods, out);
                }
            }
        }
    }

    // Always add a marker for IDE integration
    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: Arc::from(format!("$delegate${}", field.name)),
        descriptor: None,
        origin: SyntheticOrigin::LombokDelegate {
            field_name: Arc::clone(&field.name),
        },
    });
}

/// Find methods in a type (interface or class) within the same source file
fn find_type_methods(input: &SyntheticInput<'_>, type_name: &str) -> Option<Vec<MethodSummary>> {
    // Extract simple name from type descriptor (e.g., "LFilter;" -> "Filter")
    let simple_name = type_name
        .trim_start_matches('L')
        .trim_end_matches(';')
        .split('/')
        .last()
        .unwrap_or(type_name);

    // First, search for nested types within the current class
    if let Some(body) = input.decl.child_by_field_name("body") {
        let mut cursor = body.walk();
        for child in body.children(&mut cursor) {
            if matches!(child.kind(), "interface_declaration" | "class_declaration") {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = input.ctx.node_text(name_node);
                    if name == simple_name {
                        // Found the nested type! Extract its methods
                        return extract_methods_from_type(input, child);
                    }
                }
            }
        }
    }

    // If not found as nested type, search at the top level
    let root = input.decl.parent()?;
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        if matches!(child.kind(), "interface_declaration" | "class_declaration") {
            // Check if this is the type we're looking for
            if let Some(name_node) = child.child_by_field_name("name") {
                let name = input.ctx.node_text(name_node);
                if name == simple_name {
                    // Found the type! Extract its methods
                    return extract_methods_from_type(input, child);
                }
            }
        }
    }

    None
}

/// Extract methods from a type declaration
fn extract_methods_from_type(
    input: &SyntheticInput<'_>,
    type_node: Node,
) -> Option<Vec<MethodSummary>> {
    let body = type_node.child_by_field_name("body")?;
    let members = extract_class_members_from_body(input.ctx, body, input.type_ctx);

    let methods: Vec<MethodSummary> = members
        .iter()
        .filter_map(|member| match member {
            CurrentClassMember::Method(method) => Some((**method).clone()),
            _ => None,
        })
        .collect();

    Some(methods)
}

/// Generate a single delegate method
fn generate_delegate_method(
    field: &FieldSummary,
    source_method: &MethodSummary,
    explicit_methods: &[MethodSummary],
    out: &mut SyntheticMemberSet,
) {
    // Don't generate if method already exists
    let method_name = source_method.name.as_ref();
    let method_descriptor = source_method.desc();

    if has_method(explicit_methods, method_name, method_descriptor.as_ref()) {
        return;
    }

    // Don't delegate Object methods
    if is_object_method(method_name) {
        return;
    }

    // Generate the delegate method with public access
    let access_flags = rust_asm::constants::ACC_PUBLIC;

    out.methods.push(MethodSummary {
        name: Arc::clone(&source_method.name),
        params: source_method.params.clone(),
        annotations: vec![],
        access_flags,
        is_synthetic: false,
        generic_signature: source_method.generic_signature.clone(),
        return_type: source_method.return_type.clone(),
    });

    out.definitions.push(SyntheticDefinition {
        kind: SyntheticDefinitionKind::Method,
        name: Arc::clone(&source_method.name),
        descriptor: Some(method_descriptor),
        origin: SyntheticOrigin::LombokDelegate {
            field_name: Arc::clone(&field.name),
        },
    });
}

/// Check if a method is an Object method that should not be delegated
fn is_object_method(method_name: &str) -> bool {
    matches!(
        method_name,
        "equals"
            | "hashCode"
            | "toString"
            | "clone"
            | "finalize"
            | "getClass"
            | "notify"
            | "notifyAll"
            | "wait"
    )
}

/// Check if a method with the given name and descriptor already exists
fn has_method(methods: &[MethodSummary], name: &str, descriptor: &str) -> bool {
    methods
        .iter()
        .any(|method| method.name.as_ref() == name && method.desc().as_ref() == descriptor)
}

/// Get the types parameter from @Delegate annotation
fn get_types_parameter(annotation: &crate::index::AnnotationSummary) -> Vec<Arc<str>> {
    if let Some(value) = get_annotation_value(annotation, "types") {
        match value {
            AnnotationValue::Array(items) => items
                .iter()
                .filter_map(|item| match item {
                    AnnotationValue::Class(class_name) => Some(Arc::clone(class_name)),
                    _ => None,
                })
                .collect(),
            AnnotationValue::Class(class_name) => vec![Arc::clone(class_name)],
            _ => vec![],
        }
    } else {
        vec![]
    }
}

/// Get the excludes parameter from @Delegate annotation
fn get_excludes_parameter(annotation: &crate::index::AnnotationSummary) -> Vec<Arc<str>> {
    if let Some(value) = get_annotation_value(annotation, "excludes") {
        match value {
            AnnotationValue::Array(items) => items
                .iter()
                .filter_map(|item| match item {
                    AnnotationValue::Class(class_name) => Some(Arc::clone(class_name)),
                    _ => None,
                })
                .collect(),
            AnnotationValue::Class(class_name) => vec![Arc::clone(class_name)],
            _ => vec![],
        }
    } else {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::AnnotationSummary;
    use crate::language::java::{make_java_parser, scope::extract_imports, scope::extract_package};
    use rust_asm::constants::{ACC_PRIVATE, ACC_STATIC};
    use rustc_hash::FxHashMap;

    fn parse_env(src: &str) -> (JavaContextExtractor, tree_sitter::Tree, SourceTypeCtx) {
        let ctx = JavaContextExtractor::for_indexing(src, None);
        let mut parser = make_java_parser();
        let tree = parser.parse(src, None).expect("parse");
        let root = tree.root_node();
        let type_ctx = SourceTypeCtx::new(
            extract_package(&ctx, root),
            extract_imports(&ctx, root),
            None,
        );
        (ctx, tree, type_ctx)
    }

    fn first_decl(root: Node) -> Node {
        root.named_children(&mut root.walk())
            .find(|node| matches!(node.kind(), "class_declaration" | "interface_declaration"))
            .expect("type declaration")
    }

    #[test]
    fn test_delegate_generates_marker() {
        let src = r#"
            import lombok.experimental.Delegate;
            import java.util.List;
            
            class MyList {
                @Delegate
                private List<String> items;
            }
        "#;
        let (ctx, tree, type_ctx) = parse_env(src);
        let decl = first_decl(tree.root_node());

        let synthetic = crate::language::java::synthetic::synthesize_for_type(
            &ctx,
            decl,
            Some("MyList"),
            &type_ctx,
            &[],
            &[FieldSummary {
                name: Arc::from("items"),
                descriptor: Arc::from("Ljava/util/List;"),
                access_flags: ACC_PRIVATE,
                annotations: vec![AnnotationSummary {
                    internal_name: Arc::from("lombok/experimental/Delegate"),
                    runtime_visible: true,
                    elements: FxHashMap::default(),
                }],
                is_synthetic: false,
                generic_signature: None,
            }],
        );

        // Should generate a delegate marker
        assert!(
            !synthetic.definitions.is_empty(),
            "Should generate delegate definitions"
        );

        let has_delegate_marker = synthetic
            .definitions
            .iter()
            .any(|d| matches!(d.origin, SyntheticOrigin::LombokDelegate { .. }));
        assert!(has_delegate_marker, "Should have delegate marker");
    }

    #[test]
    fn test_delegate_not_generated_for_static_field() {
        let src = r#"
            import lombok.experimental.Delegate;
            import java.util.List;
            
            class MyList {
                @Delegate
                private static List<String> SHARED_LIST;
            }
        "#;
        let (ctx, tree, type_ctx) = parse_env(src);
        let decl = first_decl(tree.root_node());

        let synthetic = crate::language::java::synthetic::synthesize_for_type(
            &ctx,
            decl,
            Some("MyList"),
            &type_ctx,
            &[],
            &[FieldSummary {
                name: Arc::from("SHARED_LIST"),
                descriptor: Arc::from("Ljava/util/List;"),
                access_flags: ACC_PRIVATE | ACC_STATIC,
                annotations: vec![AnnotationSummary {
                    internal_name: Arc::from("lombok/experimental/Delegate"),
                    runtime_visible: true,
                    elements: FxHashMap::default(),
                }],
                is_synthetic: false,
                generic_signature: None,
            }],
        );

        // Should not generate delegate for static field
        let has_delegate_marker = synthetic
            .definitions
            .iter()
            .any(|d| matches!(d.origin, SyntheticOrigin::LombokDelegate { .. }));
        assert!(
            !has_delegate_marker,
            "Should not generate delegate for static field"
        );
    }

    #[test]
    fn test_delegate_with_types_parameter() {
        let src = r#"
            import lombok.experimental.Delegate;
            import java.util.Collection;
            
            class MyCollection {
                @Delegate(types = Collection.class)
                private java.util.ArrayList<String> items;
            }
        "#;
        let (ctx, tree, type_ctx) = parse_env(src);
        let decl = first_decl(tree.root_node());

        let mut elements = FxHashMap::default();
        elements.insert(
            Arc::from("types"),
            AnnotationValue::Class(Arc::from("Ljava/util/Collection;")),
        );

        let synthetic = crate::language::java::synthetic::synthesize_for_type(
            &ctx,
            decl,
            Some("MyCollection"),
            &type_ctx,
            &[],
            &[FieldSummary {
                name: Arc::from("items"),
                descriptor: Arc::from("Ljava/util/ArrayList;"),
                access_flags: ACC_PRIVATE,
                annotations: vec![AnnotationSummary {
                    internal_name: Arc::from("lombok/experimental/Delegate"),
                    runtime_visible: true,
                    elements,
                }],
                is_synthetic: false,
                generic_signature: None,
            }],
        );

        // Should generate delegate marker
        let has_delegate_marker = synthetic
            .definitions
            .iter()
            .any(|d| matches!(d.origin, SyntheticOrigin::LombokDelegate { .. }));
        assert!(
            has_delegate_marker,
            "Should generate delegate marker with types parameter"
        );
    }

    #[test]
    fn test_delegate_with_excludes_parameter() {
        let src = r#"
            import lombok.experimental.Delegate;
            import java.util.List;
            
            class MyList {
                @Delegate(excludes = java.util.Collection.class)
                private List<String> items;
            }
        "#;
        let (ctx, tree, type_ctx) = parse_env(src);
        let decl = first_decl(tree.root_node());

        let mut elements = FxHashMap::default();
        elements.insert(
            Arc::from("excludes"),
            AnnotationValue::Class(Arc::from("Ljava/util/Collection;")),
        );

        let synthetic = crate::language::java::synthetic::synthesize_for_type(
            &ctx,
            decl,
            Some("MyList"),
            &type_ctx,
            &[],
            &[FieldSummary {
                name: Arc::from("items"),
                descriptor: Arc::from("Ljava/util/List;"),
                access_flags: ACC_PRIVATE,
                annotations: vec![AnnotationSummary {
                    internal_name: Arc::from("lombok/experimental/Delegate"),
                    runtime_visible: true,
                    elements,
                }],
                is_synthetic: false,
                generic_signature: None,
            }],
        );

        // Should generate delegate marker
        let has_delegate_marker = synthetic
            .definitions
            .iter()
            .any(|d| matches!(d.origin, SyntheticOrigin::LombokDelegate { .. }));
        assert!(
            has_delegate_marker,
            "Should generate delegate marker with excludes parameter"
        );
    }
}
