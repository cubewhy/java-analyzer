use super::config::LombokConfig;
use super::types::{AccessLevel, annotations};
use crate::index::{AnnotationSummary, AnnotationValue, FieldSummary};
use std::sync::Arc;

/// Check if annotations contain a specific Lombok annotation
/// Checks both full internal name (lombok/Getter) and simple name (Getter)
pub fn has_lombok_annotation(annotations: &[AnnotationSummary], internal_name: &str) -> bool {
    let simple_name = internal_name.split('/').last().unwrap_or(internal_name);
    annotations.iter().any(|anno| {
        let anno_name = anno.internal_name.as_ref();
        anno_name == internal_name || anno_name == simple_name
    })
}

/// Find a Lombok annotation by internal name
/// Checks both full internal name (lombok/Getter) and simple name (Getter)
pub fn find_lombok_annotation<'a>(
    annotations: &'a [AnnotationSummary],
    internal_name: &str,
) -> Option<&'a AnnotationSummary> {
    let simple_name = internal_name.split('/').last().unwrap_or(internal_name);
    annotations.iter().find(|anno| {
        let anno_name = anno.internal_name.as_ref();
        anno_name == internal_name || anno_name == simple_name
    })
}

/// Get annotation element value by key
pub fn get_annotation_value<'a>(
    anno: &'a AnnotationSummary,
    key: &str,
) -> Option<&'a AnnotationValue> {
    anno.elements.get(key)
}

/// Parse AccessLevel from annotation (defaults to PUBLIC if not specified)
pub fn parse_access_level(anno: &AnnotationSummary) -> AccessLevel {
    get_annotation_value(anno, "value")
        .and_then(AccessLevel::from_annotation_value)
        .unwrap_or(AccessLevel::Public)
}

/// Check if a field should be excluded based on annotation parameters
pub fn is_field_excluded(field_name: &str, anno: &AnnotationSummary) -> bool {
    // Check 'exclude' parameter
    if let Some(exclude_value) = get_annotation_value(anno, "exclude") {
        if matches_field_in_array(field_name, exclude_value) {
            return true;
        }
    }

    // Check 'of' parameter (if present, only include listed fields)
    if let Some(of_value) = get_annotation_value(anno, "of") {
        return !matches_field_in_array(field_name, of_value);
    }

    false
}

/// Check if field name matches any value in an annotation array
fn matches_field_in_array(field_name: &str, value: &AnnotationValue) -> bool {
    match value {
        AnnotationValue::String(s) => s.as_ref() == field_name,
        AnnotationValue::Array(items) => items.iter().any(|item| {
            if let AnnotationValue::String(s) = item {
                s.as_ref() == field_name
            } else {
                false
            }
        }),
        _ => false,
    }
}

/// Check if field should generate getter/setter
pub fn should_generate_for_field(
    field: &FieldSummary,
    class_level_anno: Option<&AnnotationSummary>,
    field_level_anno: Option<&AnnotationSummary>,
) -> bool {
    use rust_asm::constants::ACC_STATIC;

    let is_static = (field.access_flags & ACC_STATIC) != 0;

    // Field-level annotation takes precedence
    if let Some(anno) = field_level_anno {
        // Field-level annotation on static field is allowed (generates static getter)
        // Check for AccessLevel.NONE
        let access = parse_access_level(anno);
        return access != AccessLevel::None;
    }

    // Class-level annotation: skip static fields
    if is_static {
        return false;
    }

    // Check class-level annotation
    if let Some(anno) = class_level_anno {
        // Check if field is excluded
        if is_field_excluded(field.name.as_ref(), anno) {
            return false;
        }

        // Check for AccessLevel.NONE
        let access = parse_access_level(anno);
        return access != AccessLevel::None;
    }

    false
}

/// Compute getter method name for a field
pub fn compute_getter_name(
    field_name: &str,
    field_descriptor: &str,
    config: &LombokConfig,
) -> String {
    let base_name = strip_field_prefix(field_name, config);

    if config.accessors_fluent() {
        return base_name.to_string();
    }

    // Check if it's a boolean field
    let is_boolean = field_descriptor == "Z";
    let prefix = if is_boolean { "is" } else { "get" };

    format!("{}{}", prefix, capitalize_first(base_name))
}

/// Compute setter method name for a field
pub fn compute_setter_name(field_name: &str, config: &LombokConfig) -> String {
    let base_name = strip_field_prefix(field_name, config);

    if config.accessors_fluent() {
        return base_name.to_string();
    }

    format!("set{}", capitalize_first(base_name))
}

/// Strip configured prefixes from field name
pub fn strip_field_prefix<'a>(field_name: &'a str, config: &LombokConfig) -> &'a str {
    let prefixes = config.accessors_prefix();

    for prefix in &prefixes {
        if field_name.starts_with(prefix) {
            let stripped = &field_name[prefix.len()..];
            // Only strip if there's something left
            if !stripped.is_empty() {
                return stripped;
            }
        }
    }

    field_name
}

/// Capitalize first character of a string
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => {
            let mut result = first.to_uppercase().to_string();
            result.push_str(chars.as_str());
            result
        }
    }
}

/// Check if a field is final
pub fn is_field_final(field: &FieldSummary) -> bool {
    use rust_asm::constants::ACC_FINAL;
    (field.access_flags & ACC_FINAL) != 0
}

/// Check if a field has @NonNull annotation
pub fn is_field_non_null(field: &FieldSummary) -> bool {
    has_lombok_annotation(&field.annotations, annotations::NON_NULL)
}

/// Copy annotations that should be propagated to generated methods
pub fn copy_annotations(
    source: &[AnnotationSummary],
    config: &LombokConfig,
) -> Vec<AnnotationSummary> {
    let copyable_patterns = config.copyable_annotations();

    if copyable_patterns.is_empty() {
        return vec![];
    }

    source
        .iter()
        .filter(|anno| {
            let name = anno.internal_name.as_ref();
            copyable_patterns.iter().any(|pattern| {
                // Simple pattern matching: support * wildcard
                if pattern.contains('*') {
                    let pattern = pattern.replace('.', "/");
                    matches_pattern(name, &pattern)
                } else {
                    let pattern = pattern.replace('.', "/");
                    name == pattern
                }
            })
        })
        .cloned()
        .collect()
}

/// Simple wildcard pattern matching
fn matches_pattern(text: &str, pattern: &str) -> bool {
    if !pattern.contains('*') {
        return text == pattern;
    }

    let parts: Vec<&str> = pattern.split('*').collect();

    if parts.len() == 2 {
        let (prefix, suffix) = (parts[0], parts[1]);
        return text.starts_with(prefix) && text.ends_with(suffix);
    }

    // More complex patterns - simple implementation
    let mut text_pos = 0;
    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }

        if i == 0 {
            // First part must match at start
            if !text[text_pos..].starts_with(part) {
                return false;
            }
            text_pos += part.len();
        } else if i == parts.len() - 1 {
            // Last part must match at end
            return text[text_pos..].ends_with(part);
        } else {
            // Middle parts
            if let Some(pos) = text[text_pos..].find(part) {
                text_pos += pos + part.len();
            } else {
                return false;
            }
        }
    }

    true
}

/// Get boolean annotation parameter value
pub fn get_bool_param(anno: &AnnotationSummary, key: &str, default: bool) -> bool {
    get_annotation_value(anno, key)
        .and_then(|v| match v {
            AnnotationValue::Boolean(b) => Some(*b),
            _ => None,
        })
        .unwrap_or(default)
}

/// Get string annotation parameter value
pub fn get_string_param<'a>(anno: &'a AnnotationSummary, key: &str) -> Option<&'a str> {
    get_annotation_value(anno, key).and_then(|v| match v {
        AnnotationValue::String(s) => Some(s.as_ref()),
        _ => None,
    })
}

/// Get string array annotation parameter value
pub fn get_string_array_param(anno: &AnnotationSummary, key: &str) -> Vec<Arc<str>> {
    get_annotation_value(anno, key)
        .map(|v| match v {
            AnnotationValue::String(s) => vec![Arc::clone(s)],
            AnnotationValue::Array(items) => items
                .iter()
                .filter_map(|item| match item {
                    AnnotationValue::String(s) => Some(Arc::clone(s)),
                    _ => None,
                })
                .collect(),
            _ => vec![],
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rustc_hash::FxHashMap;

    fn make_annotation(
        internal_name: &str,
        elements: Vec<(&str, AnnotationValue)>,
    ) -> AnnotationSummary {
        AnnotationSummary {
            internal_name: Arc::from(internal_name),
            runtime_visible: true,
            elements: elements
                .into_iter()
                .map(|(k, v)| (Arc::from(k), v))
                .collect(),
        }
    }

    #[test]
    fn test_has_lombok_annotation() {
        let annos = vec![
            make_annotation("lombok/Getter", vec![]),
            make_annotation("lombok/Setter", vec![]),
        ];

        assert!(has_lombok_annotation(&annos, "lombok/Getter"));
        assert!(has_lombok_annotation(&annos, "lombok/Setter"));
        assert!(!has_lombok_annotation(&annos, "lombok/ToString"));
    }

    #[test]
    fn test_parse_access_level() {
        let anno = make_annotation(
            "lombok/Getter",
            vec![(
                "value",
                AnnotationValue::Enum {
                    type_name: Arc::from("lombok/AccessLevel"),
                    const_name: Arc::from("PROTECTED"),
                },
            )],
        );

        assert_eq!(parse_access_level(&anno), AccessLevel::Protected);
    }

    #[test]
    fn test_is_field_excluded() {
        let anno = make_annotation(
            "lombok/ToString",
            vec![(
                "exclude",
                AnnotationValue::Array(vec![
                    AnnotationValue::String(Arc::from("password")),
                    AnnotationValue::String(Arc::from("secret")),
                ]),
            )],
        );

        assert!(is_field_excluded("password", &anno));
        assert!(is_field_excluded("secret", &anno));
        assert!(!is_field_excluded("username", &anno));
    }

    #[test]
    fn test_compute_getter_name() {
        let config = LombokConfig::new();

        assert_eq!(
            compute_getter_name("name", "Ljava/lang/String;", &config),
            "getName"
        );
        assert_eq!(compute_getter_name("active", "Z", &config), "isActive");
        assert_eq!(compute_getter_name("count", "I", &config), "getCount");
    }

    #[test]
    fn test_compute_getter_name_with_prefix() {
        let mut config = LombokConfig::new();
        config.merge_from_content("lombok.accessors.prefix = m_");

        assert_eq!(
            compute_getter_name("m_name", "Ljava/lang/String;", &config),
            "getName"
        );
        assert_eq!(compute_getter_name("m_active", "Z", &config), "isActive");
    }

    #[test]
    fn test_compute_getter_name_fluent() {
        let mut config = LombokConfig::new();
        config.merge_from_content("lombok.accessors.fluent = true");

        assert_eq!(
            compute_getter_name("name", "Ljava/lang/String;", &config),
            "name"
        );
        assert_eq!(compute_getter_name("active", "Z", &config), "active");
    }

    #[test]
    fn test_capitalize_first() {
        assert_eq!(capitalize_first("hello"), "Hello");
        assert_eq!(capitalize_first("World"), "World");
        assert_eq!(capitalize_first(""), "");
        assert_eq!(capitalize_first("a"), "A");
    }

    #[test]
    fn test_matches_pattern() {
        assert!(matches_pattern("java/lang/String", "java/lang/String"));
        assert!(matches_pattern("java/lang/String", "java/*"));
        assert!(matches_pattern("java/lang/String", "*/String"));
        assert!(matches_pattern("java/lang/String", "*lang*"));
        assert!(!matches_pattern("java/lang/String", "kotlin/*"));
    }
}
