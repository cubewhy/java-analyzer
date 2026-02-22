use rust_asm::constants::{
    ACC_ABSTRACT, ACC_FINAL, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC, ACC_STATIC,
};
use std::sync::Arc;
use tracing::debug;
use tree_sitter::{Node, Parser, Query};

use super::{
    ClassMetadata, ClassOrigin, FieldSummary, MethodSummary, parse_return_type_from_descriptor,
};
use crate::language::ts_utils::{capture_text, run_query};

/// Parse the source file string and return all classes defined within it.
pub fn parse_source_str(source: &str, lang: &str, origin: ClassOrigin) -> Vec<ClassMetadata> {
    match lang {
        "java" => parse_java_source(source, origin),
        "kotlin" => parse_kotlin_source(source, origin),
        other => {
            debug!("unsupported source lang: {}", other);
            vec![]
        }
    }
}

/// Parse from file path (automatically determines language)
pub fn parse_source_file(path: &std::path::Path, origin: ClassOrigin) -> Vec<ClassMetadata> {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            debug!("failed to read {:?}: {}", path, e);
            return vec![];
        }
    };
    let lang = match path.extension().and_then(|e| e.to_str()) {
        Some("java") => "java",
        Some("kt") => "kotlin",
        _ => return vec![],
    };
    parse_source_str(&content, lang, origin)
}

pub fn parse_java_source(source: &str, origin: ClassOrigin) -> Vec<ClassMetadata> {
    let mut parser = make_java_parser();
    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return vec![],
    };
    let root = tree.root_node();
    let bytes = source.as_bytes();

    let package = extract_java_package(root, bytes);
    let mut results = Vec::new();
    collect_java_classes(root, bytes, &package, None, &origin, &mut results);
    results
}

fn collect_java_classes(
    node: Node,
    bytes: &[u8],
    package: &Option<Arc<str>>,
    outer_class: Option<Arc<str>>,
    origin: &ClassOrigin,
    out: &mut Vec<ClassMetadata>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "class_declaration"
            | "interface_declaration"
            | "enum_declaration"
            | "annotation_type_declaration"
            | "record_declaration" => {
                if let Some(meta) =
                    parse_java_class(child, bytes, package, outer_class.clone(), origin)
                {
                    // Recursive processing of inner classes
                    let inner_outer = Some(Arc::clone(&meta.name));
                    if let Some(body) = child.child_by_field_name("body") {
                        collect_java_classes(body, bytes, package, inner_outer, origin, out);
                    }
                    out.push(meta);
                }
            }
            _ => {
                // Continue searching downwards (e.g., inner classes within class_body)
                collect_java_classes(child, bytes, package, outer_class.clone(), origin, out);
            }
        }
    }
}

fn parse_java_class(
    node: Node,
    bytes: &[u8],
    package: &Option<Arc<str>>,
    outer_class: Option<Arc<str>>,
    origin: &ClassOrigin,
) -> Option<ClassMetadata> {
    let name_node = node.child_by_field_name("name")?;
    let class_name = node_text(name_node, bytes);
    if class_name.is_empty() {
        return None;
    }

    let name: Arc<str> = Arc::from(class_name);
    let internal_name: Arc<str> = match (package, &outer_class) {
        (Some(pkg), Some(outer)) => Arc::from(format!("{}/{}${}", pkg, outer, class_name).as_str()),
        (Some(pkg), None) => Arc::from(format!("{}/{}", pkg, class_name).as_str()),
        (None, Some(outer)) => Arc::from(format!("{}${}", outer, class_name).as_str()),
        (None, None) => Arc::clone(&name),
    };

    // super class
    let super_name = node
        .child_by_field_name("superclass")
        .map(|n| node_text(n, bytes).to_string());

    // interfaces
    let interfaces = node
        .child_by_field_name("interfaces")
        .map(|iface_node| {
            let q_src = r#"(type_identifier) @t"#;
            if let Ok(q) = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src) {
                let idx = q.capture_index_for_name("t").unwrap();
                run_query(&q, iface_node, bytes, None)
                    .into_iter()
                    .filter_map(|caps| capture_text(&caps, idx, bytes).map(|s| s.to_string()))
                    .collect()
            } else {
                vec![]
            }
        })
        .unwrap_or_default();

    // methods & fields
    let body = node.child_by_field_name("body");
    let (methods, fields) = body
        .map(|b| {
            let methods = extract_java_methods(b, bytes);
            let fields = extract_java_fields(b, bytes);
            (methods, fields)
        })
        .unwrap_or_default();

    let access_flags = extract_java_access_flags(node, bytes);

    Some(ClassMetadata {
        package: package.clone(),
        name,
        internal_name,
        super_name,
        interfaces,
        methods,
        fields,
        access_flags,
        inner_class_of: outer_class,
        origin: origin.clone(),
    })
}

fn extract_java_methods(body: Node, bytes: &[u8]) -> Vec<MethodSummary> {
    let mut results = Vec::new();
    let mut cursor = body.walk();

    for child in body.children(&mut cursor) {
        if child.kind() != "method_declaration" {
            continue;
        }
        if let Some(m) = parse_java_method(child, bytes) {
            results.push(m);
        }
    }
    results
}

fn parse_java_method(node: Node, bytes: &[u8]) -> Option<MethodSummary> {
    // method_declaration child nodes in order:
    // modifiers? -> return_type(void_type|integral_type|type_identifier|...) -> identifier(name) -> formal_parameters → block?
    //
    // Strategy: Traverse all named child nodes, identifying roles by kind
    let mut method_name: Option<&str> = None;
    let mut return_type_text: Option<&str> = None;
    let mut params_text: Option<&str> = None;
    let mut access_flags: u16 = ACC_PUBLIC;

    let mut walker = node.walk();
    for child in node.children(&mut walker) {
        match child.kind() {
            "modifiers" => {
                access_flags = parse_java_modifiers(node_text(child, bytes));
            }
            "void_type"
            | "integral_type"      // int, long, byte, short, char
            | "floating_point_type" // float, double
            | "boolean_type"
            | "type_identifier"    // references type
            | "array_type"
            | "generic_type"
            | "scoped_type_identifier" => {
                if return_type_text.is_none() {
                    return_type_text = Some(node_text(child, bytes));
                }
            }
            "identifier" => {
                // The first identifier is the method name
                if method_name.is_none() {
                    method_name = Some(node_text(child, bytes));
                }
            }
            "formal_parameters" => {
                params_text = Some(node_text(child, bytes));
            }
            _ => {}
        }
    }

    let name = method_name?;
    let ret = return_type_text.unwrap_or("void");
    let params = params_text.unwrap_or("()");

    let descriptor = build_java_descriptor(params, ret);
    let return_type = parse_return_type_from_descriptor(&descriptor);

    Some(MethodSummary {
        name: Arc::from(name),
        descriptor: Arc::from(descriptor.as_str()),
        access_flags,
        is_synthetic: false,
        generic_signature: None,
        return_type,
    })
}

fn extract_java_fields(body: Node, bytes: &[u8]) -> Vec<FieldSummary> {
    let mut results = Vec::new();
    let mut cursor = body.walk();

    for child in body.children(&mut cursor) {
        if child.kind() != "field_declaration" {
            continue;
        }
        results.extend(parse_java_field(child, bytes));
    }
    results
}

fn parse_java_field(node: Node, bytes: &[u8]) -> Vec<FieldSummary> {
    // field_declaration:
    //   modifiers? -> type(integral_type|type_identifier|...) -> variable_declarator+
    let mut type_text: Option<&str> = None;
    let mut access_flags = ACC_PUBLIC;
    let mut names: Vec<&str> = Vec::new();

    let mut walker = node.walk();
    for child in node.children(&mut walker) {
        match child.kind() {
            "modifiers" => {
                access_flags = parse_java_modifiers(node_text(child, bytes));
            }
            "void_type"
            | "integral_type"
            | "floating_point_type"
            | "boolean_type"
            | "type_identifier"
            | "array_type"
            | "generic_type"
            | "scoped_type_identifier" => {
                if type_text.is_none() {
                    type_text = Some(node_text(child, bytes));
                }
            }
            "variable_declarator" => {
                // variable_declarator: identifier ("=" expression)?
                let mut vc = child.walk();
                for vchild in child.children(&mut vc) {
                    if vchild.kind() == "identifier" {
                        names.push(node_text(vchild, bytes));
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    let ty = type_text.unwrap_or("Object");
    names
        .into_iter()
        .map(|name| {
            let descriptor = java_type_to_descriptor(ty);
            FieldSummary {
                name: Arc::from(name),
                descriptor: Arc::from(descriptor.as_str()),
                access_flags,
                is_synthetic: false,
            }
        })
        .collect()
}

/// parse access flags from modifiers
fn parse_java_modifiers(text: &str) -> u16 {
    let mut flags: u16 = 0;
    if text.contains("public") {
        flags |= ACC_PUBLIC;
    }
    if text.contains("private") {
        flags |= ACC_PRIVATE;
    }
    if text.contains("protected") {
        flags |= ACC_PROTECTED;
    }
    if text.contains("static") {
        flags |= ACC_STATIC;
    }
    if text.contains("final") {
        flags |= ACC_FINAL;
    }
    if text.contains("abstract") {
        flags |= ACC_ABSTRACT;
    }
    // package-private，flags = 0
    flags
}

fn extract_java_package(root: Node, bytes: &[u8]) -> Option<Arc<str>> {
    let q_src = r#"(package_declaration (scoped_identifier) @pkg)"#;
    let q = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src).ok()?;
    let idx = q.capture_index_for_name("pkg")?;
    let results = run_query(&q, root, bytes, None);
    let pkg_text = results
        .first()
        .and_then(|caps| capture_text(caps, idx, bytes))?;
    // "com.example.foo" → "com/example/foo"
    Some(Arc::from(pkg_text.replace('.', "/").as_str()))
}

fn extract_java_access_flags(node: Node, bytes: &[u8]) -> u16 {
    let mut flags: u16 = 0;
    let mut walker = node.walk();
    for child in node.children(&mut walker) {
        if child.kind() == "modifiers" {
            flags = parse_java_modifiers(node_text(child, bytes));
            break;
        }
    }
    flags
}

pub fn build_java_descriptor(params_text: &str, ret_type: &str) -> String {
    let inner = params_text
        .trim()
        .trim_start_matches('(')
        .trim_end_matches(')');

    let mut desc = String::from("(");

    if !inner.trim().is_empty() {
        for param in split_params(inner) {
            let param = param.trim();
            // Extract the type part (remove the variable name; the last token is the variable name)
            let type_str = param
                .split_whitespace()
                .rev()
                .nth(1)
                .unwrap_or("")
                // Remove array parentheses
                .trim_end_matches(']')
                .trim();
            // Process arrays (may be "int[]" or "int []")
            let array_depth = param.chars().filter(|&c| c == '[').count();
            for _ in 0..array_depth {
                desc.push('[');
            }
            desc.push_str(&java_source_type_to_descriptor(type_str));
        }
    }

    desc.push(')');
    desc.push_str(&java_source_type_to_descriptor(ret_type.trim()));
    desc
}

/// Parameters are separated by commas, ignoring commas within generic angle brackets.
fn split_params(s: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '<' => depth += 1,
            '>' => depth -= 1,
            ',' if depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    if start < s.len() {
        result.push(&s[start..]);
    }
    result
}

/// Java source code type name → JVM descriptor
fn java_source_type_to_descriptor(ty: &str) -> String {
    // Remove generics
    let base = ty.split('<').next().unwrap_or(ty).trim();
    match base {
        "void" => "V".into(),
        "boolean" => "Z".into(),
        "byte" => "B".into(),
        "char" => "C".into(),
        "short" => "S".into(),
        "int" => "I".into(),
        "long" => "J".into(),
        "float" => "F".into(),
        "double" => "D".into(),
        other => {
            let internal = other.replace('.', "/");
            format!("L{};", internal)
        }
    }
}

/// Java source code type name → JVM descriptor (used for fields)
pub fn java_type_to_descriptor(ty: &str) -> String {
    let base = ty.split('<').next().unwrap_or(ty).trim();
    let array_depth = ty.chars().filter(|&c| c == '[').count();
    let brackets: String = (0..array_depth).map(|_| '[').collect();
    format!("{}{}", brackets, java_source_type_to_descriptor(base))
}

pub fn parse_kotlin_source(source: &str, origin: ClassOrigin) -> Vec<ClassMetadata> {
    let mut parser = make_kotlin_parser();
    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return vec![],
    };
    let root = tree.root_node();
    let bytes = source.as_bytes();

    let package = extract_kotlin_package(root, bytes);
    let mut results = Vec::new();
    collect_kotlin_classes(root, bytes, &package, None, &origin, &mut results);
    results
}

fn collect_kotlin_classes(
    node: Node,
    bytes: &[u8],
    package: &Option<Arc<str>>,
    outer_class: Option<Arc<str>>,
    origin: &ClassOrigin,
    out: &mut Vec<ClassMetadata>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "class_declaration" | "object_declaration" | "companion_object" => {
                if let Some(meta) =
                    parse_kotlin_class(child, bytes, package, outer_class.clone(), origin)
                {
                    let inner_outer = Some(Arc::clone(&meta.name));
                    // Recursively process inner classes within class_body
                    if let Some(body) = child
                        .named_children(&mut child.walk())
                        .find(|n| n.kind() == "class_body")
                    {
                        collect_kotlin_classes(body, bytes, package, inner_outer, origin, out);
                    }
                    out.push(meta);
                }
            }
            _ => {
                collect_kotlin_classes(child, bytes, package, outer_class.clone(), origin, out);
            }
        }
    }
}

fn parse_kotlin_class(
    node: Node,
    bytes: &[u8],
    package: &Option<Arc<str>>,
    outer_class: Option<Arc<str>>,
    origin: &ClassOrigin,
) -> Option<ClassMetadata> {
    // The class name for class_declaration / object_declaration is type_identifier.
    let name_node = node
        .children(&mut node.walk())
        .find(|n| n.kind() == "type_identifier")?;

    let class_name = node_text(name_node, bytes);
    if class_name.is_empty() {
        return None;
    }

    let name: Arc<str> = Arc::from(class_name);
    let internal_name: Arc<str> = match package {
        Some(pkg) => Arc::from(format!("{}/{}", pkg, class_name).as_str()),
        None => Arc::clone(&name),
    };

    // Parent class: the first type_identifier in delegation_specifiers
    let super_name = node
        .named_children(&mut node.walk())
        .find(|n| n.kind() == "delegation_specifiers")
        .and_then(|ds| {
            ds.named_children(&mut ds.walk())
                .find(|n| n.kind() == "constructor_invocation" || n.kind() == "user_type")
                .map(|n| node_text(n, bytes).to_string())
        });

    // Interface: entries of type user_type in delegation_specifiers (not the first constructor_invocation)
    let interfaces: Vec<String> = node
        .named_children(&mut node.walk())
        .find(|n| n.kind() == "delegation_specifiers")
        .map(|ds| {
            ds.named_children(&mut ds.walk())
                .filter(|n| n.kind() == "user_type")
                .map(|n| node_text(n, bytes).to_string())
                .collect()
        })
        .unwrap_or_default();

    let body = node
        .named_children(&mut node.walk())
        .find(|n| n.kind() == "class_body");

    let (methods, fields) = body
        .map(|b| {
            let methods = extract_kotlin_methods(b, bytes);
            let fields = extract_kotlin_fields(b, bytes);
            (methods, fields)
        })
        .unwrap_or_default();

    let access_flags = extract_kotlin_access_flags(node, bytes);

    Some(ClassMetadata {
        package: package.clone(),
        name,
        internal_name,
        super_name,
        interfaces,
        methods,
        fields,
        access_flags,
        inner_class_of: outer_class,
        origin: origin.clone(),
    })
}

fn extract_kotlin_methods(body: Node, bytes: &[u8]) -> Vec<MethodSummary> {
    // AST: function_declaration
    //   simple_identifier @name
    //   function_value_parameters @params
    //   (type_reference / user_type)? @return_type
    let q_src = r#"
        (function_declaration
            (simple_identifier) @name
            (function_value_parameters) @params)
    "#;
    let q = match Query::new(&tree_sitter_kotlin::LANGUAGE.into(), q_src) {
        Ok(q) => q,
        Err(_) => return vec![],
    };
    let name_idx = q.capture_index_for_name("name").unwrap();
    let params_idx = q.capture_index_for_name("params").unwrap();

    let mut result = Vec::new();

    for caps in run_query(&q, body, bytes, None) {
        let name = match capture_text(&caps, name_idx, bytes) {
            Some(n) => n,
            None => continue,
        };
        let params_text = capture_text(&caps, params_idx, bytes).unwrap_or("()");

        // Get the corresponding node from captures, then find its type_reference
        // Use the node position to find the function declaration node and then get the return type
        let descriptor = build_kotlin_descriptor(params_text);
        let return_type = parse_return_type_from_descriptor(&descriptor);

        result.push(MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor.as_str()),
            access_flags: ACC_PUBLIC,
            is_synthetic: false,
            generic_signature: None,
            return_type,
        });
    }
    result
}

fn extract_kotlin_fields(body: Node, bytes: &[u8]) -> Vec<FieldSummary> {
    // property_declaration
    //   (binding_pattern_kind val/var)
    //   variable_declaration
    //     simple_identifier @name
    //     user_type > type_identifier @type
    let q_src = r#"
        (property_declaration
            (variable_declaration
                (simple_identifier) @name
                (user_type (type_identifier) @type)))
    "#;
    let q = match Query::new(&tree_sitter_kotlin::LANGUAGE.into(), q_src) {
        Ok(q) => q,
        Err(_) => return vec![],
    };
    let name_idx = q.capture_index_for_name("name").unwrap();
    let type_idx = q.capture_index_for_name("type").unwrap();

    run_query(&q, body, bytes, None)
        .into_iter()
        .filter_map(|caps| {
            let name = capture_text(&caps, name_idx, bytes)?;
            let ty = capture_text(&caps, type_idx, bytes)?;
            let descriptor = kotlin_type_to_descriptor(ty);
            Some(FieldSummary {
                name: Arc::from(name),
                descriptor: Arc::from(descriptor.as_str()),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
            })
        })
        .collect()
}

fn extract_kotlin_package(root: Node, bytes: &[u8]) -> Option<Arc<str>> {
    // package_header > identifier
    let q_src = r#"(package_header (identifier) @pkg)"#;
    let q = Query::new(&tree_sitter_kotlin::LANGUAGE.into(), q_src).ok()?;
    let idx = q.capture_index_for_name("pkg")?;
    let results = run_query(&q, root, bytes, None);
    let pkg = results
        .first()
        .and_then(|caps| capture_text(caps, idx, bytes))?;
    Some(Arc::from(pkg.replace('.', "/").as_str()))
}

fn extract_kotlin_access_flags(node: Node, bytes: &[u8]) -> u16 {
    let mut flags: u16 = 0;
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "modifiers" {
            let text = node_text(child, bytes);
            if text.contains("public") {
                flags |= ACC_PUBLIC;
            }
            if text.contains("private") {
                flags |= ACC_PRIVATE;
            }
            if text.contains("protected") {
                flags |= ACC_PROTECTED;
            }
            if text.contains("open") { /* Kotlin open = not final */ }
            if text.contains("abstract") {
                flags |= ACC_ABSTRACT;
            }
            if text.contains("data") { /* data classes */ }
        }
    }
    // Kotlin 默认 public
    if flags & (ACC_PUBLIC | ACC_PRIVATE | ACC_PROTECTED) == 0 {
        flags |= ACC_PUBLIC;
    }
    flags
}

/// Kotlin parameter list text → JVM descriptor (approximately, use V when there is no return type information)
fn build_kotlin_descriptor(params_text: &str) -> String {
    let inner = params_text
        .trim()
        .trim_start_matches('(')
        .trim_end_matches(')');

    let mut desc = String::from("(");

    if !inner.trim().is_empty() {
        for param in split_params(inner) {
            let param = param.trim();
            let ty = param
                .split_once(':')
                .map(|x| x.1)
                .unwrap_or("")
                .trim()
                // Remove generics
                .split('<')
                .next()
                .unwrap_or("")
                .trim()
                // 去掉可空标记
                .trim_end_matches('?');
            desc.push_str(&kotlin_type_to_descriptor(ty));
        }
    }

    desc.push_str(")V"); // TODO: 返回类型暂用 V，实际需要 tree-sitter 进一步解析
    desc
}

/// Kotlin type name -> JVM descriptor
fn kotlin_type_to_descriptor(ty: &str) -> String {
    let ty = ty.trim().trim_end_matches('?');
    let base = ty.split('<').next().unwrap_or(ty).trim();
    match base {
        "Unit" => "V".into(),
        "Boolean" => "Z".into(),
        "Byte" => "B".into(),
        "Char" => "C".into(),
        "Short" => "S".into(),
        "Int" => "I".into(),
        "Long" => "J".into(),
        "Float" => "F".into(),
        "Double" => "D".into(),
        "String" => "Ljava/lang/String;".into(),
        "Any" => "Ljava/lang/Object;".into(),
        "List" | "MutableList" => "Ljava/util/List;".into(),
        "Map" | "MutableMap" => "Ljava/util/Map;".into(),
        "Set" | "MutableSet" => "Ljava/util/Set;".into(),
        "Array" => "[Ljava/lang/Object;".into(),
        other => format!("L{};", other.replace('.', "/")),
    }
}

fn node_text<'a>(node: Node, bytes: &'a [u8]) -> &'a str {
    node.utf8_text(bytes).unwrap_or("")
}

fn make_java_parser() -> Parser {
    let mut p = Parser::new();
    p.set_language(&tree_sitter_java::LANGUAGE.into())
        .expect("java grammar");
    p
}

fn make_kotlin_parser() -> Parser {
    let mut p = Parser::new();
    p.set_language(&tree_sitter_kotlin::LANGUAGE.into())
        .expect("kotlin grammar");
    p
}

#[cfg(test)]
mod tests {
    use crate::index::ClassOrigin;
    use crate::index::source::parse_java_source;

    #[test]
    fn test_nested_class_internal_name_with_package() {
        let src = r#"
package org.cubewhy.a;
public class Main {
    public static class NestedClass {
        public void randomFunction(String arg1) {}
    }
}
"#;
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let nested = classes
            .iter()
            .find(|c| c.name.as_ref() == "NestedClass")
            .unwrap();
        assert_eq!(
            nested.internal_name.as_ref(),
            "org/cubewhy/a/Main$NestedClass",
            "nested class internal name should use $ separator"
        );
        assert_eq!(nested.inner_class_of.as_deref(), Some("Main"));
    }

    #[test]
    fn test_nested_class_internal_name_without_package() {
        let src = r#"
public class Main {
    public static class NestedClass {
        public void randomFunction() {}
    }
}
"#;
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let nested = classes
            .iter()
            .find(|c| c.name.as_ref() == "NestedClass")
            .unwrap();
        assert_eq!(nested.internal_name.as_ref(), "Main$NestedClass");
    }

    #[test]
    fn test_nested_class_methods_indexed() {
        let src = r#"
package org.cubewhy.a;
public class Main {
    public static class NestedClass {
        public void randomFunction(String arg1) {}
    }
}
"#;
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let nested = classes
            .iter()
            .find(|c| c.name.as_ref() == "NestedClass")
            .unwrap();
        assert!(
            nested
                .methods
                .iter()
                .any(|m| m.name.as_ref() == "randomFunction"),
            "randomFunction should be indexed"
        );
    }
}
