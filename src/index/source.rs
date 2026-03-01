use rust_asm::constants::{ACC_ABSTRACT, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC};
use std::sync::Arc;
use tracing::debug;
use tree_sitter::{Node, Parser, Query};

use super::{
    ClassMetadata, ClassOrigin, FieldSummary, MethodSummary, parse_return_type_from_descriptor,
};
use crate::{
    completion::{context::CurrentClassMember, type_resolver::java_source_type_to_descriptor},
    index::intern_str,
    language::{
        java::{
            JavaContextExtractor, members::extract_class_members_from_body, scope::extract_package,
            utils::parse_java_modifiers,
        },
        ts_utils::{capture_text, run_query},
    },
};

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
    let ctx = JavaContextExtractor::for_indexing(source);
    let mut parser = make_java_parser();
    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return vec![],
    };
    let root = tree.root_node();

    let package = extract_package(&ctx, root);
    let mut results = Vec::new();
    collect_java_classes(&ctx, root, &package, None, &origin, &mut results);

    results
}

fn collect_java_classes(
    ctx: &JavaContextExtractor,
    node: Node,
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
                    parse_java_class(ctx, child, package, outer_class.clone(), origin)
                {
                    // Recursive processing of inner classes
                    let inner_outer = Some(Arc::clone(&meta.name));
                    if let Some(body) = child.child_by_field_name("body") {
                        collect_java_classes(ctx, body, package, inner_outer, origin, out);
                    }
                    out.push(meta);
                }
            }
            _ => {
                // Continue searching downwards (e.g., inner classes within class_body)
                collect_java_classes(ctx, child, package, outer_class.clone(), origin, out);
            }
        }
    }
}

fn parse_java_class(
    ctx: &JavaContextExtractor,
    node: Node,
    package: &Option<Arc<str>>,
    outer_class: Option<Arc<str>>,
    origin: &ClassOrigin,
) -> Option<ClassMetadata> {
    let name_node = node.child_by_field_name("name")?;
    let class_name = ctx.node_text(name_node);
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
        .and_then(|superclass_node| {
            superclass_node
                .named_children(&mut superclass_node.walk())
                .find(|c| c.kind() == "type_identifier")
                .map(|c| intern_str(ctx.node_text(c)))
        });

    // interfaces
    let interfaces: Vec<Arc<str>> = node
        .child_by_field_name("interfaces")
        .map(|iface_node| {
            let q_src = r#"(type_identifier) @t"#;
            if let Ok(q) = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src) {
                let idx = q.capture_index_for_name("t").unwrap();
                run_query(&q, iface_node, ctx.bytes(), None)
                    .into_iter()
                    .filter_map(|caps| capture_text(&caps, idx, ctx.bytes()).map(intern_str))
                    .collect()
            } else {
                vec![]
            }
        })
        .unwrap_or_default();

    // methods & fields
    let mut methods = Vec::new();
    let mut fields = Vec::new();

    let body = node.child_by_field_name("body");
    let full_source = std::str::from_utf8(ctx.bytes()).unwrap_or("");
    let ctx = JavaContextExtractor::for_indexing(full_source);
    if let Some(b) = body {
        for member in extract_class_members_from_body(&ctx, b) {
            match member {
                CurrentClassMember::Method(m) => methods.push((*m).clone()),
                CurrentClassMember::Field(f) => fields.push((*f).clone()),
            }
        }
    }
    let access_flags = extract_java_access_flags(&ctx, node);

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
        generic_signature: extract_generic_signature(node, ctx.bytes(), "Ljava/lang/Object;"),
        origin: origin.clone(),
    })
}

fn extract_java_access_flags(ctx: &JavaContextExtractor, node: Node) -> u16 {
    let mut flags: u16 = ACC_PUBLIC;
    let mut walker = node.walk();
    for child in node.children(&mut walker) {
        if child.kind() == "modifiers" {
            flags = parse_java_modifiers(ctx.node_text(child));
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
                .map(|n| intern_str(node_text(n, bytes)))
        });

    // Interface: entries of type user_type in delegation_specifiers (not the first constructor_invocation)
    let interfaces: Vec<Arc<str>> = node
        .named_children(&mut node.walk())
        .find(|n| n.kind() == "delegation_specifiers")
        .map(|ds| {
            ds.named_children(&mut ds.walk())
                .filter(|n| n.kind() == "user_type")
                .map(|n| intern_str(node_text(n, bytes)))
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
        generic_signature: extract_generic_signature(node, bytes, "Ljava/lang/Object;"),
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
        let name_node = match caps.iter().find(|(idx, _)| *idx == name_idx) {
            Some(n) => n.1,
            None => continue,
        };
        let name = node_text(name_node, bytes);
        let params_text = capture_text(&caps, params_idx, bytes).unwrap_or("()");

        // Get the corresponding node from captures, then find its type_reference
        // Use the node position to find the function declaration node and then get the return type
        let descriptor = build_kotlin_descriptor(params_text);
        let return_type = parse_return_type_from_descriptor(&descriptor);

        let func_node = name_node.parent().unwrap();
        let generic_signature = extract_generic_signature(func_node, bytes, &descriptor);
        let param_names: Vec<Arc<str>> = {
            let params_node = caps
                .iter()
                .find(|(idx, _)| *idx == params_idx)
                .map(|(_, n)| *n);
            if let Some(pn) = params_node {
                pn.named_children(&mut pn.walk())
                    .filter(|n| n.kind() == "parameter" || n.kind() == "function_value_parameter")
                    .filter_map(|n| {
                        n.children(&mut n.walk())
                            .find(|c| c.kind() == "simple_identifier")
                            .map(|c| Arc::from(node_text(c, bytes)))
                    })
                    .collect()
            } else {
                vec![]
            }
        };

        result.push(MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor.as_str()),
            param_names,
            access_flags: ACC_PUBLIC,
            is_synthetic: false,
            generic_signature,
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
                generic_signature: None,
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

/// 提取类或方法的泛型参数，并构建成 JVM 规范的泛型签名。
/// 例如从 `class List<T, E>` 提取出 `<T:Ljava/lang/Object;E:Ljava/lang/Object;>Ljava/lang/Object;`
fn extract_generic_signature(node: Node, bytes: &[u8], suffix: &str) -> Option<Arc<str>> {
    // 兼容 Java (child_by_field_name) 和 Kotlin (直接找 kind)
    let tp_node = node.child_by_field_name("type_parameters").or_else(|| {
        node.children(&mut node.walk())
            .find(|n| n.kind() == "type_parameters")
    })?;

    let mut sig = String::from("<");
    let mut has_params = false;
    let mut walker = tp_node.walk();

    for child in tp_node.named_children(&mut walker) {
        if child.kind() == "type_parameter" {
            // Java 是 identifier，Kotlin 是 type_identifier
            if let Some(id_node) = child
                .children(&mut child.walk())
                .find(|c| c.kind() == "identifier" || c.kind() == "type_identifier")
            {
                let name = node_text(id_node, bytes).trim();
                if !name.is_empty() {
                    sig.push_str(name);
                    // 统一擦除到 Object，因为我们的引擎目前只关心参数占位符的名称映射
                    sig.push_str(":Ljava/lang/Object;");
                    has_params = true;
                }
            }
        }
    }

    if !has_params {
        return None;
    }

    sig.push('>');
    sig.push_str(suffix);
    Some(Arc::from(sig))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::index::ClassOrigin;
    use crate::index::source::{parse_java_source, parse_kotlin_source};

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

    #[test]
    fn test_super_name_no_extends_keyword() {
        let src = "public class Child extends Parent implements Runnable, Serializable {}";
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let child = classes.iter().find(|c| c.name.as_ref() == "Child").unwrap();
        assert_eq!(
            child.super_name.as_deref(),
            Some("Parent"),
            "super_name should be 'Parent', not 'extends Parent'"
        );
        assert!(
            child.interfaces.contains(&"Runnable".into()),
            "interfaces should contain Runnable"
        );
        assert!(
            child.interfaces.contains(&"Serializable".into()),
            "interfaces should contain Serializable"
        );
    }

    #[test]
    fn test_super_name_strips_extends_keyword() {
        let src = "public class Child extends Parent implements Runnable {}";
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let child = classes.iter().find(|c| c.name.as_ref() == "Child").unwrap();
        assert_eq!(
            child.super_name.as_deref(),
            Some("Parent"),
            "super_name should be 'Parent' not 'extends Parent', got {:?}",
            child.super_name
        );
        assert!(child.interfaces.contains(&"Runnable".into()));
    }

    #[test]
    fn test_extract_java_class_generic_signature() {
        let src = "public class MyMap<K, V> { }";
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let meta = classes.first().unwrap();

        assert_eq!(
            meta.generic_signature.as_deref(),
            Some("<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/lang/Object;")
        );
    }

    #[test]
    fn test_extract_java_method_generic_signature() {
        let src = "public class Utils { public <T> T getFirst(List<T> list) { return null; } }";
        let classes = parse_java_source(src, ClassOrigin::Unknown);
        let method = classes.first().unwrap().methods.first().unwrap();

        // 验证方法上的泛型 T 被正确抓取，并且携带了后续的 descriptor
        let sig = method.generic_signature.as_deref().unwrap();
        assert!(sig.starts_with("<T:Ljava/lang/Object;>"));
    }

    #[test]
    fn test_extract_kotlin_class_generic_signature() {
        let src = "class Box<T>(val item: T) { }";
        let classes = parse_kotlin_source(src, ClassOrigin::Unknown);
        let meta = classes.first().unwrap();

        assert_eq!(
            meta.generic_signature.as_deref(),
            Some("<T:Ljava/lang/Object;>Ljava/lang/Object;")
        );
    }

    #[test]
    fn test_kotlin_param_names() {
        let src = "class Foo { fun greet(name: String, times: Int) {} }";
        let classes = parse_kotlin_source(src, ClassOrigin::Unknown);
        let method = classes[0]
            .methods
            .iter()
            .find(|m| m.name.as_ref() == "greet")
            .unwrap();
        assert_eq!(
            method.param_names.as_slice(),
            &[Arc::from("name"), Arc::from("times")]
        );
    }
}
