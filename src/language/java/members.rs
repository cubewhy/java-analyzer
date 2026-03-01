use rust_asm::constants::{ACC_PRIVATE, ACC_PUBLIC, ACC_STATIC};
use std::sync::Arc;
use tree_sitter::Node;

use crate::{
    completion::{
        context::CurrentClassMember,
        type_resolver::{java_type_to_descriptor, parse_return_type_from_descriptor},
    },
    index::{FieldSummary, MethodSummary},
    language::java::{
        JavaContextExtractor,
        utils::{extract_generic_signature, parse_java_modifiers},
    },
};

/// Extract the list of parameter names from the formal_parameters node
fn parse_param_names(ctx: &JavaContextExtractor, node: Node) -> Vec<Arc<str>> {
    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        // Handling regular and variable-length parameters (spread_parameter)
        if matches!(child.kind(), "formal_parameter" | "spread_parameter")
            && let Some(id_node) = child.child_by_field_name("name")
        {
            names.push(Arc::from(ctx.node_text(id_node)));
        }
    }
    names
}

#[rustfmt::skip]
pub fn is_java_keyword(name: &str) -> bool {
    matches!(
        name,
        "public" | "private" | "protected" | "static" | "final" | "abstract"
            | "synchronized" | "volatile" | "transient" | "native" | "strictfp"
            | "void" | "int" | "long" | "double" | "float" | "boolean"
            | "byte" | "short" | "char"
            | "class" | "interface" | "enum" | "extends" | "implements"
            | "return" | "new" | "this" | "super" | "null" | "true" | "false"
            | "if" | "else" | "for" | "while" | "do" | "switch" | "case"
            | "break" | "continue" | "default" | "try" | "catch" | "finally"
            | "throw" | "throws" | "import" | "package" | "instanceof" | "assert"
    )
}

pub fn extract_class_members_from_body(
    ctx: &JavaContextExtractor,
    body: Node,
) -> Vec<CurrentClassMember> {
    let mut members = Vec::new();
    collect_members_from_node(ctx, body, &mut members);
    members
}

pub fn collect_members_from_node(
    ctx: &JavaContextExtractor,
    node: Node,
    members: &mut Vec<CurrentClassMember>,
) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "method_declaration" => {
                if let Some(m) = parse_method_node(ctx, child) {
                    members.push(m);
                }
                if let Some(block) = child.child_by_field_name("body") {
                    let mut bc = block.walk();
                    let block_children: Vec<Node> = block.children(&mut bc).collect();
                    let mut i = 0;
                    while i < block_children.len() {
                        let bc = block_children[i];
                        if bc.kind() == "ERROR" {
                            if let Some(m) = parse_method_node(ctx, bc) {
                                members.push(m);
                            }
                            members.extend(parse_field_node(ctx, bc));
                            collect_members_from_node(ctx, bc, members);
                            let snapshot = members.clone();
                            members.extend(parse_partial_methods_from_error(ctx, bc, &snapshot));
                        } else if bc.kind() == "local_variable_declaration" {
                            let next = block_children.get(i + 1);
                            if let Some(next_node) = next
                                && next_node.kind() == "ERROR"
                                && ctx.source[next_node.start_byte()..next_node.end_byte()]
                                    .trim_start()
                                    .starts_with('(')
                                && let Some(m) = parse_misread_method(ctx, bc, *next_node)
                            {
                                members.push(m);
                                i += 1;
                            }
                        }
                        i += 1;
                    }
                }
            }
            "field_declaration" => {
                members.extend(parse_field_node(ctx, child));
            }
            "class_declaration"
            | "interface_declaration"
            | "enum_declaration"
            | "class_body"
            | "interface_body"
            | "enum_body"
            | "program" => {
                collect_members_from_node(ctx, child, members);
            }
            "ERROR" => {
                collect_members_from_node(ctx, child, members);
                let snapshot = members.clone();
                members.extend(parse_partial_methods_from_error(ctx, child, &snapshot));
            }
            _ => {}
        }
    }

    if node.kind() == "ERROR" {
        let snapshot = members.clone();
        members.extend(parse_partial_methods_from_error(ctx, node, &snapshot));
    }
}

pub fn parse_partial_methods_from_error(
    ctx: &JavaContextExtractor,
    error_node: Node,
    already_found: &[CurrentClassMember],
) -> Vec<CurrentClassMember> {
    let found_names: std::collections::HashSet<Arc<str>> =
        already_found.iter().map(|m| m.name()).collect();

    let mut cursor = error_node.walk();
    let children: Vec<Node> = error_node.children(&mut cursor).collect();
    let mut result = Vec::new();

    for (param_pos, _) in children
        .iter()
        .enumerate()
        .filter(|(_, n)| n.kind() == "formal_parameters")
    {
        let params_node = children[param_pos];
        let name = match children[..param_pos]
            .iter()
            .rev()
            .find(|n| n.kind() == "identifier")
        {
            Some(n) => ctx.node_text(*n),
            None => continue,
        };
        if name == "<init>" || name == "<clinit>" || found_names.contains(name) {
            continue;
        }

        let mut flags = 0;
        if let Some(n) = children[..param_pos]
            .iter()
            .rev()
            .find(|n| n.kind() == "modifiers")
        {
            flags = parse_java_modifiers(ctx.node_text(*n));
        }
        if flags == 0 {
            flags = ACC_PUBLIC;
        }

        let ret_type = children[..param_pos]
            .iter()
            .rev()
            .find(|n| {
                matches!(
                    n.kind(),
                    "void_type"
                        | "integral_type"
                        | "floating_point_type"
                        | "boolean_type"
                        | "type_identifier"
                        | "array_type"
                        | "generic_type"
                )
            })
            .map(|n| ctx.node_text(*n))
            .unwrap_or("void");

        let descriptor =
            crate::index::source::build_java_descriptor(ctx.node_text(params_node), ret_type);

        result.push(CurrentClassMember::Method(Arc::new(MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor.as_str()),
            param_names: parse_param_names(ctx, params_node),
            access_flags: flags,
            is_synthetic: false,
            generic_signature: None,
            return_type: parse_return_type_from_descriptor(&descriptor),
        })));
    }

    for (mi_pos, mi_node) in children
        .iter()
        .enumerate()
        .filter(|(_, n)| n.kind() == "method_invocation")
    {
        let name = match mi_node.child_by_field_name("name") {
            Some(n) => ctx.node_text(n),
            None => continue,
        };
        if name == "<init>"
            || name == "<clinit>"
            || found_names.contains(name)
            || is_java_keyword(name)
        {
            continue;
        }

        let mut flags = 0;
        let mut ret_type = "void";

        for prev in children[..mi_pos].iter().rev() {
            match prev.kind() {
                "identifier" => {
                    flags |= parse_java_modifiers(ctx.node_text(*prev));
                }
                "void_type"
                | "integral_type"
                | "floating_point_type"
                | "boolean_type"
                | "type_identifier"
                | "array_type"
                | "generic_type" => {
                    if ret_type == "void" {
                        ret_type = ctx.node_text(*prev);
                    }
                }
                "ERROR" => {
                    let mut pc = prev.walk();
                    for pchild in prev.children(&mut pc) {
                        match pchild.kind() {
                            "identifier" | "static" | "private" | "public" | "protected" => {
                                flags |= parse_java_modifiers(ctx.node_text(pchild));
                            }
                            "void_type"
                            | "integral_type"
                            | "floating_point_type"
                            | "boolean_type"
                            | "type_identifier"
                            | "array_type"
                            | "generic_type" => {
                                if ret_type == "void" {
                                    ret_type = ctx.node_text(pchild);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        if flags == 0 {
            flags = ACC_PUBLIC;
        }

        let args = mi_node
            .child_by_field_name("arguments")
            .map(|n| ctx.node_text(n))
            .unwrap_or("()");
        let descriptor = crate::index::source::build_java_descriptor(args, ret_type);

        result.push(CurrentClassMember::Method(Arc::new(MethodSummary {
            name: Arc::from(name),
            descriptor: Arc::from(descriptor.as_str()),
            param_names: vec![],
            access_flags: flags,
            is_synthetic: false,
            generic_signature: None,
            return_type: parse_return_type_from_descriptor(&descriptor),
        })));
    }

    result
}

pub fn parse_method_node(ctx: &JavaContextExtractor, node: Node) -> Option<CurrentClassMember> {
    let mut name: Option<&str> = None;
    let mut flags = 0;
    let mut ret_type = "void";
    let mut params_node: Option<Node> = None;

    let mut wc = node.walk();
    for c in node.children(&mut wc) {
        match c.kind() {
            "modifiers" => flags = parse_java_modifiers(ctx.node_text(c)),
            "identifier" if name.is_none() => name = Some(ctx.node_text(c)),
            "void_type"
            | "integral_type"
            | "floating_point_type"
            | "boolean_type"
            | "type_identifier"
            | "array_type"
            | "generic_type" => {
                ret_type = ctx.node_text(c);
            }
            "formal_parameters" => params_node = Some(c),
            _ => {}
        }
    }
    if flags == 0 {
        flags = ACC_PUBLIC;
    }

    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>" && !is_java_keyword(n))?;
    let params_text = params_node.map(|n| ctx.node_text(n)).unwrap_or("()");
    let descriptor = crate::index::source::build_java_descriptor(params_text, ret_type);

    let generic_signature = extract_generic_signature(node, ctx.bytes(), &descriptor);

    Some(CurrentClassMember::Method(Arc::new(MethodSummary {
        name: Arc::from(name),
        descriptor: Arc::from(descriptor.as_str()),
        param_names: params_node
            .map(|n| parse_param_names(ctx, n))
            .unwrap_or_default(),
        access_flags: flags,
        is_synthetic: false,
        generic_signature,
        return_type: parse_return_type_from_descriptor(&descriptor),
    })))
}

fn parse_field_node(ctx: &JavaContextExtractor, node: Node) -> Vec<CurrentClassMember> {
    let mut flags = 0;
    let mut field_type = "Object";
    let mut names = Vec::new();
    let mut wc = node.walk();
    for c in node.children(&mut wc) {
        match c.kind() {
            "modifiers" => flags = parse_java_modifiers(ctx.node_text(c)),
            "void_type"
            | "integral_type"
            | "floating_point_type"
            | "boolean_type"
            | "type_identifier"
            | "array_type"
            | "generic_type" => {
                field_type = ctx.node_text(c);
            }
            "variable_declarator" => {
                let mut vc = c.walk();
                for vchild in c.children(&mut vc) {
                    if vchild.kind() == "identifier" {
                        let n = ctx.node_text(vchild);
                        if !is_java_keyword(n) {
                            names.push(n.to_string());
                        }
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    if flags == 0 {
        flags = ACC_PUBLIC;
    }

    names
        .into_iter()
        .map(|name| {
            let desc = java_type_to_descriptor(field_type);
            CurrentClassMember::Field(Arc::new(FieldSummary {
                name: Arc::from(name.as_str()),
                descriptor: Arc::from(desc.as_str()),
                access_flags: flags,
                is_synthetic: false,
                generic_signature: None,
            }))
        })
        .collect()
}

fn parse_misread_method(
    ctx: &JavaContextExtractor,
    decl_node: Node,
    error_node: Node,
) -> Option<CurrentClassMember> {
    let mut flags = ACC_PUBLIC;
    let mut ret_type = "void";
    let mut name: Option<&str> = None;

    let mut wc = decl_node.walk();
    for c in decl_node.named_children(&mut wc) {
        match c.kind() {
            "modifiers" => {
                let t = ctx.node_text(c);
                if t.contains("static") {
                    flags |= ACC_STATIC;
                }
                if t.contains("private") {
                    flags |= ACC_PRIVATE;
                }
            }
            "type_identifier"
            | "void_type"
            | "integral_type"
            | "floating_point_type"
            | "boolean_type"
            | "array_type"
            | "generic_type" => {
                ret_type = ctx.node_text(c);
            }
            "variable_declarator" => {
                if let Some(id_node) = c.child_by_field_name("name") {
                    name = Some(ctx.node_text(id_node));
                }
            }
            _ => {}
        }
    }

    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>" && !is_java_keyword(n))?;
    let mut ec = error_node.walk();
    let params_node = error_node
        .children(&mut ec)
        .find(|c| c.kind() == "formal_parameters");
    let descriptor = crate::index::source::build_java_descriptor(
        params_node.map(|n| ctx.node_text(n)).unwrap_or("()"),
        ret_type,
    );

    Some(CurrentClassMember::Method(Arc::new(MethodSummary {
        name: Arc::from(name),
        descriptor: Arc::from(descriptor.as_str()),
        param_names: params_node
            .map(|n| parse_param_names(ctx, n))
            .unwrap_or_default(),
        access_flags: flags,
        is_synthetic: false,
        generic_signature: None,
        return_type: parse_return_type_from_descriptor(&descriptor),
    })))
}

/// Walk backwards to find a block comment starting with `/**`
pub fn extract_javadoc(node: Node, bytes: &[u8]) -> Option<Arc<str>> {
    let mut prev = node.prev_sibling();
    while let Some(n) = prev {
        if n.kind() == "block_comment" {
            let text = n.utf8_text(bytes).unwrap_or("");
            if text.starts_with("/**") {
                return Some(Arc::from(text));
            }
            break; // Standard block comment, not javadoc
        } else if n.kind() == "line_comment" {
            prev = n.prev_sibling(); // Skip over normal comments
        } else {
            break; // Found code, stop looking
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn setup(source: &str) -> (JavaContextExtractor, tree_sitter::Tree) {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        let tree = parser.parse(source, None).unwrap();

        let ctx = JavaContextExtractor::new(source, source.len());
        (ctx, tree)
    }

    #[test]
    fn test_parse_standard_members() {
        let src = indoc::indoc! {r#"
        class A {
            public int a;
            private static String b;

            public void methodA() {}
            private static Object methodB(int p) { return null; }
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let a = members.iter().find(|m| m.name().as_ref() == "a").unwrap();
        assert!(!a.is_method() && !a.is_static() && !a.is_private());

        let b = members.iter().find(|m| m.name().as_ref() == "b").unwrap();
        assert!(!b.is_method() && b.is_static() && b.is_private());

        let ma = members
            .iter()
            .find(|m| m.name().as_ref() == "methodA")
            .unwrap();
        assert!(ma.is_method() && !ma.is_static() && !ma.is_private());

        let mb = members
            .iter()
            .find(|m| m.name().as_ref() == "methodB")
            .unwrap();
        assert!(mb.is_method() && mb.is_static() && mb.is_private());
    }

    #[test]
    fn test_ignore_constructors() {
        let src = indoc::indoc! {r#"
        class A {
            public A() {}
            static { }
            void normalMethod() {}
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        assert!(members.iter().any(|m| m.name().as_ref() == "normalMethod"));
        assert!(!members.iter().any(|m| m.name().as_ref() == "<init>"));
        assert!(!members.iter().any(|m| m.name().as_ref() == "<clinit>"));
        assert!(!members.iter().any(|m| m.name().as_ref() == "A"));
    }

    #[test]
    fn test_multiple_field_declarators() {
        let src = indoc::indoc! {r#"
        class A {
            private static int x, y;
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let x = members.iter().find(|m| m.name().as_ref() == "x").unwrap();
        assert!(!x.is_method() && x.is_static() && x.is_private());

        let y = members.iter().find(|m| m.name().as_ref() == "y").unwrap();
        assert!(!y.is_method() && y.is_static() && y.is_private());
    }

    #[test]
    fn test_swallowed_error_node_members() {
        let src = indoc::indoc! {r#"
        class A {
            void brokenMethod() {
                System.out.println("No closing brace"
            
            private static void swallowedMethod() {}
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let swallowed = members
            .iter()
            .find(|m| m.name().as_ref() == "swallowedMethod");
        assert!(swallowed.is_some());
        let swallowed = swallowed.unwrap();
        assert!(swallowed.is_method() && swallowed.is_static() && swallowed.is_private());
    }

    #[test]
    fn test_misread_method_as_local_variable() {
        let src = indoc::indoc! {r#"
        class A {
            void brokenMethod() {
                int x = 1 // missing semicolon
            
            private static String misreadMethod(int a) {}
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let misread = members
            .iter()
            .find(|m| m.name().as_ref() == "misreadMethod");
        assert!(misread.is_some());
        let misread = misread.unwrap();
        assert!(misread.is_method() && misread.is_static() && misread.is_private());
    }

    #[test]
    fn test_partial_methods_from_top_level_error() {
        let src = indoc::indoc! {r#"
        package org.example;
        
        public class A {
            void foo() {
                // missing braces mess up everything below
        
        public static void salvagedMethod(String arg) { }
        
        private Object anotherSalvaged() { return null; }
        "#};

        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let salvaged1 = members
            .iter()
            .find(|m| m.name().as_ref() == "salvagedMethod")
            .unwrap();
        assert!(salvaged1.is_method() && salvaged1.is_static() && !salvaged1.is_private());

        let salvaged2 = members
            .iter()
            .find(|m| m.name().as_ref() == "anotherSalvaged")
            .unwrap();
        assert!(salvaged2.is_method() && !salvaged2.is_static() && salvaged2.is_private());
    }
}
