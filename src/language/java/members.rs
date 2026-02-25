use std::sync::Arc;

use tree_sitter::Node;

use crate::{completion::context::CurrentClassMember, language::java::JavaContextExtractor};

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
                        } else if bc.kind() == "local_variable_declaration" {
                            // Check if next sibling is ERROR starting with `(` —
                            // this means tree-sitter misread a method declaration as a variable declaration
                            let next = block_children.get(i + 1);
                            if let Some(next_node) = next
                                && next_node.kind() == "ERROR"
                                && ctx.source[next_node.start_byte()..next_node.end_byte()]
                                    .trim_start()
                                    .starts_with('(')
                                && let Some(m) = parse_misread_method(ctx, bc, *next_node)
                            {
                                members.push(m);
                                i += 1; // skip the ERROR node too
                            }
                        }
                        i += 1;
                    }
                }
            }
            "field_declaration" => {
                members.extend(parse_field_node(ctx, child));
            }
            "ERROR" => {
                collect_members_from_node(ctx, child, members);
                let snapshot = members.clone();
                members.extend(parse_partial_methods_from_error(ctx, child, &snapshot));
            }
            _ => {}
        }
    }

    // If the node itself is ERROR (top-level), also parse its scattered children
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
    let found_names: std::collections::HashSet<&str> =
        already_found.iter().map(|m| m.name.as_ref()).collect();

    let mut cursor = error_node.walk();
    let children: Vec<Node> = error_node.children(&mut cursor).collect();
    let mut result = Vec::new();

    for (param_pos, _) in children
        .iter()
        .enumerate()
        .filter(|(_, n)| n.kind() == "formal_parameters")
    {
        // method name: nearest identifier before formal_parameters
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
        let modifiers_node = children[..param_pos]
            .iter()
            .rev()
            .find(|n| n.kind() == "modifiers");
        let is_static = modifiers_node
            .map(|n| ctx.node_text(*n).contains("static"))
            .unwrap_or(false);
        let is_private = modifiers_node
            .map(|n| ctx.node_text(*n).contains("private"))
            .unwrap_or(false);
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
        let params = ctx.node_text(children[param_pos]);
        result.push(CurrentClassMember {
            name: Arc::from(name),
            is_method: true,
            is_static,
            is_private,
            descriptor: Arc::from(
                crate::index::source::build_java_descriptor(params, ret_type).as_str(),
            ),
        });
    }
    result
}

pub fn parse_method_node(ctx: &JavaContextExtractor, node: Node) -> Option<CurrentClassMember> {
    let mut name: Option<&str> = None;
    let mut is_static = false;
    let mut is_private = false;
    let mut ret_type = "void";
    let mut params = "()";
    let mut wc = node.walk();
    for c in node.children(&mut wc) {
        match c.kind() {
            "modifiers" => {
                let t = ctx.node_text(c);
                is_static = t.contains("static");
                is_private = t.contains("private");
            }
            "identifier" if name.is_none() => {
                name = Some(ctx.node_text(c));
            }
            "void_type"
            | "integral_type"
            | "floating_point_type"
            | "boolean_type"
            | "type_identifier"
            | "array_type"
            | "generic_type" => {
                ret_type = ctx.node_text(c);
            }
            "formal_parameters" => {
                params = ctx.node_text(c);
            }
            _ => {}
        }
    }
    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>")?;
    Some(CurrentClassMember {
        name: Arc::from(name),
        is_method: true,
        is_static,
        is_private,
        descriptor: Arc::from(
            crate::index::source::build_java_descriptor(params, ret_type).as_str(),
        ),
    })
}

fn parse_field_node(ctx: &JavaContextExtractor, node: Node) -> Vec<CurrentClassMember> {
    let mut is_static = false;
    let mut is_private = false;
    let mut field_type = "Object";
    let mut names = Vec::new();
    let mut wc = node.walk();
    for c in node.children(&mut wc) {
        match c.kind() {
            "modifiers" => {
                let t = ctx.node_text(c);
                is_static = t.contains("static");
                is_private = t.contains("private");
            }
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
                        names.push(ctx.node_text(vchild).to_string());
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    names
        .into_iter()
        .map(|name| CurrentClassMember {
            name: Arc::from(name.as_str()),
            is_method: false,
            is_static,
            is_private,
            descriptor: Arc::from(
                crate::index::source::java_type_to_descriptor(field_type).as_str(),
            ),
        })
        .collect()
}

/// tree-sitter sometimes parses `private static Object test() {` as
/// local_variable_declaration("private static Object test") + ERROR("() {")
/// This method reconstructs the method member from these two nodes.
fn parse_misread_method(
    ctx: &JavaContextExtractor,
    decl_node: Node,
    error_node: Node,
) -> Option<CurrentClassMember> {
    // Extract modifiers, type, name from local_variable_declaration
    let mut is_static = false;
    let mut is_private = false;
    let mut ret_type = "void";
    let mut name: Option<&str> = None;

    let mut wc = decl_node.walk();
    for c in decl_node.named_children(&mut wc) {
        match c.kind() {
            "modifiers" => {
                let t = ctx.node_text(c);
                is_static = t.contains("static");
                is_private = t.contains("private");
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
                // name is the identifier inside variable_declarator
                let mut vc = c.walk();
                for vchild in c.children(&mut vc) {
                    if vchild.kind() == "identifier" {
                        name = Some(ctx.node_text(vchild));
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>")?;

    // Extract formal_parameters from the ERROR node
    let mut ec = error_node.walk();
    let params = error_node
        .children(&mut ec)
        .find(|c| c.kind() == "formal_parameters")
        .map(|c| ctx.node_text(c))
        .unwrap_or("()");

    Some(CurrentClassMember {
        name: Arc::from(name),
        is_method: true,
        is_static,
        is_private,
        descriptor: Arc::from(
            crate::index::source::build_java_descriptor(params, ret_type).as_str(),
        ),
    })
}
