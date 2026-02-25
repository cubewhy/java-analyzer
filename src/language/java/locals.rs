use std::sync::Arc;

use tree_sitter::{Node, Query};

use crate::{
    completion::LocalVar,
    language::{
        java::{
            JavaContextExtractor,
            utils::{
                find_ancestor, get_initializer_text, infer_type_from_initializer,
                java_type_to_internal,
            },
        },
        ts_utils::{capture_text, find_method_by_offset, run_query},
    },
};

pub(crate) fn extract_locals(
    ctx: &JavaContextExtractor,
    root: Node,
    cursor_node: Option<Node>,
) -> Vec<LocalVar> {
    let search_root = cursor_node
        .and_then(|n| find_ancestor(n, "method_declaration"))
        .or_else(|| find_method_by_offset(root, ctx.offset))
        .unwrap_or(root);
    let query_src = r#"
            (local_variable_declaration
                type: (_) @type
                declarator: (variable_declarator
                    name: (identifier) @name))
        "#;
    let q = match Query::new(&tree_sitter_java::LANGUAGE.into(), query_src) {
        Ok(q) => q,
        Err(e) => {
            tracing::debug!("local var query error: {}", e);
            return vec![];
        }
    };
    let type_idx = q.capture_index_for_name("type").unwrap();
    let name_idx = q.capture_index_for_name("name").unwrap();
    let mut vars: Vec<LocalVar> = run_query(&q, search_root, ctx.bytes, None)
        .into_iter()
        .filter_map(|captures| {
            let ty_node = captures.iter().find(|(idx, _)| *idx == type_idx)?.1;
            let name_node = captures.iter().find(|(idx, _)| *idx == name_idx)?.1;
            if ty_node.start_byte() >= ctx.offset {
                return None;
            }
            let ty = ty_node.utf8_text(ctx.bytes).ok()?;
            let name = name_node.utf8_text(ctx.bytes).ok()?;
            tracing::debug!(
                ty,
                name,
                start = ty_node.start_byte(),
                offset = ctx.offset,
                "extracted local var"
            );
            let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
            if raw_ty == "var" {
                return Some(match infer_type_from_initializer(ty_node, ctx.bytes) {
                    Some(t) => LocalVar {
                        name: Arc::from(name),
                        type_internal: Arc::from(java_type_to_internal(&t).as_str()),
                        init_expr: None,
                    },
                    None => LocalVar {
                        name: Arc::from(name),
                        type_internal: Arc::from("var"),
                        init_expr: get_initializer_text(ty_node, ctx.bytes),
                    },
                });
            }
            Some(LocalVar {
                name: Arc::from(name),
                type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
                init_expr: None,
            })
        })
        .collect();

    vars.extend(extract_misread_var_decls(ctx, search_root));

    vars.extend(extract_params(ctx, root, cursor_node));
    vars
}

fn extract_misread_var_decls(ctx: &JavaContextExtractor, root: Node) -> Vec<LocalVar> {
    let mut result = Vec::new();
    collect_misread_decls(ctx, root, &mut result);
    result
}

fn collect_misread_decls(ctx: &JavaContextExtractor, node: Node, vars: &mut Vec<LocalVar>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        // Pattern: variable_declarator containing assignment_expression
        // where the declarator's ERROR sibling contains a type keyword
        if child.kind() == "variable_declarator" {
            // Look for assignment_expression inside this declarator
            let mut vc = child.walk();
            for vchild in child.children(&mut vc) {
                if vchild.kind() == "assignment_expression" {
                    // Left side is the misread variable name
                    let lhs = vchild.child_by_field_name("left").or_else(|| {
                        let mut wc = vchild.walk();
                        vchild.named_children(&mut wc).next()
                    });
                    let rhs = vchild.child_by_field_name("right").or_else(|| {
                        let mut wc = vchild.walk();
                        vchild.named_children(&mut wc).nth(1)
                    });
                    if let (Some(name_node), Some(init_node)) = (lhs, rhs) {
                        if name_node.kind() != "identifier" {
                            continue;
                        }
                        if name_node.start_byte() >= ctx.offset {
                            continue;
                        }
                        let name = ctx.node_text(name_node);
                        // Find type from ERROR sibling (contains "var" or type_identifier)
                        let type_name = find_type_in_error_sibling(ctx, child);
                        let init_text = ctx.node_text(init_node).to_string();
                        let lv = if type_name.as_deref() == Some("var") {
                            LocalVar {
                                name: Arc::from(name),
                                type_internal: Arc::from("var"),
                                init_expr: Some(init_text),
                            }
                        } else {
                            let raw_ty = type_name.as_deref().unwrap_or("Object");
                            LocalVar {
                                name: Arc::from(name),
                                type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
                                init_expr: None,
                            }
                        };
                        vars.push(lv);
                    }
                }
            }
        }
        collect_misread_decls(ctx, child, vars);
    }
}

fn find_type_in_error_sibling(ctx: &JavaContextExtractor, declarator_node: Node) -> Option<String> {
    // Look inside ERROR children of the declarator for type_identifier
    let mut cursor = declarator_node.walk();
    for child in declarator_node.children(&mut cursor) {
        if child.kind() == "ERROR" {
            let mut ec = child.walk();
            for ec_child in child.children(&mut ec) {
                if ec_child.kind() == "type_identifier"
                    || ec_child.kind() == "integral_type"
                    || ec_child.kind() == "void_type"
                {
                    return Some(ctx.node_text(ec_child).to_string());
                }
            }
        }
    }
    None
}

fn extract_params(
    ctx: &JavaContextExtractor,
    root: Node,
    cursor_node: Option<Node>,
) -> Vec<LocalVar> {
    let method = match cursor_node
        .and_then(|n| find_ancestor(n, "method_declaration"))
        .or_else(|| find_method_by_offset(root, ctx.offset))
    {
        Some(m) => m,
        None => return vec![],
    };
    let query_src = r#"(formal_parameter type: (_) @type name: (identifier) @name)"#;
    let q = match Query::new(&tree_sitter_java::LANGUAGE.into(), query_src) {
        Ok(q) => q,
        Err(_) => return vec![],
    };
    let type_idx = q.capture_index_for_name("type").unwrap();
    let name_idx = q.capture_index_for_name("name").unwrap();
    run_query(&q, method, ctx.bytes, None)
        .into_iter()
        .filter_map(|captures| {
            let ty = capture_text(&captures, type_idx, ctx.bytes)?;
            let name = capture_text(&captures, name_idx, ctx.bytes)?;
            let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
            Some(LocalVar {
                name: Arc::from(name),
                type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
                init_expr: None,
            })
        })
        .collect()
}

fn extract_locals_from_error_nodes(ctx: &JavaContextExtractor, root: Node) -> Vec<LocalVar> {
    let mut result = Vec::new();
    collect_locals_in_errors(ctx, root, &mut result);
    result
}

fn collect_locals_in_errors(ctx: &JavaContextExtractor, node: Node, vars: &mut Vec<LocalVar>) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "ERROR" {
            // ERROR may contain local_variable_declaration
            let q_src = r#"
                (local_variable_declaration
                    type: (_) @type
                    declarator: (variable_declarator
                        name: (identifier) @name))
            "#;
            if let Ok(q) = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src) {
                let type_idx = q.capture_index_for_name("type").unwrap();
                let name_idx = q.capture_index_for_name("name").unwrap();
                let found: Vec<LocalVar> = run_query(&q, child, ctx.bytes, None)
                    .into_iter()
                    .filter_map(|captures| {
                        let ty_node = captures.iter().find(|(idx, _)| *idx == type_idx)?.1;
                        let name_node = captures.iter().find(|(idx, _)| *idx == name_idx)?.1;
                        if ty_node.start_byte() >= ctx.offset {
                            return None;
                        }
                        let ty = ty_node.utf8_text(ctx.bytes).ok()?;
                        let name = name_node.utf8_text(ctx.bytes).ok()?;
                        let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
                        if raw_ty == "var" {
                            return Some(match infer_type_from_initializer(ty_node, ctx.bytes) {
                                Some(t) => LocalVar {
                                    name: Arc::from(name),
                                    type_internal: Arc::from(java_type_to_internal(&t).as_str()),
                                    init_expr: None,
                                },
                                None => LocalVar {
                                    name: Arc::from(name),
                                    type_internal: Arc::from("var"),
                                    init_expr: get_initializer_text(ty_node, ctx.bytes),
                                },
                            });
                        }
                        Some(LocalVar {
                            name: Arc::from(name),
                            type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
                            init_expr: None,
                        })
                    })
                    .collect();
                vars.extend(found);
            }
            // Recursive entry into nested ERROR
            collect_locals_in_errors(ctx, child, vars);
        } else {
            collect_locals_in_errors(ctx, child, vars);
        }
    }
}
