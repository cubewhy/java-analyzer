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

pub fn extract_locals(
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
                return Some(LocalVar {
                    name: Arc::from(name),
                    type_internal: Arc::from("var"),
                    init_expr: get_initializer_text(ty_node, ctx.bytes),
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

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;
    use tree_sitter::Parser;

    fn setup(source: &'_ str, offset: usize) -> (JavaContextExtractor<'_>, tree_sitter::Tree) {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        let tree = parser.parse(source, None).unwrap();

        let ctx = JavaContextExtractor {
            source,
            bytes: source.as_bytes(),
            offset,
            rope: Rope::from_str(source),
        };
        (ctx, tree)
    }

    #[test]
    fn test_extract_standard_locals() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                int a = 1;
                String b = "hello";
                List<String> c = new ArrayList<>();
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        // 验证提取结果
        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "a" && v.type_internal.as_ref() == "int")
        );
        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "b" && v.type_internal.as_ref() == "String")
        );
        // 验证泛型被清洗
        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "c" && v.type_internal.as_ref() == "List")
        );
    }

    #[test]
    fn test_extract_params() {
        let src = indoc::indoc! {r#"
        class A {
            void f(int p1, String p2) {
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "p1" && v.type_internal.as_ref() == "int")
        );
        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "p2" && v.type_internal.as_ref() == "String")
        );
    }

    #[test]
    fn test_var_capture_init_expr() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                var map = new HashMap<String, String>();
                var list = new ArrayList<>();
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        let map_var = vars
            .iter()
            .find(|v| v.name.as_ref() == "map")
            .expect("Should find map");
        assert_eq!(map_var.type_internal.as_ref(), "var");
        assert!(map_var.init_expr.as_ref().unwrap().contains("new HashMap"));
    }

    #[test]
    fn test_var_inference_fallback() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                var unknown = someMethodCall();
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        // 无法推断时，类型应为 "var"，并且携带初始化表达式以供后续分析（如果支持的话）
        let v = vars.iter().find(|v| v.name.as_ref() == "unknown").unwrap();
        assert_eq!(v.type_internal.as_ref(), "var");
        assert_eq!(v.init_expr.as_deref(), Some("someMethodCall()"));
    }

    #[test]
    fn test_scope_visibility_ignore_future_vars() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                int visible = 1;
                // cursor here
                int invisible = 2;
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        assert!(vars.iter().any(|v| v.name.as_ref() == "visible"));
        assert!(!vars.iter().any(|v| v.name.as_ref() == "invisible"));
    }

    #[test]
    fn test_misread_declaration_missing_semicolon() {
        // 这是 collect_misread_decls 的重点测试场景
        // Tree-sitter 经常把没有分号的 `String s = "v"` 解析为：
        // variable_declarator 内部包含了一个 assignment_expression
        // 且类型 `String` 变成了一个 ERROR 节点或游离的 identifier
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                String s = "incomplete"
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        // 期望能从容错逻辑中提取出 s
        assert!(
            vars.iter()
                .any(|v| v.name.as_ref() == "s" && v.type_internal.as_ref() == "String"),
            "Should parse variable 's' even without semicolon. Found: {:?}",
            vars.iter().map(|v| v.name.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_misread_var_capture_init_expr() {
        // 测试在语法错误（缺分号）情况下的 var 推断
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                var x = new HashSet<>()
                // cursor here
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let vars = extract_locals(&ctx, tree.root_node(), cursor_node);

        // 注意：collect_misread_decls 内部逻辑是如果检测到 var，
        // type_internal 设为 "var"，init_expr 设为右值
        let x = vars
            .iter()
            .find(|v| v.name.as_ref() == "x")
            .expect("Should find 'x'");
        assert_eq!(x.type_internal.as_ref(), "var");
        assert!(x.init_expr.as_ref().unwrap().contains("new HashSet"));
    }

    #[test]
    fn test_locals_inside_error_nodes() {
        // 针对 extract_locals_from_error_nodes 的测试
        // 这种情况通常发生在极其破碎的代码结构中，例如 try-catch 写了一半
        // 这个函数目前在 extract_locals 中没有被直接调用（原代码可能有，或者被移除了）
        // 但我们仍然应该测试它的逻辑正确性，以便将来集成
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                try {
                    String insideError = "ok";
                } catch (
                // cursor
            }
        }
        "#};
        let offset = src.find("// cursor").unwrap();
        let (ctx, tree) = setup(src, offset);

        // 我们直接调用 extract_locals_from_error_nodes 来测试私有/crate私有逻辑
        // 因为 tree-sitter 可能会把整个 try-catch 块标记为 ERROR
        let vars = extract_locals_from_error_nodes(&ctx, tree.root_node());

        assert!(
            vars.iter().any(|v| v.name.as_ref() == "insideError"),
            "Should extract locals deeply nested inside ERROR nodes"
        );
    }
}
