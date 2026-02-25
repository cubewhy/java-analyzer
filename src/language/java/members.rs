use std::sync::Arc;

use tree_sitter::Node;

use crate::{completion::context::CurrentClassMember, language::java::JavaContextExtractor};

// TODO: duplicate logic, need a refactor
#[rustfmt::skip]
fn is_java_keyword(name: &str) -> bool {
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

    // Second pass: method_invocation 可能是被误读的方法声明
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

        let mut is_static = false;
        let mut is_private = false;
        let mut ret_type = "void";

        // 修饰符和返回类型藏在前驱的嵌套 ERROR 节点里
        for prev in children[..mi_pos].iter().rev() {
            match prev.kind() {
                "identifier" => {
                    let t = ctx.node_text(*prev);
                    if t == "static" {
                        is_static = true;
                    }
                    if t == "private" {
                        is_private = true;
                    }
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
                    // 扫嵌套 ERROR 的子节点
                    let mut pc = prev.walk();
                    for pchild in prev.children(&mut pc) {
                        match pchild.kind() {
                            "static" => is_static = true,
                            "private" => is_private = true,
                            "identifier" => {
                                let t = ctx.node_text(pchild);
                                if t == "static" {
                                    is_static = true;
                                }
                                if t == "private" {
                                    is_private = true;
                                }
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

        let args = mi_node
            .child_by_field_name("arguments")
            .map(|n| ctx.node_text(n))
            .unwrap_or("()");

        result.push(CurrentClassMember {
            name: Arc::from(name),
            is_method: true,
            is_static,
            is_private,
            descriptor: Arc::from(
                crate::index::source::build_java_descriptor(args, ret_type).as_str(),
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
    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>" && !is_java_keyword(n))?;

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

    let name = name.filter(|n| *n != "<init>" && *n != "<clinit>" && !is_java_keyword(n))?;

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

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;
    use tree_sitter::Parser;

    fn setup(source: &'_ str) -> (JavaContextExtractor<'_>, tree_sitter::Tree) {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        let tree = parser.parse(source, None).unwrap();

        let ctx = JavaContextExtractor {
            source,
            bytes: source.as_bytes(),
            offset: source.len(),
            rope: Rope::from_str(source),
        };
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

        let a = members.iter().find(|m| m.name.as_ref() == "a").unwrap();
        assert!(!a.is_method && !a.is_static && !a.is_private);

        let b = members.iter().find(|m| m.name.as_ref() == "b").unwrap();
        assert!(!b.is_method && b.is_static && b.is_private);

        let ma = members
            .iter()
            .find(|m| m.name.as_ref() == "methodA")
            .unwrap();
        assert!(ma.is_method && !ma.is_static && !ma.is_private);

        let mb = members
            .iter()
            .find(|m| m.name.as_ref() == "methodB")
            .unwrap();
        assert!(mb.is_method && mb.is_static && mb.is_private);
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

        // `<init>` 和 `<clinit>` 应该被过滤掉，只保留 normalMethod
        assert!(members.iter().any(|m| m.name.as_ref() == "normalMethod"));
        assert!(!members.iter().any(|m| m.name.as_ref() == "<init>"));
        assert!(!members.iter().any(|m| m.name.as_ref() == "<clinit>"));
        assert!(!members.iter().any(|m| m.name.as_ref() == "A")); // A() parse 出来通常 name 是 A
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

        let x = members.iter().find(|m| m.name.as_ref() == "x").unwrap();
        assert!(!x.is_method && x.is_static && x.is_private);

        let y = members.iter().find(|m| m.name.as_ref() == "y").unwrap();
        assert!(!y.is_method && y.is_static && y.is_private);
    }

    #[test]
    fn test_swallowed_error_node_members() {
        // 测试由于上一个方法缺少大括号，导致后续方法被解析到 `block` 的 ERROR 节点中
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
            .find(|m| m.name.as_ref() == "swallowedMethod");
        assert!(
            swallowed.is_some(),
            "Should extract method even if it's swallowed inside an ERROR node in a previous method's block"
        );
        let swallowed = swallowed.unwrap();
        assert!(swallowed.is_method && swallowed.is_static && swallowed.is_private);
    }

    #[test]
    fn test_misread_method_as_local_variable() {
        // 这是 parse_misread_method 专门处理的经典语法树错误
        // 在方法体中缺少分号等情况时，TS 会把 `private static Object test` 解析为局部变量声明
        // 然后把 `() {}` 解析为相邻的 ERROR 节点
        let src = indoc::indoc! {r#"
        class A {
            void brokenMethod() {
                int x = 1 // 缺分号
            
            private static String misreadMethod(int a) {}
        }
        "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        let misread = members.iter().find(|m| m.name.as_ref() == "misreadMethod");
        assert!(
            misread.is_some(),
            "Should reconstruct method from local_variable_declaration and ERROR node siblings"
        );

        let misread = misread.unwrap();
        assert!(misread.is_method && misread.is_static && misread.is_private);
    }

    #[test]
    fn test_partial_methods_from_top_level_error() {
        // 整个类结构因为严重的语法错误崩溃，退化成顶层 ERROR 节点
        // parse_partial_methods_from_error 必须能从中捞出方法签名
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
            .find(|m| m.name.as_ref() == "salvagedMethod")
            .unwrap();
        assert!(salvaged1.is_method && salvaged1.is_static && !salvaged1.is_private);

        let salvaged2 = members
            .iter()
            .find(|m| m.name.as_ref() == "anotherSalvaged")
            .unwrap();
        assert!(salvaged2.is_method && !salvaged2.is_static && salvaged2.is_private);
    }

    #[test]
    fn test_no_keyword_as_member_from_incomplete_declaration() {
        // `pub` 触发 ERROR 节点，tree-sitter 可能把后续的 `public` 误读为字段名
        let src = indoc::indoc! {r#"
    class A {
        pub // incomplete keyword-like identifier

        public static void realMethod() {}
    }
    "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        // 关键字不能作为成员名出现
        let keyword_members: Vec<_> = members
            .iter()
            .filter(|m| is_java_keyword(m.name.as_ref()))
            .collect();
        assert!(
            keyword_members.is_empty(),
            "Java keywords should never appear as member names, got: {:?}",
            keyword_members
                .iter()
                .map(|m| m.name.as_ref())
                .collect::<Vec<_>>()
        );

        // realMethod 应该正常提取
        assert!(members.iter().any(|m| m.name.as_ref() == "realMethod"));
    }

    #[test]
    fn test_no_keyword_as_field_from_error_in_method_body() {
        // 方法体内的语法错误导致后续声明被吞入 ERROR，
        // 其中的修饰符关键字不能被误提为字段
        let src = indoc::indoc! {r#"
    class A {
        void broken() {
            int x = foo(  // unclosed call

        public String realField;
        public void realMethod() {}
    }
    "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        for m in &members {
            assert!(
                !is_java_keyword(m.name.as_ref()),
                "keyword '{}' must not appear as a member name",
                m.name
            );
        }
    }

    #[test]
    fn test_identifier_named_pub_is_valid_field() {
        // `pub` 本身不是 Java 关键字，作为字段名应该允许（尽管罕见）
        let src = indoc::indoc! {r#"
    class A {
        int pub;
    }
    "#};
        let (ctx, tree) = setup(src);
        let mut members = Vec::new();
        collect_members_from_node(&ctx, tree.root_node(), &mut members);

        // `pub` 不是关键字，应正常提取
        assert!(
            members.iter().any(|m| m.name.as_ref() == "pub"),
            "`pub` is not a Java keyword and should be extracted as a field"
        );
    }
}
