use ropey::Rope;
use tree_sitter::Node;

use crate::{
    completion::CursorLocation,
    language::java::{
        JavaContextExtractor, SENTINEL,
        location::determine_location,
        utils::{
            close_open_brackets, error_has_new_keyword, error_has_trailing_dot, find_ancestor,
            find_error_ancestor, find_identifier_in_error, strip_sentinel,
            strip_sentinel_from_location,
        },
    },
};

/// Construct the injected source: Replace the token at the cursor with SENTINEL (or insert it directly)
pub fn build_injected_source(
    extractor: &JavaContextExtractor,
    cursor_node: Option<Node>,
) -> String {
    let before = &extractor.source[..extractor.offset];
    let trimmed = before.trim_end();

    // Case 1: cursor right after `new` keyword (bare `new` or `new `)
    if trimmed.ends_with("new") {
        let immediate_suffix = &extractor.source[extractor.offset..];
        // Check if there are newlines before the next token
        let separated_by_newline = immediate_suffix
            .chars()
            .take_while(|c| c.is_whitespace())
            .any(|c| c == '\n' || c == '\r');

        let after = immediate_suffix.trim_start();
        let next_meaningful = after.chars().next();
        let is_continuation = next_meaningful.is_some_and(|c| c.is_alphanumeric() || c == '_');

        // If separated by newline, we assume the user stopped typing 'new ...'
        // and the next line is unrelated.
        if separated_by_newline || !is_continuation {
            return inject_at(
                extractor,
                extractor.offset,
                extractor.offset,
                &format!(" {SENTINEL}()"),
            );
        }
    }

    // Case 2/3: cursor inside an ERROR node
    let error_node = cursor_node.and_then(|n| {
        if n.kind() == "ERROR" {
            Some(n)
        } else {
            find_error_ancestor(n)
        }
    });

    if let Some(err) = error_node {
        // Case 2: ERROR contains `new` keyword
        if error_has_new_keyword(err) {
            if let Some(id_node) = find_identifier_in_error(err) {
                let start = id_node.start_byte();
                let end = extractor.offset.min(id_node.end_byte());
                if end > start {
                    let prefix = &extractor.source[start..end];
                    return inject_at(
                        extractor,
                        start,
                        id_node.end_byte(),
                        &format!("{prefix}{SENTINEL}()"),
                    );
                }
            }
            return inject_at(
                extractor,
                extractor.offset,
                extractor.offset,
                &format!(" {SENTINEL}()"),
            );
        }

        // Case 3: ERROR has trailing dot (e.g. `cl.`)
        if error_has_trailing_dot(err, extractor.offset) {
            // Must include `;` so tree-sitter parses as expression_statement > field_access
            // instead of scoped_type_identifier
            return inject_at(
                extractor,
                extractor.offset,
                extractor.offset,
                &format!("{SENTINEL};"),
            );
        }
    }

    // Case 4: normal identifier/type_identifier — append sentinel
    let (replace_start, replace_end) = match cursor_node {
        Some(n)
            if (n.kind() == "identifier" || n.kind() == "type_identifier")
                && n.start_byte() < extractor.offset =>
        {
            (n.start_byte(), extractor.offset.min(n.end_byte()))
        }
        _ => (extractor.offset, extractor.offset),
    };

    if replace_start == replace_end {
        // Nothing at cursor, insert sentinel as a statement
        inject_at(
            extractor,
            extractor.offset,
            extractor.offset,
            &format!("{SENTINEL};"),
        )
    } else {
        let prefix = &extractor.source[replace_start..replace_end];
        // cursor in scoped_type_identifier (e.g., java.util.A) -> add semicolon to turn into expression statement
        // let in_scoped_type = cursor_node.is_some_and(|n| {
        //     find_ancestor(n, "scoped_type_identifier").is_some()
        //         || n.kind() == "scoped_type_identifier"
        // });

        // Check if we are inside a `new` expression to inject parentheses
        // This helps tree-sitter terminate the object_creation_expression correctly
        // instead of swallowing the next lines into the type node.
        let is_constructor_ctx = cursor_node.is_some_and(|n| {
            let mut curr = Some(n);
            while let Some(node) = curr {
                if node.kind() == "object_creation_expression" {
                    return true;
                }
                if node.kind() == "method_declaration"
                    || node.kind() == "class_declaration"
                    || node.kind() == "block"
                {
                    break;
                }
                curr = node.parent();
            }
            false
        });

        let suffix = if is_constructor_ctx { "()" } else { "" };

        inject_at(
            extractor,
            replace_start,
            replace_end,
            &format!("{prefix}{SENTINEL}{suffix};"),
        )
        // if in_scoped_type {
        // } else {
        //     inject_at(
        //         extractor,
        //         replace_start,
        //         replace_end,
        //         &format!("{prefix}{SENTINEL}{suffix}"),
        //     )
        // }
    }
}

fn inject_at(
    extractor: &JavaContextExtractor,
    replace_start: usize,
    replace_end: usize,
    replacement: &str,
) -> String {
    let mut s = String::with_capacity(extractor.source.len() + replacement.len() + 16);
    s.push_str(&extractor.source[..replace_start]);
    s.push_str(replacement);
    s.push_str(&extractor.source[replace_end..]);
    let tail = close_open_brackets(&s);
    s.push_str(&tail);
    s
}

/// Inject SENTINEL, reparse, and run determine_location on the new tree.
pub fn inject_and_determine(
    ctx: &JavaContextExtractor,
    cursor_node: Option<Node>,
    trigger_char: Option<char>,
) -> Option<(CursorLocation, String)> {
    let injected_source = build_injected_source(ctx, cursor_node);
    let sentinel_offset = injected_source.find(SENTINEL)?;
    let sentinel_end = sentinel_offset + SENTINEL.len();

    let mut parser = ctx.make_parser();
    let new_tree = parser.parse(&injected_source, None)?;
    let new_root = new_tree.root_node();

    let sentinel_node = new_root.named_descendant_for_byte_range(sentinel_offset, sentinel_end)?;

    tracing::debug!(
        injected_source = injected_source,
        sentinel_node_kind = sentinel_node.kind(),
        "inject_and_determine"
    );

    if sentinel_node.kind() == "identifier" || sentinel_node.kind() == "type_identifier" {
        let tmp = JavaContextExtractor {
            source: &injected_source,
            bytes: injected_source.as_bytes(),
            offset: sentinel_end,
            rope: Rope::from_str(&injected_source),
        };
        let (loc, q) = determine_location(&tmp, Some(sentinel_node), trigger_char);
        let clean_q = if q == SENTINEL {
            String::new()
        } else {
            strip_sentinel(&q)
        };
        let clean_loc = strip_sentinel_from_location(loc);
        return Some((clean_loc, clean_q));
    }

    let mut cur = sentinel_node;
    loop {
        if cur.kind() == "identifier" || cur.kind() == "type_identifier" {
            let tmp = JavaContextExtractor {
                source: &injected_source,
                bytes: injected_source.as_bytes(),
                offset: sentinel_end,
                rope: Rope::from_str(&injected_source),
            };
            let (loc, q) = determine_location(&tmp, Some(cur), trigger_char);
            let clean_q = strip_sentinel(&q);
            let clean_loc = strip_sentinel_from_location(loc);
            return Some((clean_loc, clean_q));
        }
        cur = cur.parent()?;
        if cur.kind() == "method_declaration" || cur.kind() == "program" {
            break;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use ropey::Rope;
    use tree_sitter::Parser;

    fn setup_ctx<'a>(
        source: &'a str,
        offset: usize,
    ) -> (JavaContextExtractor<'a>, tree_sitter::Tree) {
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
    fn test_build_injected_source_trailing_dot() {
        // Case 3: ERROR node with trailing dot
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                cl.
            }
        }
        "#};
        // Find the end of `cl.`
        let offset = src.find("cl.").unwrap() + 3;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let injected = build_injected_source(&ctx, cursor_node);

        // Expected behavior: Inject __KIRO__ after the dot and add a semicolon to close the statement,
        // and close_open_brackets will automatically complete the missing `}`.
        assert!(
            injected.contains(&format!("cl.{SENTINEL};")),
            "Injected source should contain sentinel and semicolon, got:\n{}",
            injected
        );
    }

    #[test]
    fn test_build_injected_source_new_keyword_with_newline() {
        // Case 1: A newline after the `new` keyword causes the AST to swallow the next line.
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new 
                
                System.out.println("hello");
            }
        }
        "#};
        let offset = src.find("new ").unwrap() + 4;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let injected = build_injected_source(&ctx, cursor_node);

        // Expectation: Because a newline character follows `new`, it's inferred that the user hasn't finished writing the code, so `__KIRO__()` is injected directly as an empty constructor.
        assert!(
            injected.contains(&format!("new  {SENTINEL}()")),
            "Injected source should treat separated new as complete call, got:\n{}",
            injected
        );
    }

    #[test]
    fn test_build_injected_source_normal_identifier() {
        // Case 4: Ordinary Identifiers
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                myVa
            }
        }
        "#};
        let offset = src.find("myVa").unwrap() + 4;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let injected = build_injected_source(&ctx, cursor_node);

        // Expectation: Since there is no suffix, treat it as an expression statement and inject a semicolon.
        assert!(
            injected.contains(&format!("myVa{SENTINEL}")),
            "Injected source should append sentinel to identifier, got:\n{}",
            injected
        );
    }

    #[test]
    fn test_build_injected_source_scoped_type() {
        // Case 4: scoped_type_identifier (FQN)
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                java.util.A
            }
        }
        "#};
        let offset = src.find("java.util.A").unwrap() + 11;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let injected = build_injected_source(&ctx, cursor_node);

        // Expectation: When injecting package name types, add a semicolon to convert it to a valid MemberAccess.
        assert!(
            injected.contains(&format!("java.util.A{SENTINEL};")),
            "Injected source should append semicolon for scoped types, got:\n{}",
            injected
        );
    }

    #[test]
    /// Verify whether the CursorLocation can actually be resolved after injection.
    fn test_inject_and_determine_member_access() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                cl.
            }
        }
        "#};
        let offset = src.find("cl.").unwrap() + 3;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let result = inject_and_determine(&ctx, cursor_node, None);
        assert!(result.is_some(), "inject_and_determine should succeed");

        let (location, query) = result.unwrap();

        // Expected result: MemberAccess, receiver_expr is "cl", and query is empty.
        assert!(
            matches!(
                &location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "cl"
            ),
            "Expected MemberAccess for cl., got {:?}",
            location
        );
        assert_eq!(query, "", "Query should be empty");
    }

    #[test]
    fn test_inject_and_determine_constructor() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new RandomCla
            }
        }
        "#};
        let offset = src.find("RandomCla").unwrap() + 9;
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let result = inject_and_determine(&ctx, cursor_node, None);
        assert!(result.is_some(), "inject_and_determine should succeed");

        let (location, query) = result.unwrap();

        assert!(
            matches!(
                &location,
                CursorLocation::ConstructorCall { class_prefix, .. }
                if class_prefix == "RandomCla"
            ),
            "Expected ConstructorCall with prefix RandomCla, got {:?}",
            location
        );
        assert_eq!(query, "RandomCla", "Query should match the prefix");
    }

    #[test]
    fn test_inject_and_determine_plain_identifier() {
        let src = indoc::indoc! {r#"
    class A {
        public static void func() {}
        void f() {
            func
        }
    }
    "#};
        let offset = src.find("func\n").unwrap() + 4; // 指向方法体内的 func
        let (ctx, tree) = setup_ctx(src, offset);
        let cursor_node = ctx.find_cursor_node(tree.root_node());

        let result = inject_and_determine(&ctx, cursor_node, None);
        assert!(
            result.is_some(),
            "inject_and_determine should succeed for plain identifier"
        );

        let (location, query) = result.unwrap();
        assert_eq!(query, "func");
        // 应当解析为某种可补全的上下文（如 Identifier / MethodCall）
        println!("location: {:?}", location);
    }
}
