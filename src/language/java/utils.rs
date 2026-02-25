use crate::completion::context::CursorLocation;
use crate::language::java::SENTINEL;
use ropey::Rope;
use std::sync::Arc;
use tree_sitter::Node;

pub(crate) fn find_top_error_node(root: Node) -> Option<Node> {
    let mut cursor = root.walk();
    root.children(&mut cursor)
        .find(|&child| child.kind() == "ERROR")
}

pub(crate) fn build_internal_name(
    package: &Option<Arc<str>>,
    class: &Option<Arc<str>>,
) -> Option<Arc<str>> {
    match (package, class) {
        (Some(pkg), Some(cls)) => Some(Arc::from(format!("{}/{}", pkg, cls).as_str())),
        (None, Some(cls)) => Some(Arc::clone(cls)),
        _ => None,
    }
}

pub(crate) fn is_comment_kind(kind: &str) -> bool {
    kind == "line_comment" || kind == "block_comment"
}

pub(crate) fn find_ancestor<'a>(mut node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    loop {
        node = node.parent()?;
        if node.kind() == kind {
            return Some(node);
        }
    }
}

/// 去掉字符串里的 SENTINEL（注入路径下 prefix 可能含有它）
pub(crate) fn strip_sentinel(s: &str) -> String {
    s.replace(SENTINEL, "")
}

/// 把 CursorLocation 里残留的 SENTINEL 清掉
pub(crate) fn strip_sentinel_from_location(loc: CursorLocation) -> CursorLocation {
    match loc {
        CursorLocation::MemberAccess {
            receiver_type,
            member_prefix,
            receiver_expr,
        } => CursorLocation::MemberAccess {
            receiver_type,
            member_prefix: strip_sentinel(&member_prefix),
            receiver_expr: strip_sentinel(&receiver_expr),
        },
        CursorLocation::ConstructorCall {
            class_prefix,
            expected_type,
        } => CursorLocation::ConstructorCall {
            class_prefix: strip_sentinel(&class_prefix),
            expected_type,
        },
        CursorLocation::Expression { prefix } => CursorLocation::Expression {
            prefix: strip_sentinel(&prefix),
        },
        CursorLocation::MethodArgument { prefix } => CursorLocation::MethodArgument {
            prefix: strip_sentinel(&prefix),
        },
        CursorLocation::Annotation { prefix } => CursorLocation::Annotation {
            prefix: strip_sentinel(&prefix),
        },
        CursorLocation::TypeAnnotation { prefix } => CursorLocation::TypeAnnotation {
            prefix: strip_sentinel(&prefix),
        },
        CursorLocation::VariableName { type_name } => CursorLocation::VariableName { type_name },
        CursorLocation::Import { prefix } => CursorLocation::Import {
            prefix: strip_sentinel(&prefix),
        },
        other => other,
    }
}

/// Count unmatched `{` and `(` in src, return closing string to append.
pub(crate) fn close_open_brackets(src: &str) -> String {
    let mut braces: i32 = 0;
    let mut parens: i32 = 0;
    let mut in_string = false;
    let mut in_char = false;
    let mut escaped = false;
    let mut it = src.chars().peekable();
    while let Some(c) = it.next() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' => escaped = true,
            '"' if !in_char => in_string = !in_string,
            '\'' if !in_string => in_char = !in_char,
            _ if in_string || in_char => {}
            '/' if it.peek() == Some(&'/') => {
                // skip to end of line
                for nc in it.by_ref() {
                    if nc == '\n' {
                        break;
                    }
                }
            }
            '/' if it.peek() == Some(&'*') => {
                // skip block comment
                it.next(); // consume '*'
                let mut prev = ' ';
                for nc in it.by_ref() {
                    if prev == '*' && nc == '/' {
                        break;
                    }
                    prev = nc;
                }
            }
            '{' => braces += 1,
            '}' => {
                if braces > 0 {
                    braces -= 1;
                }
            }
            '(' => parens += 1,
            ')' => {
                if parens > 0 {
                    parens -= 1;
                }
            }
            _ => {}
        }
    }
    let mut tail = String::new();
    for _ in 0..parens {
        tail.push(')');
    }
    tail.push(';');
    for _ in 0..braces {
        tail.push('}');
    }
    tail
}

pub(crate) fn get_initializer_text(type_node: Node, bytes: &[u8]) -> Option<String> {
    let decl = type_node.parent()?;
    if decl.kind() != "local_variable_declaration" {
        return None;
    }
    let mut cursor = decl.walk();
    for child in decl.named_children(&mut cursor) {
        if child.kind() != "variable_declarator" {
            continue;
        }
        let init = child.named_child(1)?;
        return init.utf8_text(bytes).ok().map(|s| s.to_string());
    }
    None
}

pub(crate) fn find_enclosing_method_in_error(root: Node, offset: usize) -> Option<Node> {
    // When the file is a top-level ERROR, method_declaration nodes may still
    // exist as direct children of the ERROR node
    fn dfs<'a>(node: Node<'a>, offset: usize, result: &mut Option<Node<'a>>) {
        if node.start_byte() > offset {
            return;
        }
        if node.kind() == "method_declaration"
            && node.start_byte() <= offset
            && node.end_byte() >= offset
        {
            *result = Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            dfs(child, offset, result);
        }
    }
    let mut result = None;
    dfs(root, offset, &mut result);
    result
}

pub fn infer_type_from_initializer(type_node: Node, bytes: &[u8]) -> Option<String> {
    let decl = type_node.parent()?;
    if decl.kind() != "local_variable_declaration" {
        return None;
    }
    let mut cursor = decl.walk();
    for child in decl.named_children(&mut cursor) {
        if child.kind() != "variable_declarator" {
            continue;
        }
        let init = child.named_child(1)?;
        match init.kind() {
            "object_creation_expression" => {
                let ty_node = init.child_by_field_name("type")?;
                let text = ty_node.utf8_text(bytes).ok()?;
                let simple = text.split('<').next()?.trim();
                if !simple.is_empty() {
                    return Some(simple.to_string());
                }
            }
            _ => {
                let text = init.utf8_text(bytes).ok()?;
                if let Some(rest) = text.trim().strip_prefix("new ") {
                    let class_name = rest.split('(').next()?.split('<').next()?.trim();
                    if !class_name.is_empty() {
                        return Some(class_name.to_string());
                    }
                }
            }
        }
    }
    None
}

pub fn is_cursor_in_comment_with_rope(source: &str, rope: &Rope, offset: usize) -> bool {
    let before = &source[..offset];
    let last_open = before.rfind("/*");
    let last_close = before.rfind("*/");
    if let Some(open) = last_open {
        match last_close {
            None => return true,
            Some(close) if open > close => return true,
            _ => {}
        }
    }
    let line_idx = rope.byte_to_line(offset.min(source.len().saturating_sub(1)));
    let line_byte_start = rope.line_to_byte(line_idx);
    let safe_offset = offset.max(line_byte_start).min(source.len());
    is_in_line_comment(&source[line_byte_start..safe_offset])
}

pub fn is_cursor_in_comment(source: &str, offset: usize) -> bool {
    let rope = Rope::from_str(source);
    is_cursor_in_comment_with_rope(source, &rope, offset)
}

fn is_in_line_comment(line: &str) -> bool {
    let mut chars = line.chars().peekable();
    let mut in_string = false;
    let mut in_char = false;
    let mut escaped = false;
    while let Some(c) = chars.next() {
        if escaped {
            escaped = false;
            continue;
        }
        match c {
            '\\' => escaped = true,
            '"' if !in_char => in_string = !in_string,
            '\'' if !in_string => in_char = !in_char,
            '/' if !in_string && !in_char => {
                if chars.peek() == Some(&'/') {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

pub fn java_type_to_internal(ty: &str) -> String {
    ty.trim().replace('.', "/")
}

pub fn find_error_ancestor(mut node: Node) -> Option<Node> {
    loop {
        if node.kind() == "ERROR" {
            return Some(node);
        }
        node = node.parent()?;
    }
}

pub fn error_has_new_keyword(error_node: Node) -> bool {
    let mut cursor = error_node.walk();
    error_node.children(&mut cursor).any(|c| c.kind() == "new")
}

pub fn find_identifier_in_error(error_node: Node) -> Option<Node> {
    let mut cursor = error_node.walk();
    error_node
        .children(&mut cursor)
        .find(|c| c.kind() == "identifier" || c.kind() == "type_identifier")
}

pub fn error_has_trailing_dot(error_node: Node, offset: usize) -> bool {
    let mut cursor = error_node.walk();
    let children: Vec<Node> = error_node.children(&mut cursor).collect();
    children
        .last()
        .is_some_and(|n| n.kind() == "." && n.end_byte() <= offset)
}

pub fn is_in_type_position(id_node: Node, decl_node: Node) -> bool {
    let mut walker = decl_node.walk();
    for child in decl_node.named_children(&mut walker) {
        if child.kind() == "modifiers" {
            continue;
        }
        return child.id() == id_node.id();
    }
    false
}

pub fn is_in_name_position(id_node: Node, decl_node: Node) -> bool {
    let mut wc = decl_node.walk();
    for declarator in decl_node.named_children(&mut wc) {
        if declarator.kind() != "variable_declarator" {
            continue;
        }
        if let Some(name_node) = declarator.child_by_field_name("name")
            && name_node.id() == id_node.id()
        {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use crate::language::java::utils::close_open_brackets;

    #[test]
    fn test_close_open_brackets_unit() {
        assert_eq!(close_open_brackets("class A { void f() {"), ";}}");
        assert_eq!(close_open_brackets("foo(a, b"), ");");
        assert_eq!(close_open_brackets("class A { void f() { foo("), ");}}");
        assert_eq!(close_open_brackets("class A {}"), ";");
        assert_eq!(close_open_brackets("}}}}"), ";");
    }
}
