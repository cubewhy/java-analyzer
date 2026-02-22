use tree_sitter::StreamingIterator;
use tree_sitter::{Node, Query, QueryCursor};

/// Execute the query and return all matching capture groups
/// Each element is `Vec<(capture_name_index, Node)>`
pub fn run_query<'tree>(
    query: &Query,
    node: Node<'tree>,
    source: &[u8],
    byte_range: Option<std::ops::Range<usize>>,
) -> Vec<Vec<(u32, Node<'tree>)>> {
    let mut cursor = QueryCursor::new();
    if let Some(range) = byte_range {
        cursor.set_byte_range(range);
    }

    let mut results = Vec::new();
    let mut matches = cursor.matches(query, node, source);

    while let Some(m) = matches.next() {
        let captures: Vec<(u32, Node<'tree>)> =
            m.captures.iter().map(|c| (c.index, c.node)).collect();
        results.push(captures);
    }

    results
}

/// Find node text in a set of captures by capture index
pub fn capture_text<'s>(
    captures: &[(u32, tree_sitter::Node<'_>)],
    index: u32,
    source: &'s [u8],
) -> Option<&'s str> {
    captures
        .iter()
        .find(|(idx, _)| *idx == index)
        .and_then(|(_, node)| node.utf8_text(source).ok())
}

/// When cursor_node is within error node and find_ancestor fails,
/// Find the nearest method_declaration containing that position throughout the entire tree using byte offset.
pub fn find_method_by_offset(root: Node, offset: usize) -> Option<Node> {
    let mut result: Option<Node> = None;
    // Depth-first traversal to find the deepest node containing the offset and kind == "method_declaration".
    fn dfs<'a>(node: Node<'a>, offset: usize, result: &mut Option<Node<'a>>) {
        if node.start_byte() > offset || node.end_byte() < offset {
            return;
        }
        if node.kind() == "method_declaration" {
            *result = Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            dfs(child, offset, result);
        }
    }
    dfs(root, offset, &mut result);
    result
}
