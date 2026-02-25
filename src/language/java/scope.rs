use std::sync::Arc;

use tree_sitter::{Node, Query};

use crate::language::{
    java::{JavaContextExtractor, utils::find_ancestor},
    ts_utils::{capture_text, run_query},
};

pub fn extract_package(ctx: &JavaContextExtractor, root: Node) -> Option<Arc<str>> {
    let q = Query::new(
        &tree_sitter_java::LANGUAGE.into(),
        r#"(package_declaration) @pkg"#,
    )
    .ok()?;
    let idx = q.capture_index_for_name("pkg")?;
    let results = run_query(&q, root, ctx.bytes, None);
    let text = results
        .first()
        .and_then(|caps| capture_text(caps, idx, ctx.bytes))?;
    let pkg = text
        .trim_start_matches("package")
        .trim()
        .trim_end_matches(';')
        .trim()
        .replace('.', "/");
    Some(Arc::from(pkg.as_str()))
}

pub fn extract_imports(ctx: &JavaContextExtractor, root: Node) -> Vec<Arc<str>> {
    let q = match Query::new(
        &tree_sitter_java::LANGUAGE.into(),
        r#"(import_declaration) @import"#,
    ) {
        Ok(q) => q,
        Err(_) => return vec![],
    };
    let idx = q.capture_index_for_name("import").unwrap();
    run_query(&q, root, ctx.bytes, None)
        .into_iter()
        .filter_map(|caps| {
            let text = capture_text(&caps, idx, ctx.bytes)?;
            let cleaned = text
                .trim_start_matches("import")
                .trim()
                .trim_end_matches(';')
                .trim();
            if cleaned.is_empty() {
                None
            } else {
                Some(Arc::from(cleaned))
            }
        })
        .collect()
}

pub fn extract_enclosing_class(
    ctx: &JavaContextExtractor,
    cursor_node: Option<Node>,
) -> Option<Arc<str>> {
    let class_node = cursor_node.and_then(|n| find_ancestor(n, "class_declaration"))?;
    let name_node = class_node.child_by_field_name("name")?;
    Some(Arc::from(ctx.node_text(name_node)))
}

pub(crate) fn extract_enclosing_class_by_offset(
    ctx: &JavaContextExtractor,
    root: Node,
) -> Option<Arc<str>> {
    let mut result: Option<Arc<str>> = None;
    fn dfs<'a>(node: Node<'a>, offset: usize, bytes: &[u8], result: &mut Option<Arc<str>>) {
        if node.start_byte() > offset || node.end_byte() <= offset {
            return;
        }
        if node.kind() == "class_declaration"
            && let Some(name_node) = node.child_by_field_name("name")
            && let Ok(name) = name_node.utf8_text(bytes)
        {
            *result = Some(Arc::from(name));
        }
        // Handle top-level ERROR: find `class` keyword followed by identifier
        if node.kind() == "ERROR" {
            let mut cursor = node.walk();
            let children: Vec<Node> = node.children(&mut cursor).collect();
            for i in 0..children.len().saturating_sub(1) {
                if children[i].kind() == "class"
                    && children[i + 1].kind() == "identifier"
                    && let Ok(name) = children[i + 1].utf8_text(bytes)
                {
                    *result = Some(Arc::from(name));
                }
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            dfs(child, offset, bytes, result);
        }
    }
    dfs(root, ctx.offset, ctx.bytes, &mut result);
    result
}
