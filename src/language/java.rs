use super::Language;
use super::ts_utils::{capture_text, run_query};
use crate::completion::context::CurrentClassMember;
use crate::completion::{
    CompletionContext,
    context::{CursorLocation, LocalVar},
};
use crate::language::rope_utils::rope_line_col_to_offset;
use crate::language::ts_utils::find_method_by_offset;
use ropey::Rope;
use std::sync::Arc;
use tracing::debug;
use tree_sitter::{Node, Parser, Query};

#[derive(Debug)]
pub struct JavaLanguage;

impl Language for JavaLanguage {
    fn id(&self) -> &'static str {
        "java"
    }

    fn supports(&self, language_id: &str) -> bool {
        language_id == "java"
    }

    fn make_parser(&self) -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        parser
    }

    fn parse_completion_context(
        &self,
        source: &str,
        line: u32,
        character: u32,
        trigger_char: Option<char>,
    ) -> Option<CompletionContext> {
        let rope = Rope::from_str(source);
        let offset = rope_line_col_to_offset(&rope, line, character)?;
        debug!(line, character, trigger = ?trigger_char, "java: parsing context");

        // Comment checking is performed once at the outermost layer and will not be repeated thereafter.
        if is_cursor_in_comment_with_rope(source, &rope, offset) {
            return Some(CompletionContext::new(
                CursorLocation::Unknown,
                "",
                vec![],
                None,
                None,
                None,
                vec![],
            ));
        }

        let mut parser = self.make_parser();
        let tree = parser.parse(source, None)?;
        let extractor = JavaContextExtractor::new_with_rope(source, offset, rope);
        Some(extractor.extract(tree.root_node(), trigger_char))
    }
}

struct JavaContextExtractor<'s> {
    source: &'s str,
    bytes: &'s [u8],
    offset: usize,
    rope: Rope,
}

impl<'s> JavaContextExtractor<'s> {
    #[cfg(test)]
    fn new(source: &'s str, offset: usize) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            offset,
            rope: Rope::from_str(source),
        }
    }

    fn new_with_rope(source: &'s str, offset: usize, rope: Rope) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            offset,
            rope,
        }
    }

    fn extract(self, root: Node, trigger_char: Option<char>) -> CompletionContext {
        let cursor_node = self.find_cursor_node(root);

        // A fallback comment node at the tree-sitter level
        if cursor_node
            .map(|n| is_comment_kind(n.kind()))
            .unwrap_or(false)
        {
            return self.empty_context();
        }

        let (location, query) = self.determine_location(cursor_node, trigger_char);
        let local_variables = self.extract_locals(root, cursor_node);

        let enclosing_class = self
            .extract_enclosing_class(cursor_node)
            .or_else(|| self.extract_enclosing_class_by_offset(root));
        let enclosing_package = self.extract_package(root);
        let enclosing_internal_name = build_internal_name(&enclosing_package, &enclosing_class);
        let existing_imports = self.extract_imports(root);

        let current_class_members = cursor_node
            .and_then(|n| find_ancestor(n, "class_declaration"))
            .and_then(|cls| cls.child_by_field_name("body"))
            .map(|body| self.extract_class_members_from_body(body))
            .unwrap_or_default();

        let enclosing_class_member = cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset))
            .and_then(|m| self.parse_method_node(m));

        let char_after_cursor = self.source[self.offset..]
            .chars()
            .find(|c| !(c.is_alphanumeric() || *c == '_'));

        CompletionContext::new(
            location,
            query,
            local_variables,
            enclosing_class,
            enclosing_internal_name,
            enclosing_package,
            existing_imports,
        )
        .with_class_members(current_class_members)
        .with_enclosing_member(enclosing_class_member)
        .with_char_after_cursor(char_after_cursor)
    }

    fn find_cursor_node<'tree>(&self, root: Node<'tree>) -> Option<Node<'tree>> {
        // 优先：[offset-1, offset)
        if let Some(n) =
            root.named_descendant_for_byte_range(self.offset.saturating_sub(1), self.offset)
            && !is_comment_kind(n.kind())
            && n.end_byte() >= self.offset
        {
            return Some(n);
        }
        // Back: [offset, offset+1), handles the case at the beginning of the line.
        if self.offset < self.bytes.len()
            && let Some(n) = root.named_descendant_for_byte_range(self.offset, self.offset + 1)
            && !is_comment_kind(n.kind())
        {
            return Some(n);
        }

        None
    }

    fn determine_location(
        &self,
        cursor_node: Option<Node>,
        trigger_char: Option<char>,
    ) -> (CursorLocation, String) {
        let node = match cursor_node {
            Some(n) => n,
            None => return self.fallback_location(trigger_char),
        };

        let mut current = node;
        loop {
            match current.kind() {
                "marker_annotation" | "annotation" => {
                    let name_node = current.child_by_field_name("name");
                    let prefix = name_node
                        .map(|n| self.cursor_truncated_text(n))
                        .unwrap_or_default();
                    return (
                        CursorLocation::Annotation {
                            prefix: prefix.clone(),
                        },
                        prefix,
                    );
                }
                "import_declaration" => return self.handle_import(current),
                "method_invocation" => return self.handle_member_access(current),
                "field_access" => return self.handle_member_access(current),
                "object_creation_expression" => return self.handle_constructor(current),
                "argument_list" => return self.handle_argument_list(current),
                "identifier" | "type_identifier" => {
                    return self.handle_identifier(current, trigger_char);
                }
                _ => {}
            }
            match current.parent() {
                Some(p) => current = p,
                None => break,
            }
        }
        self.fallback_location(trigger_char)
    }

    fn handle_identifier(
        &self,
        node: Node,
        _trigger_char: Option<char>,
    ) -> (CursorLocation, String) {
        let mut ancestor = node;
        loop {
            ancestor = match ancestor.parent() {
                Some(p) => p,
                None => break,
            };
            match ancestor.kind() {
                "field_access" | "method_invocation" => {
                    return self.handle_member_access(ancestor);
                }
                "import_declaration" => return self.handle_import(ancestor),
                "object_creation_expression" => return self.handle_constructor(ancestor),
                "local_variable_declaration" => {
                    if self.is_in_type_position(node, ancestor) {
                        let text = self.cursor_truncated_text(node);
                        return (
                            CursorLocation::TypeAnnotation {
                                prefix: text.clone(),
                            },
                            text,
                        );
                    }
                    if self.is_in_name_position(node, ancestor) {
                        return (CursorLocation::Unknown, String::new());
                    }
                    let text = self.cursor_truncated_text(node);
                    return self.maybe_member_access_from_text(text);
                }
                "argument_list" => {
                    // If the cursor is before '(', it will complete the method name instead of the parameters.
                    if let Some(parent) = node.parent()
                        && parent.kind() == "method_invocation"
                        && self.offset <= ancestor.start_byte()
                    {
                        return self.handle_member_access(parent);
                    }
                    return self.handle_argument_list(ancestor);
                }
                "block" | "class_body" | "program" => break,
                _ => {}
            }
        }
        let text = self.cursor_truncated_text(node);
        self.maybe_member_access_from_text(text)
    }

    fn handle_argument_list(&self, _node: Node) -> (CursorLocation, String) {
        let prefix = self.current_line_prefix_trimmed();
        let prefix = extract_arg_prefix(prefix);
        (
            CursorLocation::MethodArgument {
                prefix: prefix.clone(),
            },
            prefix,
        )
    }

    fn fallback_location(&self, _trigger_char: Option<char>) -> (CursorLocation, String) {
        let current_line = self.current_line_prefix_trimmed();

        if is_in_line_comment(current_line) {
            return (CursorLocation::Unknown, String::new());
        }
        if let Some(result) = self.parse_last_line(current_line) {
            return result;
        }
        let query = current_line
            .split(|c: char| !c.is_alphanumeric() && c != '_')
            .next_back()
            .unwrap_or("")
            .to_string();

        // Annotation
        let prefix_len = current_line.len().saturating_sub(query.len());
        if current_line[..prefix_len].trim_end().ends_with('@') {
            return (
                CursorLocation::Annotation {
                    prefix: query.clone(),
                },
                query,
            );
        }

        (
            CursorLocation::Expression {
                prefix: query.clone(),
            },
            query,
        )
    }

    /// The content from the beginning of the current line to the cursor (after trimming).
    fn current_line_prefix_trimmed(&self) -> &str {
        let line_idx = self
            .rope
            .byte_to_line(self.offset.min(self.source.len().saturating_sub(1)));
        let line_byte_start = self.rope.line_to_byte(line_idx);
        let safe_offset = self.offset.max(line_byte_start).min(self.source.len());
        self.source[line_byte_start..safe_offset].trim()
    }

    fn maybe_member_access_from_text(&self, text: String) -> (CursorLocation, String) {
        let line = self.current_line_prefix_trimmed();
        if let Some(result) = self.parse_last_line(line) {
            return result;
        }

        // Annotation
        let prefix_len = line.len().saturating_sub(text.len());
        if line[..prefix_len].trim_end().ends_with('@') {
            return (
                CursorLocation::Annotation {
                    prefix: text.clone(),
                },
                text,
            );
        }

        (
            CursorLocation::Expression {
                prefix: text.clone(),
            },
            text,
        )
    }

    fn cursor_truncated_text(&self, node: Node) -> String {
        let start = node.start_byte();
        let end = node.end_byte().min(self.offset);
        if end <= start {
            return String::new();
        }
        self.source[start..end].to_string()
    }

    fn node_text(&self, node: Node) -> &str {
        node.utf8_text(self.bytes).unwrap_or("")
    }

    fn empty_context(&self) -> CompletionContext {
        CompletionContext::new(
            CursorLocation::Unknown,
            "",
            vec![],
            None,
            None,
            None,
            vec![],
        )
    }

    fn parse_last_line(&self, last_line: &str) -> Option<(CursorLocation, String)> {
        // constructor: "new Foo" / "new "
        let stmt_fragment = last_line
            .rsplit([';', '{', '='])
            .next()
            .unwrap_or(last_line)
            .trim();

        let new_rest = stmt_fragment.strip_prefix("new ").or_else(|| {
            if stmt_fragment == "new" {
                Some("")
            } else {
                None
            }
        });

        if let Some(rest) = new_rest
            && !has_chained_dot(rest)
        {
            let class_prefix = rest.split('(').next().unwrap_or(rest).trim().to_string();
            let expected_type = self.infer_expected_type_from_text(last_line);
            return Some((
                CursorLocation::ConstructorCall {
                    class_prefix: class_prefix.clone(),
                    expected_type,
                },
                class_prefix,
            ));
        }

        // dot access
        if let Some(dot_pos) = last_meaningful_dot(last_line) {
            let receiver = last_line[..dot_pos].trim();
            let member_pfx = last_line[dot_pos + 1..].to_string();
            let is_type = receiver.chars().next().is_some_and(|c| c.is_uppercase());
            return Some(if is_type {
                (
                    CursorLocation::StaticAccess {
                        class_internal_name: Arc::from(receiver.replace('.', "/").as_str()),
                        member_prefix: member_pfx.clone(),
                    },
                    member_pfx,
                )
            } else {
                (
                    CursorLocation::MemberAccess {
                        receiver_type: None,
                        member_prefix: member_pfx.clone(),
                        receiver_expr: receiver.to_string(),
                    },
                    member_pfx,
                )
            });
        }
        None
    }

    fn handle_import(&self, node: Node) -> (CursorLocation, String) {
        let text = self.node_text(node);
        let prefix = text
            .trim_start_matches("import")
            .trim()
            .trim_end_matches(';')
            .trim()
            .to_string();
        let query = prefix.rsplit('.').next().unwrap_or("").to_string();
        (CursorLocation::Import { prefix }, query)
    }

    fn handle_member_access(&self, node: Node) -> (CursorLocation, String) {
        let mut walker = node.walk();
        let children: Vec<Node> = node.children(&mut walker).collect();

        let dot_pos = match children.iter().position(|n| n.kind() == ".") {
            Some(p) => p,
            None => {
                let name_node = children
                    .iter()
                    .find(|n| n.kind() == "identifier" || n.kind() == "type_identifier");
                let text = name_node
                    .map(|n| self.cursor_truncated_text(*n))
                    .unwrap_or_default();
                return (
                    CursorLocation::Expression {
                        prefix: text.clone(),
                    },
                    text,
                );
            }
        };

        let member_node = children[dot_pos + 1..]
            .iter()
            .find(|n| n.kind() == "identifier" || n.kind() == "type_identifier");

        let member_prefix = match member_node {
            Some(mn) => {
                let s = mn.start_byte();
                let e = mn.end_byte();
                if self.offset <= s {
                    String::new()
                } else if self.offset < e {
                    self.source[s..self.offset].to_string()
                } else {
                    self.node_text(*mn).to_string()
                }
            }
            None => {
                let line = self.current_line_prefix_trimmed();
                last_meaningful_dot(line)
                    .map(|p| line[p + 1..].to_string())
                    .unwrap_or_default()
            }
        };

        let receiver_node = if dot_pos > 0 {
            Some(children[dot_pos - 1])
        } else {
            None
        };
        let receiver_expr = receiver_node
            .map(|n| self.node_text(n).to_string())
            .unwrap_or_default();

        if let Some(recv) = receiver_node {
            let recv_text = self.node_text(recv);
            if recv_text.chars().next().is_some_and(|c| c.is_uppercase())
                && node.kind() == "field_access"
            {
                return (
                    CursorLocation::StaticAccess {
                        class_internal_name: Arc::from(recv_text.replace('.', "/").as_str()),
                        member_prefix: member_prefix.clone(),
                    },
                    member_prefix,
                );
            }
        }

        (
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: member_prefix.clone(),
                receiver_expr,
            },
            member_prefix,
        )
    }

    fn handle_constructor(&self, node: Node) -> (CursorLocation, String) {
        let class_prefix = node
            .child_by_field_name("type")
            .map(|n| self.node_text(n).to_string())
            .unwrap_or_default();
        let expected_type = self.infer_expected_type_from_lhs(node);
        (
            CursorLocation::ConstructorCall {
                class_prefix: class_prefix.clone(),
                expected_type,
            },
            class_prefix,
        )
    }

    fn infer_expected_type_from_lhs(&self, node: Node) -> Option<String> {
        let decl = find_ancestor(node, "local_variable_declaration")?;
        let mut walker = decl.walk();
        for child in decl.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            let ty = self.node_text(child);
            let simple = ty.split('<').next()?.trim();
            if !simple.is_empty() {
                return Some(simple.to_string());
            }
        }
        None
    }

    fn infer_expected_type_from_text(&self, line: &str) -> Option<String> {
        let lhs = line.split('=').next()?.trim();
        let first_token = lhs.split_whitespace().next()?;
        let simple = first_token.split('<').next()?.trim();
        if simple.chars().next().is_some_and(|c| c.is_uppercase()) {
            Some(simple.to_string())
        } else {
            None
        }
    }

    fn is_in_name_position(&self, id_node: Node, decl_node: Node) -> bool {
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

    fn is_in_type_position(&self, id_node: Node, decl_node: Node) -> bool {
        let mut walker = decl_node.walk();
        for child in decl_node.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            return child.id() == id_node.id();
        }
        false
    }

    fn extract_locals(&self, root: Node, cursor_node: Option<Node>) -> Vec<LocalVar> {
        let search_root = cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset))
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
                debug!("local var query error: {}", e);
                return vec![];
            }
        };
        let type_idx = q.capture_index_for_name("type").unwrap();
        let name_idx = q.capture_index_for_name("name").unwrap();

        let mut vars: Vec<LocalVar> = run_query(&q, search_root, self.bytes, None)
            .into_iter()
            .filter_map(|captures| {
                let ty_node = captures.iter().find(|(idx, _)| *idx == type_idx)?.1;
                let name_node = captures.iter().find(|(idx, _)| *idx == name_idx)?.1;
                if ty_node.start_byte() >= self.offset {
                    return None;
                }

                let ty = ty_node.utf8_text(self.bytes).ok()?;
                let name = name_node.utf8_text(self.bytes).ok()?;
                debug!(
                    ty,
                    name,
                    start = ty_node.start_byte(),
                    offset = self.offset,
                    "extracted local var"
                );

                let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
                if raw_ty == "var" {
                    return Some(match infer_type_from_initializer(ty_node, self.bytes) {
                        Some(t) => LocalVar {
                            name: Arc::from(name),
                            type_internal: Arc::from(java_type_to_internal(&t).as_str()),
                            init_expr: None,
                        },
                        None => LocalVar {
                            name: Arc::from(name),
                            type_internal: Arc::from("var"),
                            init_expr: get_initializer_text(ty_node, self.bytes),
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

        vars.extend(self.extract_params(root, cursor_node));
        vars
    }

    fn extract_params(&self, root: Node, cursor_node: Option<Node>) -> Vec<LocalVar> {
        let method = match cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset))
        {
            Some(m) => m,
            None => return vec![],
        };
        let query_src = r#"
            (formal_parameter type: (_) @type name: (identifier) @name)
        "#;
        let q = match Query::new(&tree_sitter_java::LANGUAGE.into(), query_src) {
            Ok(q) => q,
            Err(_) => return vec![],
        };
        let type_idx = q.capture_index_for_name("type").unwrap();
        let name_idx = q.capture_index_for_name("name").unwrap();

        run_query(&q, method, self.bytes, None)
            .into_iter()
            .filter_map(|captures| {
                let ty = capture_text(&captures, type_idx, self.bytes)?;
                let name = capture_text(&captures, name_idx, self.bytes)?;
                let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
                Some(LocalVar {
                    name: Arc::from(name),
                    type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
                    init_expr: None,
                })
            })
            .collect()
    }

    fn extract_enclosing_class(&self, cursor_node: Option<Node>) -> Option<Arc<str>> {
        let class_node = cursor_node.and_then(|n| find_ancestor(n, "class_declaration"))?;
        let name_node = class_node.child_by_field_name("name")?;
        Some(Arc::from(self.node_text(name_node)))
    }

    fn extract_enclosing_class_by_offset(&self, root: Node) -> Option<Arc<str>> {
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
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                dfs(child, offset, bytes, result);
            }
        }
        dfs(root, self.offset, self.bytes, &mut result);
        result
    }

    fn extract_imports(&self, root: Node) -> Vec<String> {
        let q = match Query::new(
            &tree_sitter_java::LANGUAGE.into(),
            r#"(import_declaration) @import"#,
        ) {
            Ok(q) => q,
            Err(_) => return vec![],
        };
        let idx = q.capture_index_for_name("import").unwrap();
        run_query(&q, root, self.bytes, None)
            .into_iter()
            .filter_map(|caps| {
                let text = capture_text(&caps, idx, self.bytes)?;
                let cleaned = text
                    .trim_start_matches("import")
                    .trim()
                    .trim_end_matches(';')
                    .trim()
                    .to_string();
                if cleaned.is_empty() {
                    None
                } else {
                    Some(cleaned)
                }
            })
            .collect()
    }

    fn extract_package(&self, root: Node) -> Option<Arc<str>> {
        let q = Query::new(
            &tree_sitter_java::LANGUAGE.into(),
            r#"(package_declaration) @pkg"#,
        )
        .ok()?;
        let idx = q.capture_index_for_name("pkg")?;
        let results = run_query(&q, root, self.bytes, None);
        let text = results
            .first()
            .and_then(|caps| capture_text(caps, idx, self.bytes))?;
        let pkg = text
            .trim_start_matches("package")
            .trim()
            .trim_end_matches(';')
            .trim()
            .replace('.', "/");
        Some(Arc::from(pkg.as_str()))
    }

    /// Extract all members (methods + fields) from class_body
    fn extract_class_members_from_body(&self, body: Node) -> Vec<CurrentClassMember> {
        let mut members = Vec::new();
        let mut cursor = body.walk();
        for child in body.children(&mut cursor) {
            match child.kind() {
                "method_declaration" => {
                    if let Some(m) = self.parse_method_node(child) {
                        members.push(m);
                    }
                }
                "field_declaration" => {
                    members.extend(self.parse_field_node(child));
                }
                _ => {}
            }
        }
        members
    }

    /// parse a single method_declaration node -> CurrentClassMember
    fn parse_method_node(&self, node: Node) -> Option<CurrentClassMember> {
        let mut name: Option<&str> = None;
        let mut is_static = false;
        let mut is_private = false;
        let mut ret_type = "void";
        let mut params = "()";

        let mut wc = node.walk();
        for c in node.children(&mut wc) {
            match c.kind() {
                "modifiers" => {
                    let t = self.node_text(c);
                    is_static = t.contains("static");
                    is_private = t.contains("private");
                }
                "identifier" if name.is_none() => {
                    name = Some(self.node_text(c));
                }
                "void_type"
                | "integral_type"
                | "floating_point_type"
                | "boolean_type"
                | "type_identifier"
                | "array_type"
                | "generic_type" => {
                    ret_type = self.node_text(c);
                }
                "formal_parameters" => {
                    params = self.node_text(c);
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

    /// Parse a single field_declaration node -> Vec<CurrentClassMember> (multiple fields can be declared on one line)
    fn parse_field_node(&self, node: Node) -> Vec<CurrentClassMember> {
        let mut is_static = false;
        let mut is_private = false;
        let mut field_type = "Object";
        let mut names = Vec::new();

        let mut wc = node.walk();
        for c in node.children(&mut wc) {
            match c.kind() {
                "modifiers" => {
                    let t = self.node_text(c);
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
                    field_type = self.node_text(c);
                }
                "variable_declarator" => {
                    let mut vc = c.walk();
                    for vchild in c.children(&mut vc) {
                        if vchild.kind() == "identifier" {
                            names.push(self.node_text(vchild).to_string());
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
}

/// Internal name construction: package + class → "pkg/Class
fn build_internal_name(package: &Option<Arc<str>>, class: &Option<Arc<str>>) -> Option<Arc<str>> {
    match (package, class) {
        (Some(pkg), Some(cls)) => Some(Arc::from(format!("{}/{}", pkg, cls).as_str())),
        (None, Some(cls)) => Some(Arc::clone(cls)),
        _ => None,
    }
}

/// Is the node kind a comment?
fn is_comment_kind(kind: &str) -> bool {
    kind == "line_comment" || kind == "block_comment"
}

fn find_ancestor<'a>(mut node: Node<'a>, kind: &str) -> Option<Node<'a>> {
    loop {
        node = node.parent()?;
        if node.kind() == kind {
            return Some(node);
        }
    }
}

fn last_meaningful_dot(s: &str) -> Option<usize> {
    let mut depth = 0i32;
    let mut last = None;
    for (i, c) in s.char_indices() {
        match c {
            '(' | '[' | '<' => depth += 1,
            ')' | ']' | '>' => depth -= 1,
            '.' if depth == 0 => last = Some(i),
            _ => {}
        }
    }
    last
}

/// "foo(arg1, aV" → "aV"
fn extract_arg_prefix(last_line: &str) -> String {
    let mut depth = 0i32;
    let mut last_open = None;
    for (i, c) in last_line.char_indices() {
        match c {
            '(' => {
                depth += 1;
                if depth == 1 {
                    last_open = Some(i);
                }
            }
            ')' => {
                depth -= 1;
            }
            _ => {}
        }
    }
    let inside = match last_open {
        Some(pos) if depth > 0 => &last_line[pos + 1..],
        _ => last_line,
    };
    inside
        .rfind(',')
        .map(|p| &inside[p + 1..])
        .unwrap_or(inside)
        .trim()
        .to_string()
}

/// Returns true if `rest` (the part after "new ") contains a method chain dot.
fn has_chained_dot(rest: &str) -> bool {
    let mut depth = 0i32;
    let mut after_close = false;
    for c in rest.chars() {
        match c {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    after_close = true;
                }
            }
            '.' if after_close && depth == 0 => return true,
            _ => {}
        }
    }
    false
}

fn get_initializer_text(type_node: Node, bytes: &[u8]) -> Option<String> {
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

fn infer_type_from_initializer(type_node: Node, bytes: &[u8]) -> Option<String> {
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

/// Check if offset is inside a single-line comment (//) or a block comment (/* */).
pub fn is_cursor_in_comment_with_rope(source: &str, rope: &Rope, offset: usize) -> bool {
    let before = &source[..offset];

    // Block comment 检测（维持原状）
    let last_open = before.rfind("/*");
    let last_close = before.rfind("*/");
    if let Some(open) = last_open {
        match last_close {
            None => return true,
            Some(close) if open > close => return true,
            _ => {}
        }
    }

    // 单行注释检测：用 Rope 实现 O(log n) 行首定位
    let line_idx = rope.byte_to_line(offset.min(source.len().saturating_sub(1)));
    let line_byte_start = rope.line_to_byte(line_idx);
    let safe_offset = offset.max(line_byte_start).min(source.len());

    is_in_line_comment(&source[line_byte_start..safe_offset])
}

pub fn is_cursor_in_comment(source: &str, offset: usize) -> bool {
    let rope = Rope::from_str(source);
    is_cursor_in_comment_with_rope(source, &rope, offset)
}

/// Check if a line of text (the part before the cursor) is in a comment.
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
            '\\' => {
                escaped = true;
            }
            '"' if !in_char => {
                in_string = !in_string;
            }
            '\'' if !in_string => {
                in_char = !in_char;
            }
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
    match ty {
        "byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char" | "void" => {
            ty.to_string()
        }
        other => other.replace('.', "/"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{completion::context::CursorLocation, language::rope_utils::line_col_to_offset};

    fn at(src: &str, line: u32, col: u32) -> CompletionContext {
        JavaLanguage
            .parse_completion_context(src, line, col, None)
            .unwrap()
    }

    fn end_of(src: &str) -> CompletionContext {
        let lines: Vec<&str> = src.lines().collect();
        let line = (lines.len() - 1) as u32;
        let col = lines.last().unwrap().len() as u32;
        at(src, line, col)
    }

    #[test]
    fn test_import() {
        let src = "import com.example.Foo;";
        let ctx = end_of(src);
        assert!(matches!(ctx.location, CursorLocation::Import { .. }));
    }

    #[test]
    fn test_member_access() {
        // "class A { void f() { someList.get; } }\n"
        //  "someList.get" starts at byte 21, "get" at byte 30..33
        //  col = 33 - start_of_line(0) = 33
        let src = "class A { void f() { someList.get; } }\n";
        let ctx = at(src, 0, 33);
        assert!(
            matches!(ctx.location, CursorLocation::MemberAccess { .. }),
            "{:?}",
            ctx.location
        );
        if let CursorLocation::MemberAccess { member_prefix, .. } = &ctx.location {
            assert_eq!(member_prefix, "get");
        }
    }

    #[test]
    fn test_constructor() {
        let src = indoc::indoc! {r#"
            class A {
                void f() { new ArrayList() }
            }
        "#};
        let line = 1u32;
        let col = src.lines().nth(1).unwrap().find("ArrayList").unwrap() as u32 + 9;
        let ctx = at(src, line, col);
        assert!(
            matches!(ctx.location, CursorLocation::ConstructorCall { .. }),
            "{:?}",
            ctx.location
        );
    }

    #[test]
    fn test_local_var_extracted() {
        let src = indoc::indoc! {r#"
            class A {
                void f() {
                    List<String> items = new ArrayList<>();
                    items.get
                }
            }
        "#};
        let line = 3u32;
        let col = src.lines().nth(3).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "items"),
            "locals: {:?}",
            ctx.local_variables
        );
    }

    #[test]
    fn test_params_extracted() {
        let src = indoc::indoc! {r#"
            class A {
                void process(String input, int count) {
                    input.length
                }
            }
        "#};
        let line = 2u32;
        let col = src.lines().nth(2).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "input")
        );
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "count")
        );
    }

    #[test]
    fn test_enclosing_class() {
        let src = indoc::indoc! {r#"
            class MyService {
                void foo() { this.bar }
            }
        "#};
        let line = 1u32;
        let col = src.lines().nth(1).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert_eq!(ctx.enclosing_class.as_deref(), Some("MyService"));
    }

    #[test]
    fn test_imports_extracted() {
        let src = indoc::indoc! {r#"
            import java.util.List;
            import java.util.Map;
            class A { void f() {} }
        "#};
        let ctx = at(src, 2, 10);
        assert!(ctx.existing_imports.iter().any(|i| i.contains("List")));
        assert!(ctx.existing_imports.iter().any(|i| i.contains("Map")));
    }

    #[test]
    fn test_this_dot_no_semicolon_member_prefix_empty() {
        // No semicolon or subsequent text follows `this.`, the cursor immediately follows the dot.
        // `member_prefix` should be an empty string, `location` should be `MemberAccess`.
        let src = indoc::indoc! {r#"
        class A {
            private void priFunc() {}
            void fun() {
                this.
            }
        }
    "#};
        // The cursor is after the dot "this."
        let line = 3u32;
        let raw_line = src.lines().nth(3).unwrap();
        let col = raw_line.find("this.").unwrap() as u32 + 5; // after the dot (".")
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "this"
            ),
            "expected MemberAccess{{receiver_expr=this, member_prefix=}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_this_dot_cursor_before_existing_identifier() {
        // `this.|fun()` — cursor after the dot and before fun
        // member_prefix should be an empty string
        let src = indoc::indoc! {r#"
        class A {
            private void priFunc() {}
            void fun() {
                this.fun()
            }
        }
    "#};
        let line = 3u32;
        let raw_line = src.lines().nth(3).unwrap();
        // The cursor is after "this." and before "fun".
        let col = raw_line.find("this.").unwrap() as u32 + 5;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "this"
            ),
            "cursor before 'fun': member_prefix should be empty, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_this_dot_cursor_mid_identifier() {
        // `this.fu|n()` — cursor is in the middle of fun
        // member_prefix should be "fu"
        let src = indoc::indoc! {r#"
        class A {
            void fun() {
                this.fun()
            }
        }
    "#};
        let line = 2u32;
        let raw_line = src.lines().nth(2).unwrap();
        // Starting column of fun
        let fun_col = raw_line.find("fun").unwrap() as u32;
        // The cursor is after fu and before n.
        let col = fun_col + 2;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix == "fu" && receiver_expr == "this"
            ),
            "cursor mid-identifier: member_prefix should be 'fu', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_this_dot_with_semicolon_member_prefix_empty() {
        // `this.|;` — There is a semicolon, the cursor is after the dot.
        let src = indoc::indoc! {r#"
        class A {
            void fun() {
                this.;
            }
        }
    "#};
        let line = 2u32;
        let raw_line = src.lines().nth(2).unwrap();
        let col = raw_line.find("this.").unwrap() as u32 + 5;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "this"
            ),
            "this.|; should give empty member_prefix, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_expression_ma_finds_main_in_same_package() {
        // `Ma|` should be found in the same package as Main
        // Validate ExpressionProvider without filtering enclosing class
        let src = indoc::indoc! {r#"
        package org.cubewhy.a;
        class Main {
            void func() {
                Ma
            }
        }
    "#};
        let line = 3u32;
        let raw_line = src.lines().nth(3).unwrap();
        let col = raw_line.len() as u32;
        let ctx = at(src, line, col);
        // Verification shows that Expression{prefix="Ma"} has been parsed.
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix == "Ma"),
            "expected Expression{{Ma}}, got {:?}",
            ctx.location
        );
        // enclosing_class should be Main
        assert_eq!(ctx.enclosing_class.as_deref(), Some("Main"));
    }

    #[test]
    fn test_argument_prefix_truncated_at_cursor() {
        // When `println(var|)`, the cursor is in the middle of `aVar`, so the prefix should be the part before "var".
        // That is, in `println(aVar)`, the cursor is after "aV", so the prefix should be "aV".
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aVar = "test";
                System.out.println(aVar)
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        // Cursor after "aV" and before "ar"
        let col = raw.find("aVar").unwrap() as u32 + 2;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::MethodArgument { prefix } if prefix == "aV"),
            "expected MethodArgument{{aV}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_direct_call_with_paren_is_expression() {
        // fun|() — No receiver, should be parsed as an Expression, prefix = "fun"
        let src = indoc::indoc! {r#"
        class A {
            void fun() {}
            void test() {
                fun()
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        // Cursor at the end of "fun"
        let col = raw.find("fun").unwrap() as u32 + 3;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix == "fun"),
            "direct call fun() should give Expression{{fun}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_char_after_cursor_skips_identifier_chars() {
        // fun|() — The cursor is at the end of fun, skipping the first character after the identifier, which is '('.
        let src = indoc::indoc! {r#"
        class A {
            void fun() {}
            void test() {
                fun()
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.find("fun").unwrap() as u32 + 3;
        let ctx = at(src, line, col);
        assert!(
            ctx.has_paren_after_cursor(),
            "char after identifier should be '(', got {:?}",
            ctx.char_after_cursor
        );
    }

    #[test]
    fn test_this_method_paren_already_exists() {
        // this.priFunc() — The cursor is at the end of priFunc, followed by '('
        let src = indoc::indoc! {r#"
        class A {
            void fun() {
                this.priFunc()
            }
            private void priFunc() {}
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("priFunc").unwrap() as u32 + "priFunc".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.has_paren_after_cursor(),
            "after 'priFunc' should see '(', char_after_cursor={:?}",
            ctx.char_after_cursor
        );
    }

    #[test]
    fn test_empty_argument_list_is_method_argument() {
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                System.out.println()
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find('(').unwrap() as u32 + 1;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::MethodArgument { prefix } if prefix.is_empty()),
            "empty argument list should give MethodArgument{{prefix:\"\"}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_static_method_members_visible_in_static_context() {
        // In the main() static method, static methods of the same class should be recorded as is_static=true in enclosing_class_member.
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                pr
            }
            public static Object pri() { return null; }
        }
    "#};
        let line = 2u32;
        let col = src.lines().nth(2).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.is_in_static_context(),
            "main() should be static context"
        );
        assert!(
            ctx.current_class_members.contains_key("pri"),
            "pri should be in current_class_members"
        );
        let pri = ctx.current_class_members.get("pri").unwrap();
        assert!(pri.is_static, "pri() should be marked static");
    }

    #[test]
    fn test_var_init_expression_is_expression_location() {
        // `Object a = p|` — The initialization expression should be resolved to Expression{prefix:"p"}
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                Object a = p
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix == "p"),
            "var init expression should give Expression{{p}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_var_init_expression_with_prefix_a() {
        // `String aCopy = a|` — Initialization expression, prefix = "a"
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aVar = "test";
                String aCopy = a
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix == "a"),
            "var init expression should give Expression{{a}}, got {:?}",
            ctx.location
        );
        // Simultaneously verify that aVar is in a local variable
        assert!(
            ctx.local_variables
                .iter()
                .any(|v| v.name.as_ref() == "aVar"),
            "aVar should be in locals: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_var_name_position_is_unknown() {
        // `String aCopy` — The cursor should not be completed when the variable name aCopy is on it.
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                String aCopy
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        // Place the cursor in the middle of "aCopy" to ensure that the tree-sitter positions the cursor_node to the identifier "aCopy".
        let acopy_start = raw.find("aCopy").unwrap() as u32;
        let col = acopy_start + 2; // "aC|opy"

        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Unknown),
            "var name position should be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_empty_line_in_static_method_has_static_context() {
        // The blank line is inside main(), enclosing_class_member should be main (static).
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                
            }
            public static Object pri() { return null; }
        }
    "#};
        let line = 2u32;
        // Blank line, col = 0 or any position within the line
        let col = 4u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.is_in_static_context(),
            "empty line inside main() should have static context, enclosing_member={:?}",
            ctx.enclosing_class_member
        );
        assert!(
            ctx.current_class_members.contains_key("pri"),
            "pri should be indexed: {:?}",
            ctx.current_class_members.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_empty_line_static_context_has_pri_member() {
        // Verify the complete chain: blank line + static context + pri is a static member
        let src = indoc::indoc! {r#"
        class A {
            public static void main() {
                
            }
            public static Object pri() { return null; }
        }
    "#};
        let line = 2u32;
        let col = 4u32;
        let ctx = at(src, line, col);

        let pri = ctx.current_class_members.get("pri");
        assert!(pri.is_some(), "pri should be in current_class_members");
        assert!(pri.unwrap().is_static, "pri() should be marked as static");
    }

    #[test]
    fn test_member_access_before_paren_is_member_access() {
        // cl.|f() — Cursor after '.' and before 'f'
        let src = indoc::indoc! {r#"
        class A {
            void test() {
                RandomClass cl = new RandomClass();
                cl.f()
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.find("cl.").unwrap() as u32 + 3; // After the dot, before f
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "cl"
            ),
            "cl.|f() should give MemberAccess{{receiver=cl, prefix=}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_member_access_no_paren_cursor_after_dot() {
        // cl.| (without parentheses, cursor is placed directly after the dot)
        // fallback_location should be able to handle this
        let src = indoc::indoc! {r#"
        class A {
            void test() {
                RandomClass cl = new RandomClass();
                cl.
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.find("cl.").unwrap() as u32 + 3;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "cl"
            ),
            "cl.| should give MemberAccess, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_expected_type_string() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                String a = new ;
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        // cursor after "new "
        let col = raw.find("new ").unwrap() as u32 + 4;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { expected_type: Some(t), .. }
                if t == "String"
            ),
            "expected_type should be 'String', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_expected_type_generic_stripped() {
        // List<Integer> items = new |
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                List<Integer> items = new ;
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("new ").unwrap() as u32 + 4;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { expected_type: Some(t), .. }
                if t == "List"
            ),
            "expected_type should be 'List' (generics stripped), got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_no_expected_type_standalone() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new ;
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("new ").unwrap() as u32 + 4;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { expected_type: None, class_prefix, .. }
                if class_prefix.is_empty()
            ),
            "standalone 'new' should have ConstructorCall{{prefix:\"\", expected_type:None}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_expected_type_with_prefix() {
        // RandomClass rc = new RandomClass|
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                RandomClass rc = new RandomClass();
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("RandomClass(").unwrap() as u32 + "RandomClass".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { expected_type: Some(t), .. }
                if t == "RandomClass"
            ),
            "expected_type should be 'RandomClass', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_infer_type_from_text_simple() {
        let extractor = JavaContextExtractor::new("", 0);
        assert_eq!(
            extractor.infer_expected_type_from_text("    String a = new "),
            Some("String".to_string())
        );
    }

    #[test]
    fn test_infer_type_from_text_generic() {
        let extractor = JavaContextExtractor::new("", 0);
        assert_eq!(
            extractor.infer_expected_type_from_text("    List<Integer> items = new "),
            Some("List".to_string())
        );
    }

    #[test]
    fn test_infer_type_from_text_no_equals() {
        // "new " alone — no '=' → None
        let extractor = JavaContextExtractor::new("", 0);
        assert_eq!(
            extractor.infer_expected_type_from_text("        new "),
            None
        );
    }

    #[test]
    fn test_infer_type_from_text_lowercase_lhs() {
        // "int a = new " — "int" starts lowercase → None
        let extractor = JavaContextExtractor::new("", 0);
        assert_eq!(
            extractor.infer_expected_type_from_text("int a = new "),
            None
        );
    }

    #[test]
    fn test_infer_type_from_text_var_keyword() {
        // "var a = new " — "var" starts lowercase → None
        let extractor = JavaContextExtractor::new("", 0);
        assert_eq!(
            extractor.infer_expected_type_from_text("var a = new "),
            None
        );
    }

    #[test]
    fn test_infer_type_from_text_multiword_lhs() {
        // "final String a = new " — first token is "final" (lowercase) → None
        // ... but "final String" means we should skip modifiers
        // Current impl returns None for this — acceptable limitation,
        // tree-sitter path (infer_expected_type_from_lhs) handles it correctly.
        let extractor = JavaContextExtractor::new("", 0);
        // "final" is lowercase → None from text heuristic
        assert_eq!(
            extractor.infer_expected_type_from_text("final String a = new "),
            None
        );
    }

    #[test]
    fn test_new_chain_dot_is_member_access() {
        // new RandomClass().| → MemberAccess{receiver="new RandomClass()", prefix=""}
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new RandomClass().
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col =
            raw.find("new RandomClass().").unwrap() as u32 + "new RandomClass().".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, member_prefix, .. }
                if receiver_expr == "new RandomClass()" && member_prefix.is_empty()
            ),
            "new Foo().| should be MemberAccess, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_new_chain_dot_with_prefix_is_member_access() {
        // new RandomClass().fu| → MemberAccess{receiver="new RandomClass()", prefix="fu"}
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new RandomClass().fu
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find(".fu").unwrap() as u32 + 3;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, member_prefix, .. }
                if receiver_expr == "new RandomClass()" && member_prefix == "fu"
            ),
            "new Foo().fu| should be MemberAccess{{prefix=fu}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_new_without_dot_still_constructor() {
        // new RandomClass| -> ConstructorCall (not affected by fix)
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new RandomClass
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("new RandomClass").unwrap() as u32 + "new RandomClass".len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { class_prefix, .. }
                if class_prefix == "RandomClass"
            ),
            "new Foo| should still be ConstructorCall, got {:?}",
            ctx.location
        );
    }

    // has_chained_dot unit tests
    #[test]
    fn test_has_chained_dot_with_dot() {
        assert!(has_chained_dot("Foo()."));
        assert!(has_chained_dot("Foo(a, b)."));
    }

    #[test]
    fn test_has_chained_dot_without_dot() {
        assert!(!has_chained_dot("Foo()"));
        assert!(!has_chained_dot("Foo"));
        assert!(!has_chained_dot(""));
    }

    #[test]
    fn test_has_chained_dot_nested_parens() {
        // "Foo(bar()).baz" — dot after outer close paren
        assert!(has_chained_dot("Foo(bar())."));
        // "Foo(bar())" — no dot after outer close
        assert!(!has_chained_dot("Foo(bar())"));
    }

    #[test]
    fn test_var_type_inferred_from_constructor() {
        let src = indoc::indoc! {r#"
        class Main {
            public static class NestedClass {
                public void randomFunction(String arg1) {}
            }
            public static void main(String[] args) {
                var nc = new NestedClass();
                nc.
            }
        }
    "#};

        // Find line/col dynamically instead of hardcoding
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("nc.").map(|c| (i as u32, c as u32 + 3)))
            .expect("nc. not found in source");

        let ctx = at(src, line, col);

        let nc = ctx.local_variables.iter().find(|v| v.name.as_ref() == "nc");
        assert!(
            nc.is_some(),
            "nc should be in locals: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| format!("{}:{}", v.name, v.type_internal))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            nc.unwrap().type_internal.as_ref(),
            "NestedClass",
            "var type should be inferred as NestedClass"
        );
    }

    #[test]
    fn test_var_with_non_constructor_init_skipped() {
        // var x = someMethod() — cannot infer, should not crash
        let src = indoc::indoc! {r#"
        class A {
            static String helper() { return ""; }
            void f() {
                var x = helper();
                x.
            }
        }
    "#};
        let line = 4u32;
        let raw = src.lines().nth(4).unwrap();
        let col = raw.find("x.").unwrap() as u32 + 2;
        let ctx = at(src, line, col);
        // x should not appear (type unknown), but should not crash
        // The important thing is no panic
        let _ = ctx;
    }

    #[test]
    fn test_bare_method_call_no_semicolon() {
        // getMain2().| without semicolon
        let src = indoc::indoc! {r#"
        class Main {
            public static void main(String[] args) {
                getMain2().
            }
            private static Object getMain2() { return null; }
        }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("getMain2().").map(|c| (i as u32, c as u32 + 11)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, member_prefix, .. }
                if receiver_expr == "getMain2()" && member_prefix.is_empty()
            ),
            "getMain2().| should be MemberAccess, got {:?}",
            ctx.location
        );
        // enclosing_internal_name must be set for bare method call resolution
        assert!(
            ctx.enclosing_internal_name.is_some(),
            "enclosing_internal_name should be set even without semicolon"
        );
        assert_eq!(ctx.enclosing_class.as_deref(), Some("Main"));
    }

    #[test]
    fn test_cursor_in_line_comment_returns_unknown() {
        let src = "// some random comments\n\npublic class ExampleClass {}";
        // Cursor at the end of the comment line
        let col = "// some random comments".len() as u32;
        let ctx = JavaLanguage
            .parse_completion_context(src, 0, col, None)
            .unwrap();
        assert!(
            matches!(ctx.location, CursorLocation::Unknown),
            "cursor inside line comment should give Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_cursor_on_empty_line_after_comment_is_expression() {
        let src = "// some random comments\n\npublic class ExampleClass {}";
        // The cursor is on line 1 (empty line).
        let ctx = JavaLanguage
            .parse_completion_context(src, 1, 0, None)
            .unwrap();
        assert!(
            !matches!(ctx.location, CursorLocation::Unknown),
            "empty line after comment should not be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_empty_line_after_comment_not_comment_context() {
        let src = indoc::indoc! {r#"
        // some random comments
        
        public class ExampleClass {}
    "#};
        let ctx = at(src, 1, 0);
        assert!(
            !matches!(&ctx.location,
            CursorLocation::Expression { prefix } if prefix == "comments"),
            "empty line after comment should not have prefix='comments', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_cursor_inside_line_comment_is_unknown() {
        let src = "// some random comments";
        let col = src.len() as u32;
        let ctx = at(src, 0, col);
        assert!(
            matches!(ctx.location, CursorLocation::Unknown),
            "cursor inside line comment should be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_cursor_at_start_of_line_comment_is_unknown() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                // comment here
            }
        }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.find("//").unwrap() as u32 + 5;
        let ctx = at(src, line, col);
        assert!(
            matches!(ctx.location, CursorLocation::Unknown),
            "cursor inside // comment should be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_empty_line_after_comment_is_not_unknown() {
        // A blank line immediately following a comment line, with the cursor on the blank line, should not be interpreted as a comment.
        let src = "// some random comments\n\npublic class ExampleClass {}";
        // line=1 is a blank line
        let ctx = at(src, 1, 0);
        assert!(
            !matches!(ctx.location, CursorLocation::Unknown),
            "empty line after comment should NOT be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_cursor_inside_comment_is_unknown() {
        let src = "// some random comments\n\npublic class ExampleClass {}";
        // line=0, col is in the middle of the comment
        let col = "// some random".len() as u32;
        let ctx = at(src, 0, col);
        assert!(
            matches!(ctx.location, CursorLocation::Unknown),
            "cursor inside comment should be Unknown, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_line_col_to_offset_multibyte() {
        // 含 CJK 字符（UTF-8 3字节，UTF-16 1单元）
        let src = "你好\nworld";
        // line=0, character=2 → 第3个UTF-16单元 = 6字节
        assert_eq!(line_col_to_offset(src, 0, 2), Some(6));
        // line=1, character=3 → "wor" = 3字节
        assert_eq!(line_col_to_offset(src, 1, 3), Some(6 + 1 + 3)); // \n=1, wor=3
    }

    #[test]
    fn test_line_col_to_offset_out_of_bounds() {
        let src = "hello\nworld";
        assert_eq!(line_col_to_offset(src, 5, 0), None);
    }

    #[test]
    fn test_current_line_prefix_uses_rope() {
        // 验证多行文件中 current_line_prefix_trimmed 正确定位
        let src = "class A {\n    void f() {\n        items\n    }\n}\n";
        let offset = line_col_to_offset(src, 2, 13).unwrap(); // 末尾 's' 之后
        let extractor = JavaContextExtractor::new(src, offset);
        let prefix = extractor.current_line_prefix_trimmed();
        assert_eq!(prefix, "items");
    }
}
