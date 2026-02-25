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

const SENTINEL: &str = "__KIRO__";

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

        if cursor_node
            .map(|n| is_comment_kind(n.kind()))
            .unwrap_or(false)
        {
            return self.empty_context();
        }

        let (location, query) = self.determine_location(cursor_node, trigger_char);

        // 如果 AST 解析失败，走注入路径
        let (location, query) = if matches!(location, CursorLocation::Unknown) {
            self.inject_and_determine(cursor_node, trigger_char)
                .unwrap_or((location, query))
        } else {
            (location, query)
        };

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
            .or_else(|| {
                // Fallback: find top-level ERROR node anywhere under program
                let error_node = find_top_error_node(root)?;
                let mut members = Vec::new();
                self.collect_members_from_node(error_node, &mut members);
                let snapshot = members.clone();
                members.extend(self.parse_partial_methods_from_error(error_node, &snapshot));
                Some(members)
            })
            .unwrap_or_default();
        let enclosing_class_member = cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset))
            .or_else(|| find_enclosing_method_in_error(root, self.offset))
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

    /// 构造注入后的 source：用 SENTINEL 替换光标处的 token（或直接插入）
    fn build_injected_source(&self, cursor_node: Option<Node>) -> String {
        let before = &self.source[..self.offset];
        let trimmed = before.trim_end();

        // Case 1: cursor right after `new` keyword (bare `new` or `new `)
        if trimmed.ends_with("new") {
            let immediate_suffix = &self.source[self.offset..];
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
                return self.inject_at(self.offset, self.offset, &format!(" {SENTINEL}()"));
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
                    let end = self.offset.min(id_node.end_byte());
                    if end > start {
                        let prefix = &self.source[start..end];
                        return self.inject_at(
                            start,
                            id_node.end_byte(),
                            &format!("{prefix}{SENTINEL}()"),
                        );
                    }
                }
                return self.inject_at(self.offset, self.offset, &format!(" {SENTINEL}()"));
            }

            // Case 3: ERROR has trailing dot (e.g. `cl.`)
            if error_has_trailing_dot(err, self.offset) {
                // Must include `;` so tree-sitter parses as expression_statement > field_access
                // instead of scoped_type_identifier
                return self.inject_at(self.offset, self.offset, &format!("{SENTINEL};"));
            }
        }

        // Case 4: normal identifier/type_identifier — append sentinel
        let (replace_start, replace_end) = match cursor_node {
            Some(n)
                if (n.kind() == "identifier" || n.kind() == "type_identifier")
                    && n.start_byte() < self.offset =>
            {
                (n.start_byte(), self.offset.min(n.end_byte()))
            }
            _ => (self.offset, self.offset),
        };

        if replace_start == replace_end {
            // Nothing at cursor, insert sentinel as a statement
            self.inject_at(self.offset, self.offset, &format!("{SENTINEL};"))
        } else {
            let prefix = &self.source[replace_start..replace_end];
            // cursor 在 scoped_type_identifier 里（如 java.util.A）-> 加分号变成表达式语句
            let in_scoped_type = cursor_node.is_some_and(|n| {
                find_ancestor(n, "scoped_type_identifier").is_some()
                    || n.kind() == "scoped_type_identifier"
            });

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

            if in_scoped_type {
                self.inject_at(
                    replace_start,
                    replace_end,
                    &format!("{prefix}{SENTINEL}{suffix};"),
                )
            } else {
                self.inject_at(
                    replace_start,
                    replace_end,
                    &format!("{prefix}{SENTINEL}{suffix}"),
                )
            }
        }
    }

    fn inject_at(&self, replace_start: usize, replace_end: usize, replacement: &str) -> String {
        let mut s = String::with_capacity(self.source.len() + replacement.len() + 16);
        s.push_str(&self.source[..replace_start]);
        s.push_str(replacement);
        s.push_str(&self.source[replace_end..]);
        let tail = close_open_brackets(&s);
        s.push_str(&tail);
        s
    }

    /// 注入 SENTINEL，重新 parse，在新树上跑 determine_location
    fn inject_and_determine(
        &self,
        cursor_node: Option<Node>,
        trigger_char: Option<char>,
    ) -> Option<(CursorLocation, String)> {
        let injected_source = self.build_injected_source(cursor_node);
        let sentinel_offset = injected_source.find(SENTINEL)?;
        let sentinel_end = sentinel_offset + SENTINEL.len();

        let mut parser = self.make_parser();
        let new_tree = parser.parse(&injected_source, None)?;
        let new_root = new_tree.root_node();

        let sentinel_node =
            new_root.named_descendant_for_byte_range(sentinel_offset, sentinel_end)?;

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
            let (loc, q) = tmp.determine_location(Some(sentinel_node), trigger_char);
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
                let (loc, q) = tmp.determine_location(Some(cur), trigger_char);
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

    fn find_cursor_node<'tree>(&self, root: Node<'tree>) -> Option<Node<'tree>> {
        if let Some(n) =
            root.named_descendant_for_byte_range(self.offset.saturating_sub(1), self.offset)
            && !is_comment_kind(n.kind())
            && n.end_byte() >= self.offset
        {
            return Some(n);
        }
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
        let (loc, query) = self.determine_location_impl(cursor_node, trigger_char);
        if self.location_has_newline(&loc) {
            return (CursorLocation::Unknown, String::new());
        }
        (loc, query)
    }

    fn location_has_newline(&self, loc: &CursorLocation) -> bool {
        match loc {
            CursorLocation::ConstructorCall { class_prefix, .. } => class_prefix.contains('\n'),
            CursorLocation::MemberAccess { member_prefix, .. } => member_prefix.contains('\n'),
            CursorLocation::Expression { prefix } => prefix.contains('\n'),
            CursorLocation::MethodArgument { prefix } => prefix.contains('\n'),
            CursorLocation::TypeAnnotation { prefix } => prefix.contains('\n'),
            CursorLocation::Annotation { prefix } => prefix.contains('\n'),
            CursorLocation::Import { prefix } => prefix.contains('\n'),
            _ => false,
        }
    }

    fn determine_location_impl(
        &self,
        cursor_node: Option<Node>,
        trigger_char: Option<char>,
    ) -> (CursorLocation, String) {
        let node = match cursor_node {
            Some(n) => n,
            None => return (CursorLocation::Unknown, String::new()),
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
        (CursorLocation::Unknown, String::new())
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
                        let type_name = self.extract_type_from_decl(ancestor);
                        return (CursorLocation::VariableName { type_name }, String::new());
                    }
                    if self.is_in_type_subtree(node, ancestor) {
                        // 取光标前的文本作为 prefix，但只取最后一段（点后面的部分）
                        let text = self.cursor_truncated_text(node);
                        let clean = strip_sentinel(&text);
                        // 触发注入路径来正确识别
                        return (CursorLocation::Unknown, String::new());
                    }
                    let text = self.cursor_truncated_text(node);
                    let clean = strip_sentinel(&text);
                    return (
                        CursorLocation::Expression {
                            prefix: clean.clone(),
                        },
                        clean,
                    );
                }
                "formal_parameter" => {
                    if self.is_in_formal_param_name_position(node, ancestor) {
                        let type_name = ancestor
                            .child_by_field_name("type")
                            .map(|n| {
                                self.node_text(n)
                                    .split('<')
                                    .next()
                                    .unwrap_or("")
                                    .trim()
                                    .to_string()
                            })
                            .unwrap_or_default();
                        return (CursorLocation::VariableName { type_name }, String::new());
                    }

                    // type position
                    let text = self.cursor_truncated_text(node);
                    return (
                        CursorLocation::TypeAnnotation {
                            prefix: text.clone(),
                        },
                        text,
                    );
                }
                "argument_list" => {
                    if let Some(parent) = node.parent()
                        && parent.kind() == "method_invocation"
                        && self.offset <= ancestor.start_byte()
                    {
                        return self.handle_member_access(parent);
                    }
                    return self.handle_argument_list(ancestor);
                }
                // If inside ERROR node, return Unknown to trigger injection path
                "ERROR" => return (CursorLocation::Unknown, String::new()),
                "block" | "class_body" | "program" => break,
                _ => {}
            }
        }
        let text = self.cursor_truncated_text(node);
        let clean = strip_sentinel(&text);
        (
            CursorLocation::Expression {
                prefix: clean.clone(),
            },
            clean,
        )
    }

    fn is_in_type_subtree(&self, id_node: Node, decl_node: Node) -> bool {
        // 找到 decl_node 的类型子节点，检查 id_node 是否在其后代里
        let mut walker = decl_node.walk();
        for child in decl_node.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            // 第一个非 modifiers 子节点是类型节点
            return self.is_descendant_of(id_node, child);
        }
        false
    }

    fn is_descendant_of(&self, node: Node, ancestor: Node) -> bool {
        let mut cur = node;
        loop {
            if cur.id() == ancestor.id() {
                return true;
            }
            match cur.parent() {
                Some(p) => cur = p,
                None => return false,
            }
        }
    }

    fn extract_type_from_decl(&self, decl_node: Node) -> String {
        let mut walker = decl_node.walk();
        for child in decl_node.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            return self
                .node_text(child)
                .split('<')
                .next()
                .unwrap_or("")
                .trim()
                .to_string();
        }
        String::new()
    }

    fn is_in_formal_param_name_position(&self, id_node: Node, param_node: Node) -> bool {
        param_node
            .child_by_field_name("name")
            .is_some_and(|n| n.id() == id_node.id())
    }

    fn handle_argument_list(&self, node: Node) -> (CursorLocation, String) {
        // 找 argument_list 里光标前最近的 identifier 节点作为 prefix
        let prefix = self.find_prefix_in_argument_list(node);
        (
            CursorLocation::MethodArgument {
                prefix: prefix.clone(),
            },
            prefix,
        )
    }

    fn find_prefix_in_argument_list(&self, arg_list: Node) -> String {
        // 遍历 argument_list 的子节点，找到包含光标的那个
        let mut cursor = arg_list.walk();
        for child in arg_list.named_children(&mut cursor) {
            if child.start_byte() <= self.offset
                && child.end_byte() >= self.offset.saturating_sub(1)
            {
                let text = self.cursor_truncated_text(child);
                let clean = strip_sentinel(&text);
                return clean;
            }
        }
        String::new()
    }

    fn handle_import(&self, node: Node) -> (CursorLocation, String) {
        let mut raw_prefix = String::new();
        self.collect_import_text(node, &mut raw_prefix);
        let prefix = strip_sentinel(&raw_prefix);
        let query = prefix.rsplit('.').next().unwrap_or("").to_string();

        (CursorLocation::Import { prefix }, query)
    }

    /// 递归收集 import 声明中的有效文本（标识符、点、星号）
    /// 跳过 import 关键字、static 关键字、分号以及所有类型的注释
    fn collect_import_text(&self, node: Node, out: &mut String) {
        // 如果节点起始位置已经在光标之后，跳过（因为我们需要光标前的部分）
        if node.start_byte() >= self.offset {
            return;
        }

        // 如果没有子节点，说明是叶子节点 (Token)
        if node.child_count() == 0 {
            let kind = node.kind();
            // 过滤掉不需要的 Token
            if kind == "import" || kind == "static" || kind == ";" || is_comment_kind(kind) {
                return;
            }

            // 获取截断到光标位置的文本
            // 注意：cursor_truncated_text 内部处理了 node.end_byte().min(self.offset)
            let text = self.cursor_truncated_text(node);
            out.push_str(&text);
        } else {
            // 递归遍历子节点
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                self.collect_import_text(child, out);
            }
        }
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
                let raw = if self.offset <= s {
                    String::new()
                } else if self.offset < e {
                    self.source[s..self.offset].to_string()
                } else {
                    self.node_text(*mn).to_string()
                };
                strip_sentinel(&raw)
            }
            // 注入路径下不会走到这里；非注入路径下 dot 后面什么都没有
            None => String::new(),
        };

        let receiver_node = if dot_pos > 0 {
            Some(children[dot_pos - 1])
        } else {
            None
        };
        let receiver_expr = receiver_node
            .map(|n| self.node_text(n).to_string())
            .unwrap_or_default();

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
        let type_node = node.child_by_field_name("type");

        // If the type node starts after the cursor and is separated by a newline,
        // it means tree-sitter consumed the next line as the type.
        // We reject this and return Unknown to trigger the injection path.
        if let Some(ty) = type_node
            && self.offset < ty.start_byte()
        {
            let gap = &self.source[self.offset..ty.start_byte()];
            if gap.contains('\n') {
                return (CursorLocation::Unknown, String::new());
            }
        }

        let class_prefix = type_node
            .map(|n| {
                // Use cursor_truncated_text so we don't capture text *after* the cursor
                let raw = self.cursor_truncated_text(n);
                strip_sentinel(&raw)
            })
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

        vars.extend(self.extract_misread_var_decls(search_root));

        vars.extend(self.extract_params(root, cursor_node));
        vars
    }

    fn extract_misread_var_decls(&self, root: Node) -> Vec<LocalVar> {
        let mut result = Vec::new();
        self.collect_misread_decls(root, &mut result);
        result
    }

    fn collect_misread_decls(&self, node: Node, vars: &mut Vec<LocalVar>) {
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
                            if name_node.start_byte() >= self.offset {
                                continue;
                            }
                            let name = self.node_text(name_node);
                            // Find type from ERROR sibling (contains "var" or type_identifier)
                            let type_name = self.find_type_in_error_sibling(child);
                            let init_text = self.node_text(init_node).to_string();
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
                                    type_internal: Arc::from(
                                        java_type_to_internal(raw_ty).as_str(),
                                    ),
                                    init_expr: None,
                                }
                            };
                            vars.push(lv);
                        }
                    }
                }
            }
            self.collect_misread_decls(child, vars);
        }
    }

    fn find_type_in_error_sibling(&self, declarator_node: Node) -> Option<String> {
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
                        return Some(self.node_text(ec_child).to_string());
                    }
                }
            }
        }
        None
    }

    fn extract_params(&self, root: Node, cursor_node: Option<Node>) -> Vec<LocalVar> {
        let method = match cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset))
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

    fn extract_class_members_from_body(&self, body: Node) -> Vec<CurrentClassMember> {
        let mut members = Vec::new();
        self.collect_members_from_node(body, &mut members);
        members
    }

    fn extract_locals_from_error_nodes(&self, root: Node) -> Vec<LocalVar> {
        let mut result = Vec::new();
        self.collect_locals_in_errors(root, &mut result);
        result
    }

    fn collect_locals_in_errors(&self, node: Node, vars: &mut Vec<LocalVar>) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "ERROR" {
                // ERROR 内部可能有 local_variable_declaration
                let q_src = r#"
                (local_variable_declaration
                    type: (_) @type
                    declarator: (variable_declarator
                        name: (identifier) @name))
            "#;
                if let Ok(q) = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src) {
                    let type_idx = q.capture_index_for_name("type").unwrap();
                    let name_idx = q.capture_index_for_name("name").unwrap();
                    let found: Vec<LocalVar> = run_query(&q, child, self.bytes, None)
                        .into_iter()
                        .filter_map(|captures| {
                            let ty_node = captures.iter().find(|(idx, _)| *idx == type_idx)?.1;
                            let name_node = captures.iter().find(|(idx, _)| *idx == name_idx)?.1;
                            if ty_node.start_byte() >= self.offset {
                                return None;
                            }
                            let ty = ty_node.utf8_text(self.bytes).ok()?;
                            let name = name_node.utf8_text(self.bytes).ok()?;
                            let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
                            if raw_ty == "var" {
                                return Some(
                                    match infer_type_from_initializer(ty_node, self.bytes) {
                                        Some(t) => LocalVar {
                                            name: Arc::from(name),
                                            type_internal: Arc::from(
                                                java_type_to_internal(&t).as_str(),
                                            ),
                                            init_expr: None,
                                        },
                                        None => LocalVar {
                                            name: Arc::from(name),
                                            type_internal: Arc::from("var"),
                                            init_expr: get_initializer_text(ty_node, self.bytes),
                                        },
                                    },
                                );
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
                // 递归进入嵌套 ERROR
                self.collect_locals_in_errors(child, vars);
            } else {
                self.collect_locals_in_errors(child, vars);
            }
        }
    }

    fn collect_members_from_node(&self, node: Node, members: &mut Vec<CurrentClassMember>) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            match child.kind() {
                "method_declaration" => {
                    if let Some(m) = self.parse_method_node(child) {
                        members.push(m);
                    }
                    if let Some(block) = child.child_by_field_name("body") {
                        let mut bc = block.walk();
                        let block_children: Vec<Node> = block.children(&mut bc).collect();
                        let mut i = 0;
                        while i < block_children.len() {
                            let bc = block_children[i];
                            if bc.kind() == "ERROR" {
                                if let Some(m) = self.parse_method_node(bc) {
                                    members.push(m);
                                }
                                members.extend(self.parse_field_node(bc));
                            } else if bc.kind() == "local_variable_declaration" {
                                // Check if next sibling is ERROR starting with `(` —
                                // this means tree-sitter misread a method declaration as a variable declaration
                                let next = block_children.get(i + 1);
                                if let Some(next_node) = next
                                    && next_node.kind() == "ERROR"
                                    && self.source[next_node.start_byte()..next_node.end_byte()]
                                        .trim_start()
                                        .starts_with('(')
                                    && let Some(m) = self.parse_misread_method(bc, *next_node)
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
                    members.extend(self.parse_field_node(child));
                }
                "ERROR" => {
                    self.collect_members_from_node(child, members);
                    let snapshot = members.clone();
                    members.extend(self.parse_partial_methods_from_error(child, &snapshot));
                }
                _ => {}
            }
        }

        // If the node itself is ERROR (top-level), also parse its scattered children
        if node.kind() == "ERROR" {
            let snapshot = members.clone();
            members.extend(self.parse_partial_methods_from_error(node, &snapshot));
        }
    }

    fn parse_partial_methods_from_error(
        &self,
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
                Some(n) => self.node_text(*n),
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
                .map(|n| self.node_text(*n).contains("static"))
                .unwrap_or(false);
            let is_private = modifiers_node
                .map(|n| self.node_text(*n).contains("private"))
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
                .map(|n| self.node_text(*n))
                .unwrap_or("void");
            let params = self.node_text(children[param_pos]);
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

    /// Scan ERROR nodes inside a method block for swallowed class members
    fn collect_members_from_error_in_block(
        &self,
        block: Node,
        members: &mut Vec<CurrentClassMember>,
    ) {
        let mut cursor = block.walk();
        for child in block.children(&mut cursor) {
            if child.kind() == "ERROR" {
                if let Some(m) = self.parse_method_node(child) {
                    members.push(m);
                } else {
                    members.extend(self.parse_field_node(child));
                }
            }
        }
    }

    /// tree-sitter sometimes parses `private static Object test() {` as
    /// local_variable_declaration("private static Object test") + ERROR("() {")
    /// This method reconstructs the method member from these two nodes.
    fn parse_misread_method(
        &self,
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
                    let t = self.node_text(c);
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
                    ret_type = self.node_text(c);
                }
                "variable_declarator" => {
                    // name is the identifier inside variable_declarator
                    let mut vc = c.walk();
                    for vchild in c.children(&mut vc) {
                        if vchild.kind() == "identifier" {
                            name = Some(self.node_text(vchild));
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
            .map(|c| self.node_text(c))
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

    fn make_parser(&self) -> Parser {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .expect("failed to load java grammar");
        parser
    }
}

fn find_top_error_node(root: Node) -> Option<Node> {
    let mut cursor = root.walk();
    root.children(&mut cursor)
        .find(|&child| child.kind() == "ERROR")
}

fn build_internal_name(package: &Option<Arc<str>>, class: &Option<Arc<str>>) -> Option<Arc<str>> {
    match (package, class) {
        (Some(pkg), Some(cls)) => Some(Arc::from(format!("{}/{}", pkg, cls).as_str())),
        (None, Some(cls)) => Some(Arc::clone(cls)),
        _ => None,
    }
}

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

/// 去掉字符串里的 SENTINEL（注入路径下 prefix 可能含有它）
fn strip_sentinel(s: &str) -> String {
    s.replace(SENTINEL, "")
}

/// 把 CursorLocation 里残留的 SENTINEL 清掉
fn strip_sentinel_from_location(loc: CursorLocation) -> CursorLocation {
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
fn close_open_brackets(src: &str) -> String {
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

fn find_enclosing_method_in_error(root: Node, offset: usize) -> Option<Node> {
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
    match ty {
        "byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char" | "void" => {
            ty.to_string()
        }
        other => other.replace('.', "/"),
    }
}

fn find_error_ancestor(mut node: Node) -> Option<Node> {
    loop {
        if node.kind() == "ERROR" {
            return Some(node);
        }
        node = node.parent()?;
    }
}

fn error_has_new_keyword(error_node: Node) -> bool {
    let mut cursor = error_node.walk();
    error_node.children(&mut cursor).any(|c| c.kind() == "new")
}

fn find_identifier_in_error(error_node: Node) -> Option<Node> {
    let mut cursor = error_node.walk();
    error_node
        .children(&mut cursor)
        .find(|c| c.kind() == "identifier" || c.kind() == "type_identifier")
}

fn error_has_trailing_dot(error_node: Node, offset: usize) -> bool {
    let mut cursor = error_node.walk();
    let children: Vec<Node> = error_node.children(&mut cursor).collect();
    children
        .last()
        .is_some_and(|n| n.kind() == "." && n.end_byte() <= offset)
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
    fn test_assignment_rhs_is_expression() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                Agent.inst = 
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix.is_empty()),
            "rhs of assignment should be Expression{{prefix: \"\"}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_incomplete_method_argument_is_expression() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                proxy.run(
            }
        }
    "#};
        let line = 3u32;
        let raw = src.lines().nth(3).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        match &ctx.location {
            CursorLocation::Expression { prefix } | CursorLocation::MethodArgument { prefix } => {
                assert!(prefix.is_empty(), "prefix should be empty");
            }
            _ => panic!(
                "Expected Expression or MethodArgument, got {:?}",
                ctx.location
            ),
        }
    }

    #[test]
    fn test_close_open_brackets_unit() {
        assert_eq!(close_open_brackets("class A { void f() {"), ";}}");
        assert_eq!(close_open_brackets("foo(a, b"), ");");
        assert_eq!(close_open_brackets("class A { void f() { foo("), ");}}");
        assert_eq!(close_open_brackets("class A {}"), ";");
        assert_eq!(close_open_brackets("}}}}"), ";");
    }

    #[test]
    fn test_injection_trailing_dot_member_access() {
        let src = indoc::indoc! {r#"
        class A {
            void test() {
                RandomClass cl = new RandomClass();
                cl.
            }
        }
        "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("cl.").map(|c| (i as u32, c as u32 + 3)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { member_prefix, receiver_expr, .. }
                if member_prefix.is_empty() && receiver_expr == "cl"
            ),
            "cl.| via injection should give MemberAccess{{receiver=cl}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_injection_new_keyword_constructor() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new
            }
        }
        "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("new").map(|c| (i as u32, c as u32 + 3)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::ConstructorCall { .. }),
            "new| via injection should give ConstructorCall, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_injection_assignment_rhs_empty() {
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                Agent.inst =
            }
        }
        "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix.is_empty()),
            "empty assignment rhs should give Expression{{prefix:\"\"}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_injection_chained_call_dot() {
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
            "getMain2().| via injection should give MemberAccess, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_snapshot_agent_error_case() {
        let src = indoc::indoc! {r#"
    class Agent {
        static Object inst;
        private static Object test() { return null; }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
    }
    "#};

        // Snapshot the raw AST
        fn node_to_string(node: tree_sitter::Node, src: &str, indent: usize) -> String {
            let pad = " ".repeat(indent * 2);
            let text: String = src[node.start_byte()..node.end_byte()]
                .chars()
                .take(40)
                .collect();
            let mut s = format!(
                "{}{} [{}-{}] {:?}\n",
                pad,
                node.kind(),
                node.start_byte(),
                node.end_byte(),
                text
            );
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                s.push_str(&node_to_string(child, src, indent + 1));
            }
            s
        }

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let ast = node_to_string(tree.root_node(), src, 0);
        insta::assert_snapshot!("agent_error_ast", ast);

        // Snapshot the extraction result
        let line = 7u32;
        let raw = src.lines().nth(7).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);

        let mut members: Vec<String> = ctx
            .current_class_members
            .keys()
            .map(|k| {
                let m = &ctx.current_class_members[k];
                format!(
                    "{} static={} private={} method={}",
                    k, m.is_static, m.is_private, m.is_method
                )
            })
            .collect();
        members.sort();
        insta::assert_snapshot!("agent_error_members", members.join("\n"));
        insta::assert_snapshot!("agent_error_location", format!("{:?}", ctx.location));
        insta::assert_snapshot!(
            "agent_error_enclosing",
            format!("{:?}", ctx.enclosing_class)
        );
    }

    #[test]
    fn test_snapshot_partial_method_extraction() {
        let src = indoc::indoc! {r#"
    class Agent {
        static Object inst;
        private static Object test() { return null; }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
    }
    "#};

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let root = tree.root_node();

        // root is program, child(0) is ERROR
        let error_node = root.child(0).unwrap();
        assert_eq!(error_node.kind(), "ERROR");

        let rope = ropey::Rope::from_str(src);
        let extractor = super::JavaContextExtractor::new_with_rope(src, 0, rope);

        // Snapshot direct children kinds
        let mut cursor = error_node.walk();
        let children_info: Vec<String> = error_node
            .children(&mut cursor)
            .map(|c| {
                format!(
                    "{} {:?}",
                    c.kind(),
                    &src[c.start_byte()..c.end_byte().min(c.start_byte() + 30)]
                )
            })
            .collect();
        insta::assert_snapshot!("error_children", children_info.join("\n"));

        // Snapshot what parse_partial_methods_from_error returns
        let snapshot: Vec<CurrentClassMember> = vec![];
        let partial = extractor.parse_partial_methods_from_error(error_node, &snapshot);
        let mut result: Vec<String> = partial
            .iter()
            .map(|m| format!("{} static={} private={}", m.name, m.is_static, m.is_private))
            .collect();
        result.sort();
        insta::assert_snapshot!("partial_methods", result.join("\n"));
    }

    #[test]
    fn test_snapshot_real_agent_case() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void premain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            new Object().run();
        }
        private static Object test() { return null; }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
    }
    "#};

        let line = 13u32;
        let raw = src.lines().nth(13).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);

        let mut members: Vec<String> = ctx
            .current_class_members
            .keys()
            .map(|k| {
                let m = &ctx.current_class_members[k];
                format!(
                    "{} static={} private={} method={}",
                    k, m.is_static, m.is_private, m.is_method
                )
            })
            .collect();
        members.sort();
        insta::assert_snapshot!("real_agent_members", members.join("\n"));
        insta::assert_snapshot!("real_agent_location", format!("{:?}", ctx.location));
        insta::assert_snapshot!("real_agent_enclosing", format!("{:?}", ctx.enclosing_class));
    }

    #[test]
    fn test_snapshot_real_agent_root() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void premain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            new Object().run();
        }
        private static Object test() { return null; }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
    }
    "#};

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let root = tree.root_node();

        // Just snapshot top-level structure (2 levels deep)
        let mut top = String::new();
        let mut rc = root.walk();
        for child in root.children(&mut rc) {
            top.push_str(&format!(
                "{} [{}-{}]\n",
                child.kind(),
                child.start_byte(),
                child.end_byte()
            ));
            let mut cc = child.walk();
            for grandchild in child.children(&mut cc) {
                top.push_str(&format!(
                    "  {} [{}-{}]\n",
                    grandchild.kind(),
                    grandchild.start_byte(),
                    grandchild.end_byte()
                ));
            }
        }
        insta::assert_snapshot!("real_agent_root", top);
    }

    #[test]
    fn test_class_members_visible_when_whole_file_is_error() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void premain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            new Object().run();
        }
        private static Object test() { return null; }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
    }
    "#};
        let line = 13u32;
        let raw = src.lines().nth(13).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.current_class_members.contains_key("test"),
            "test() should be visible, members: {:?}",
            ctx.current_class_members.keys().collect::<Vec<_>>()
        );
        assert!(
            ctx.current_class_members.contains_key("agentmain"),
            "agentmain() should be visible, members: {:?}",
            ctx.current_class_members.keys().collect::<Vec<_>>()
        );
        assert!(
            ctx.current_class_members.contains_key("premain"),
            "premain() should be visible, members: {:?}",
            ctx.current_class_members.keys().collect::<Vec<_>>()
        );
        assert_eq!(ctx.enclosing_class.as_deref(), Some("Agent"));
    }

    #[test]
    fn test_snapshot_test_after_agentmain() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void premain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            new Object().run();
        }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
        private static Object test() { return null; }
    }
    "#};

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let root = tree.root_node();

        let mut top = String::new();
        let mut rc = root.walk();
        for child in root.children(&mut rc) {
            top.push_str(&format!(
                "{} [{}-{}]\n",
                child.kind(),
                child.start_byte(),
                child.end_byte()
            ));
            let mut cc = child.walk();
            for grandchild in child.children(&mut cc) {
                top.push_str(&format!(
                    "  {} [{}-{}] {:?}\n",
                    grandchild.kind(),
                    grandchild.start_byte(),
                    grandchild.end_byte(),
                    &src[grandchild.start_byte()
                        ..grandchild.end_byte().min(grandchild.start_byte() + 30)]
                ));
            }
        }
        insta::assert_snapshot!(top);

        let line = 12u32;
        let raw = src.lines().nth(12).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        let mut members: Vec<String> = ctx
            .current_class_members
            .keys()
            .map(|k| k.to_string())
            .collect();
        members.sort();
        insta::assert_snapshot!(members.join("\n"));
    }

    #[test]
    fn test_snapshot_class_body_structure() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void premain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            new Object().run();
        }
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
        private static Object test() { return null; }
    }
    "#};

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();

        fn print_node(node: tree_sitter::Node, src: &str, indent: usize, out: &mut String) {
            let pad = " ".repeat(indent * 2);
            let text: String = src[node.start_byte()..node.end_byte()]
                .chars()
                .take(30)
                .collect();
            out.push_str(&format!(
                "{}{} [{}-{}] {:?}\n",
                pad,
                node.kind(),
                node.start_byte(),
                node.end_byte(),
                text
            ));
            if indent < 4 {
                // 只展开4层
                let mut c = node.walk();
                for child in node.children(&mut c) {
                    print_node(child, src, indent + 1, out);
                }
            }
        }

        let mut out = String::new();
        print_node(tree.root_node(), src, 0, &mut out);
        insta::assert_snapshot!(out);
    }

    #[test]
    fn test_snapshot_agentmain_block() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
        private static Object test() { return null; }
    }
    "#};

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();

        // Just print everything under agentmain's block
        fn print_flat(node: tree_sitter::Node, src: &str, out: &mut String) {
            let text: String = src[node.start_byte()..node.end_byte()]
                .chars()
                .take(30)
                .collect();
            out.push_str(&format!(
                "{} [{}-{}] {:?}\n",
                node.kind(),
                node.start_byte(),
                node.end_byte(),
                text
            ));
            let mut c = node.walk();
            for child in node.children(&mut c) {
                out.push_str(&format!(
                    "  {} [{}-{}] {:?}\n",
                    child.kind(),
                    child.start_byte(),
                    child.end_byte(),
                    &src[child.start_byte()..child.end_byte().min(child.start_byte() + 25)]
                ));
            }
        }

        // Find agentmain block by offset range [292-453] from previous snapshot
        let block = tree
            .root_node()
            .named_descendant_for_byte_range(292, 293)
            .unwrap();
        // walk up to block
        let mut cur = block;
        while cur.kind() != "block" {
            cur = cur.parent().unwrap();
        }

        let mut out = String::new();
        print_flat(cur, src, &mut out);
        insta::assert_snapshot!(out);
    }

    #[test]
    fn test_class_members_visible_when_error_node_present() {
        let src = indoc::indoc! {r#"
    package org.cubewhy.relx;
    public class Agent {
        private static Object inst;
        public static void agentmain(String args, Object inst) throws Exception {
            Agent.inst = inst;
            var proxy = new Object();
            proxy.run();
            Agent.inst = 
        }
        private static Object test() { return null; }
    }
    "#};
        let line = 7u32;
        let raw = src.lines().nth(7).unwrap();
        let col = raw.len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.current_class_members.contains_key("test"),
            "test() should be extracted even when misread as local_variable_declaration, members: {:?}",
            ctx.current_class_members.keys().collect::<Vec<_>>()
        );
        assert!(
            ctx.current_class_members.get("test").unwrap().is_static,
            "test() should be marked static"
        );
    }

    #[test]
    fn test_chained_method_call_dot_is_member_access() {
        // RealMain.getInstance().| → MemberAccess, not StaticAccess
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            RealMain.getInstance().
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("getInstance().").map(|c| (i as u32, c as u32 + 14)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, member_prefix, .. }
                if receiver_expr == "RealMain.getInstance()" && member_prefix.is_empty()
            ),
            "RealMain.getInstance().| should be MemberAccess, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_chained_method_call_dot_with_prefix_is_member_access() {
        // RealMain.getInstance().ge| → MemberAccess{prefix="ge"}
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            RealMain.getInstance().ge
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| {
                l.find("getInstance().ge")
                    .map(|c| (i as u32, c as u32 + 16))
            })
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, member_prefix, .. }
                if receiver_expr == "RealMain.getInstance()" && member_prefix == "ge"
            ),
            "RealMain.getInstance().ge| should be MemberAccess{{prefix=ge}}, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_uppercase_receiver_method_invocation_is_member_access() {
        // Uppercase.method().| — receiver is method_invocation, not identifier
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            Uppercase.method().
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("method().").map(|c| (i as u32, c as u32 + 9)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, .. }
                if receiver_expr == "Uppercase.method()"
            ),
            "Uppercase.method().| should be MemberAccess, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_formal_param_type_position_is_type_annotation() {
        let src = indoc::indoc! {r#"
    class A {
        public static void main(String[] args) {}
    }
    "#};
        let line = 1u32;
        let raw = src.lines().nth(1).unwrap();
        let col = raw.find("String").unwrap() as u32 + 3; // cursor mid "String"
        let ctx = at(src, line, col);
        assert!(
            matches!(ctx.location, CursorLocation::TypeAnnotation { .. }),
            "cursor on param type should be TypeAnnotation, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_formal_param_name_position_is_variable_name() {
        let src = indoc::indoc! {r#"
    class A {
        public static void main(String[] args) {}
    }
    "#};
        let line = 1u32;
        let raw = src.lines().nth(1).unwrap();
        let args_col = raw.find("args").unwrap() as u32 + 2;
        let ctx = at(src, line, args_col);
        assert!(
            matches!(ctx.location, CursorLocation::VariableName { .. }),
            "cursor on param name should be VariableName, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_formal_param_name_has_type_name() {
        let src = indoc::indoc! {r#"
    class A {
        public static void main(String[] args) {}
    }
    "#};
        let line = 1u32;
        let raw = src.lines().nth(1).unwrap();
        let args_col = raw.find("args").unwrap() as u32 + 2;
        let ctx = at(src, line, args_col);
        assert!(
            matches!(&ctx.location, CursorLocation::VariableName { type_name } if type_name == "String[]"),
            "param name location should carry type 'String[]', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_var_name_position_is_variable_name() {
        let src = indoc::indoc! {r#"
    class A {
        public static void main() {
            String aCopy
        }
    }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let acopy_start = raw.find("aCopy").unwrap() as u32;
        let col = acopy_start + 2;
        let ctx = at(src, line, col);
        assert!(
            matches!(ctx.location, CursorLocation::VariableName { .. }),
            "var name position should be VariableName, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_var_name_position_has_type_name() {
        let src = indoc::indoc! {r#"
    class A {
        public static void main() {
            String aCopy
        }
    }
    "#};
        let line = 2u32;
        let raw = src.lines().nth(2).unwrap();
        let acopy_start = raw.find("aCopy").unwrap() as u32;
        let col = acopy_start + 2;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::VariableName { type_name } if type_name == "String"),
            "var name location should carry type 'String', got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_new_followed_by_comment_is_constructor() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            new // comment
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("new ").map(|c| (i as u32, c as u32 + 4)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::ConstructorCall { .. }),
            "new followed by comment should give ConstructorCall, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_new_at_end_of_line_is_constructor() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            new
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("new").map(|c| (i as u32, c as u32 + 3)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::ConstructorCall { .. }),
            "new at end of line should give ConstructorCall, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_expression_after_syntax_error_line() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            String str = "hello":
            s
        }
    }
    "#};
        let line = 3u32;
        let col = src.lines().nth(3).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(&ctx.location, CursorLocation::Expression { prefix } if prefix == "s"),
            "expression after syntax error should still parse, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_snapshot_syntax_error_colon_ast() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            String str = "hello":
            var b1 = str;
            b
        }
    }
    "#};
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();

        fn dump(node: tree_sitter::Node, src: &str, indent: usize, out: &mut String) {
            let pad = "  ".repeat(indent);
            let text: String = src[node.start_byte()..node.end_byte()]
                .chars()
                .take(30)
                .collect();
            out.push_str(&format!(
                "{}{} [{}-{}] {:?}\n",
                pad,
                node.kind(),
                node.start_byte(),
                node.end_byte(),
                text
            ));
            let mut c = node.walk();
            for child in node.children(&mut c) {
                dump(child, src, indent + 1, out);
            }
        }

        let mut out = String::new();
        dump(tree.root_node(), src, 0, &mut out);
        insta::assert_snapshot!(out);
    }

    #[test]
    fn test_locals_extracted_after_syntax_error_line() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            String str = "hello":
            var b1 = str;
            b
        }
    }
    "#};
        let line = 4u32;
        let col = src.lines().nth(4).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            ctx.local_variables.iter().any(|v| v.name.as_ref() == "str"),
            "str should be in locals: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
        assert!(
            ctx.local_variables.iter().any(|v| v.name.as_ref() == "b1"),
            "b1 should be in locals: {:?}",
            ctx.local_variables
                .iter()
                .map(|v| v.name.as_ref())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_fqn_type_in_var_decl_triggers_import() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            java.util.A
        }
    }
    "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("java.util.A").map(|c| (i as u32, c as u32 + 11)))
            .unwrap();
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix.contains("java.util")
            ) || matches!(
                &ctx.location,
                CursorLocation::MemberAccess { receiver_expr, .. } if receiver_expr.contains("java.util")
            ),
            "java.util.A should give Import or MemberAccess with java.util receiver, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_snapshot_fqn_type_injection() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            java.util.A
        }
    }
    "#};
        let offset = src.find("java.util.A").unwrap() + 11;
        let rope = ropey::Rope::from_str(src);
        let extractor = JavaContextExtractor::new_with_rope(src, offset, rope);

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let cursor_node = extractor.find_cursor_node(tree.root_node());

        let injected = extractor.build_injected_source(cursor_node);
        insta::assert_snapshot!(injected);
    }

    #[test]
    fn test_snapshot_fqn_type_injection_v2() {
        let src = indoc::indoc! {r#"
    class A {
        void f() {
            java.util.A
        }
    }
    "#};
        let offset = src.find("java.util.A").unwrap() + 11;
        let rope = ropey::Rope::from_str(src);
        let extractor = JavaContextExtractor::new_with_rope(src, offset, rope);

        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(src, None).unwrap();
        let cursor_node = extractor.find_cursor_node(tree.root_node());

        let injected = extractor.build_injected_source(cursor_node);
        insta::assert_snapshot!("injected_v2", injected);

        // 同时 snapshot 注入后的 AST
        let mut parser2 = tree_sitter::Parser::new();
        parser2
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree2 = parser2.parse(&injected, None).unwrap();

        fn dump(node: tree_sitter::Node, src: &str, indent: usize, out: &mut String) {
            let pad = "  ".repeat(indent);
            let text: String = src[node.start_byte()..node.end_byte()]
                .chars()
                .take(40)
                .collect();
            out.push_str(&format!(
                "{}{} [{}-{}] {:?}\n",
                pad,
                node.kind(),
                node.start_byte(),
                node.end_byte(),
                text
            ));
            let mut c = node.walk();
            for child in node.children(&mut c) {
                dump(child, src, indent + 1, out);
            }
        }

        let mut ast = String::new();
        dump(tree2.root_node(), &injected, 0, &mut ast);
        insta::assert_snapshot!("injected_v2_ast", ast);
    }

    #[test]
    fn test_injection_import_without_semicolon() {
        let src = indoc::indoc! {r#"
        import java.l
        class A {}
        "#};
        // 模拟光标在 'java.l' 后面
        let line = 0u32;
        let col = 13u32;
        let ctx = at(src, line, col);

        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.l"
            ),
            "__KIRO__ should be stripped from import prefix, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_prefix_does_not_include_next_line() {
        let src = indoc::indoc! {r#"
        import org.cubewhy.
        // some comment
        class A {}
    "#};
        let line = 0u32;
        let col = src.lines().next().unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "org.cubewhy."
            ),
            "import prefix should not bleed into next line, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_multiline_prefix_flattened() {
        let src = indoc::indoc! {r#"
        import
            org.cubewhy.
        class A {}
    "#};
        // 光标在 org.cubewhy. 末尾
        let line = 1u32;
        let col = src.lines().nth(1).unwrap().len() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "org.cubewhy."
            ),
            "multiline import should be flattened, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_with_inline_comment_ignored() {
        let src = indoc::indoc! {r#"
        import org.; // some comment
        class A {}
    "#};
        let line = 0u32;
        let col = src.lines().next().unwrap().find(';').unwrap() as u32;
        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "org."
            ),
            "inline comment should be stripped from import prefix, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_incomplete_with_newlines_swallowed() {
        // Reproduces the issue where "Ar\n\n System..." is captured as the class prefix
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new Ar
                
                System.out.println("hello");
            }
        }
        "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("new Ar").map(|c| (i as u32, c as u32 + 6)))
            .unwrap();

        let ctx = at(src, line, col);

        assert!(
            matches!(
                &ctx.location,
                CursorLocation::ConstructorCall { class_prefix, .. }
                if class_prefix == "Ar"
            ),
            "Should extract 'Ar' cleanly without newlines, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_constructor_with_newline_gap_triggers_injection() {
        // Reproduces the issue where "System.out.println" is captured as class_prefix
        // because tree-sitter greedily consumes the next line.
        let src = indoc::indoc! {r#"
        class A {
            void f() {
                new 
        
                System.out.println(new ArrayList());
            }
        }
        "#};
        // cursor is after "new "
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("new ").map(|c| (i as u32, c as u32 + 4)))
            .unwrap();

        let ctx = at(src, line, col);

        // The fix should cause handle_constructor to return Unknown,
        // triggering injection of "new __KIRO__()", which results in an empty prefix.
        match &ctx.location {
            CursorLocation::ConstructorCall { class_prefix, .. } => {
                assert!(
                    class_prefix.is_empty(),
                    "Expected empty class_prefix (from injection), but got '{}'",
                    class_prefix
                );
            }
            _ => panic!("Expected ConstructorCall, got {:?}", ctx.location),
        }
    }

    #[test]
    fn test_import_with_whitespace_and_newlines() {
        // 验证 AST 遍历能自动忽略空白字符，将分散的 token 拼合
        let src = indoc::indoc! {r#"
        import java.
            util.
            List;
        class A {}
        "#};
        // 光标在 List 后面
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("List").map(|c| (i as u32, c as u32 + 4)))
            .unwrap();

        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.util.List"
            ),
            "Should flatten multiline import ignoring whitespace, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_with_block_comments() {
        // 验证 AST 遍历能准确跳过 block_comment 节点
        let src = indoc::indoc! {r#"
        import java./* comment */util.List;
        class A {}
        "#};
        let (line, col) = src
            .lines()
            .enumerate()
            .find_map(|(i, l)| l.find("List").map(|c| (i as u32, c as u32 + 4)))
            .unwrap();

        let ctx = at(src, line, col);
        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.util.List"
            ),
            "Should ignore inline block comments, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_static_handling() {
        // 验证 AST 遍历跳过了 'import' 和 'static' 关键字
        let src = "import static java.lang.Math.PI;";
        let ctx = end_of(src); // 光标在最后

        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.lang.Math.PI"
            ),
            "Should strip 'import' and 'static' keywords, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_incomplete_truncated() {
        // 验证光标截断逻辑：光标在 'u' 之后，应该只捕获到 'u'，忽略后面的 'til'
        let src = "import java.util;";
        // 光标在 'java.u|til'
        let col = "import java.u".len() as u32;
        let ctx = at(src, 0, col);

        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.u"
            ),
            "Should truncate text at cursor position, got {:?}",
            ctx.location
        );
    }

    #[test]
    fn test_import_asterisk() {
        // 验证 * 号被视为有效 token 收集
        let src = "import java.util.*;";
        let col = "import java.util.*".len() as u32;
        let ctx = at(src, 0, col);

        assert!(
            matches!(
                &ctx.location,
                CursorLocation::Import { prefix } if prefix == "java.util.*"
            ),
            "Should include asterisk, got {:?}",
            ctx.location
        );
    }
}
