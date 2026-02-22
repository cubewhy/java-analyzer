use std::sync::Arc;
use tracing::debug;
use tree_sitter::{Node, Parser, Query};

use super::Language;
use super::ts_utils::{capture_text, run_query};
use crate::completion::context::CurrentClassMember;
use crate::completion::{
    CompletionContext,
    context::{CursorLocation, LocalVar},
};
use crate::language::ts_utils::find_method_by_offset;

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
        let offset = line_col_to_offset(source, line, character)?;
        debug!(line, character, trigger = ?trigger_char, "java: parsing context");

        let mut parser = self.make_parser();
        let tree = parser.parse(source, None)?;
        let root = tree.root_node();

        let extractor = JavaContextExtractor::new(source, offset);
        let ctx = extractor.extract(root, trigger_char);
        Some(ctx)
    }
}

struct JavaContextExtractor<'s> {
    source: &'s str,
    bytes: &'s [u8],
    offset: usize,
}

impl<'s> JavaContextExtractor<'s> {
    fn new(source: &'s str, offset: usize) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            offset,
        }
    }

    fn extract(self, root: Node, trigger_char: Option<char>) -> CompletionContext {
        // Try offset first; if that yields nothing, try offset-1 (handles col-0 case)
        let cursor_node = self.find_cursor_node(root);

        let (location, query) = self.determine_location(cursor_node, trigger_char);
        let local_variables = self.extract_locals(root, cursor_node);
        let enclosing_class = self.extract_enclosing_class(cursor_node);
        let enclosing_package = self.extract_package(root);

        // Construct the internal name using the package name + simple class name
        let enclosing_internal_name = match (&enclosing_package, &enclosing_class) {
            (Some(pkg), Some(cls)) => Some(Arc::from(format!("{}/{}", pkg, cls).as_str())),
            (None, Some(cls)) => Some(Arc::clone(cls)),
            _ => None,
        };

        let existing_imports = self.extract_imports(root);

        let current_class_members = cursor_node
            .and_then(|n| find_ancestor(n, "class_declaration"))
            .and_then(|cls| cls.child_by_field_name("body"))
            .map(|body| self.extract_class_members_from_body(body))
            .unwrap_or_default();

        let enclosing_class_member = cursor_node
            .and_then(|n| find_ancestor(n, "method_declaration"))
            .or_else(|| find_method_by_offset(root, self.offset)) // ★
            .and_then(|method| self.extract_method_as_member(method));

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
        // Primary: named node at [offset-1, offset)
        let node = root.named_descendant_for_byte_range(self.offset.saturating_sub(1), self.offset);
        if node.is_some() {
            return node;
        }
        // Fallback: try [offset, offset+1) for col-0 cases
        if self.offset < self.bytes.len() {
            return root.named_descendant_for_byte_range(self.offset, self.offset + 1);
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
                "import_declaration" => return self.handle_import(current),
                "method_invocation" => return self.handle_member_access(current),
                "field_access" => return self.handle_member_access(current),
                "object_creation_expression" => return self.handle_constructor(current),
                "argument_list" => {
                    let prefix_text = &self.source[..self.offset];
                    let last_line = prefix_text.lines().last().unwrap_or("").trim();
                    let prefix = extract_arg_prefix(last_line);
                    return (
                        CursorLocation::MethodArgument {
                            prefix: prefix.clone(),
                        },
                        prefix,
                    );
                }
                "identifier" | "type_identifier" => {
                    let mut ancestor = current;
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
                            "object_creation_expression" => {
                                return self.handle_constructor(ancestor);
                            }
                            "local_variable_declaration" => {
                                if self.is_in_type_position(current, ancestor) {
                                    let text = self.cursor_truncated_text(current);
                                    return (
                                        CursorLocation::TypeAnnotation {
                                            prefix: text.clone(),
                                        },
                                        text,
                                    );
                                }
                                // Check if it is in the variable name position (the name field of variable_declarator)
                                if self.is_in_name_position(current, ancestor) {
                                    // Variable name not fully padded
                                    return (CursorLocation::Unknown, String::new());
                                }
                                // Other locations (initialization expressions) -> Expression
                                let text = self.cursor_truncated_text(current);
                                return self.maybe_member_access_from_text(text);
                            }
                            "argument_list" => {
                                if let Some(parent) = current.parent()
                                    && parent.kind() == "method_invocation"
                                {
                                    let open_paren_offset = current.start_byte();
                                    // When the cursor is before or just on '(', it completes member names, not parameters.
                                    if self.offset <= open_paren_offset {
                                        return self.handle_member_access(parent);
                                    }
                                }
                                let prefix_text = &self.source[..self.offset];
                                let last_line = prefix_text.lines().last().unwrap_or("").trim();
                                let prefix = extract_arg_prefix(last_line);
                                return (
                                    CursorLocation::MethodArgument {
                                        prefix: prefix.clone(),
                                    },
                                    prefix,
                                );
                            }
                            "block" | "class_body" | "program" => break,
                            _ => {}
                        }
                    }
                    // Truncate to cursor position
                    let text = self.cursor_truncated_text(current);
                    return self.maybe_member_access_from_text(text);
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

    /// Whether the cursor is located at the position of the variable name in the variable declaration.
    fn is_in_name_position(&self, id_node: Node, decl_node: Node) -> bool {
        // The first named child of variable_declarator is name
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

    /// Retrieves the node text, but only the cursor position (handles the case where the cursor is in the middle of the identifier).
    fn cursor_truncated_text(&self, node: Node) -> String {
        let start = node.start_byte();
        let end = node.end_byte().min(self.offset);
        if end <= start {
            return String::new();
        }
        self.source[start..end].to_string()
    }

    /// Whether the cursor is located at the type declaration position of the variable.
    fn is_in_type_position(&self, id_node: Node, decl_node: Node) -> bool {
        // The first named non-modifier child node of local_variable_declaration is the type
        let mut walker = decl_node.walk();
        for child in decl_node.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            // First non-modifier named child node
            return child.id() == id_node.id();
        }
        false
    }

    /// Core heuristic: given the trimmed last line before cursor,
    /// return (CursorLocation, query) or None if no pattern matched.
    /// Caller supplies the final fallback.
    fn parse_last_line(&self, last_line: &str) -> Option<(CursorLocation, String)> {
        // ── constructor: "new Foo" / "new " ──────────────────────────────────
        let stmt_fragment = last_line
            .rsplit([';', '{', '='])
            .next()
            .unwrap_or(last_line)
            .trim();

        // Match both "new Foo" and "new" (trailing space trimmed away)
        let new_rest = stmt_fragment.strip_prefix("new ").or_else(|| {
            if stmt_fragment == "new" {
                Some("")
            } else {
                None
            }
        });

        // Only treat as ConstructorCall if there's no dot after the constructor
        // "new Foo"      → ConstructorCall ✓
        // "new Foo()"    → ConstructorCall ✓
        // "new Foo()."   → skip, let dot-access handle it ✗
        if let Some(rest) = new_rest {
            let has_dot_after_ctor = rest.contains(')') && rest.trim_end().ends_with('.');
            // more precisely: if rest contains "(..." and after closing paren there's a dot
            if !has_chained_dot(rest) {
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
        }
        // ── dot access ────────────────────────────────────────────────────────
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

    /// When tree-sitter fails to recognize field_access, use text heuristics to determine it.
    fn maybe_member_access_from_text(&self, text: String) -> (CursorLocation, String) {
        let prefix_text = &self.source[..self.offset];
        let last_line = prefix_text.lines().last().unwrap_or("").trim();

        if let Some(result) = self.parse_last_line(last_line) {
            return result;
        }

        (
            CursorLocation::Expression {
                prefix: text.clone(),
            },
            text,
        )
    }

    fn fallback_location(&self, _trigger_char: Option<char>) -> (CursorLocation, String) {
        let prefix_text = &self.source[..self.offset];
        let last_line = prefix_text.lines().last().unwrap_or("").trim();

        if let Some(result) = self.parse_last_line(last_line) {
            return result;
        }

        let query = last_line
            .split(|c: char| !c.is_alphanumeric() && c != '_')
            .next_back()
            .unwrap_or("")
            .to_string();
        (
            CursorLocation::Expression {
                prefix: query.clone(),
            },
            query,
        )
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
        // Structure of field_access / method_invocation:
        // Java field_access:  identifier "." identifier
        // Java method_invocation: ... "." identifier arguments
        // Child nodes do not have a field name; the field name needs to be retrieved by position.

        // Take the last identifier/type_identifier as member_prefix
        let mut walker = node.walk();
        let children: Vec<Node> = node.children(&mut walker).collect();

        let dot_pos = children.iter().position(|n| n.kind() == ".");

        // Find the first identifier after "."
        let dot_pos = match dot_pos {
            Some(p) => p,
            None => {
                // Find the method name identifier (the first identifier child node)
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

        // Truncate member_prefix with the cursor
        let member_prefix = match member_node {
            Some(mn) => {
                let node_start = mn.start_byte();
                let node_end = mn.end_byte();
                if self.offset <= node_start {
                    String::new()
                } else if self.offset < node_end {
                    self.source[node_start..self.offset].to_string()
                } else {
                    self.node_text(*mn).to_string()
                }
            }
            None => {
                let prefix_text = &self.source[..self.offset];
                let last_line = prefix_text.lines().last().unwrap_or("");
                last_meaningful_dot(last_line.trim())
                    .map(|p| last_line.trim()[p + 1..].to_string())
                    .unwrap_or_default()
            }
        };

        // Take the part before "." as the receiver
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
            let is_type = recv_text.chars().next().is_some_and(|c| c.is_uppercase());
            if is_type && node.kind() == "field_access" {
                let internal = recv_text.replace('.', "/");
                return (
                    CursorLocation::StaticAccess {
                        class_internal_name: Arc::from(internal.as_str()),
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
        // Walk up to local_variable_declaration
        let decl = find_ancestor(node, "local_variable_declaration")?;
        // The first non-modifier named child is the type
        let mut walker = decl.walk();
        for child in decl.named_children(&mut walker) {
            if child.kind() == "modifiers" {
                continue;
            }
            let ty = self.node_text(child);
            // Strip generics: List<String> → List
            let simple = ty.split('<').next()?.trim();
            if !simple.is_empty() {
                return Some(simple.to_string());
            }
        }
        None
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

        let raw_results = run_query(&q, search_root, self.bytes, None);

        let mut vars: Vec<LocalVar> = raw_results
            .into_iter()
            .filter_map(|captures| {
                let ty_node = captures.iter().find(|(idx, _)| *idx == type_idx)?.1;
                let name_node = captures.iter().find(|(idx, _)| *idx == name_idx)?.1;

                // Manual filtering: The declaration must precede the cursor (using the start position of the type node)
                if ty_node.start_byte() >= self.offset {
                    return None;
                }

                let ty = ty_node.utf8_text(self.bytes).ok()?;
                let name = name_node.utf8_text(self.bytes).ok()?;

                tracing::debug!(
                    ty,
                    name,
                    start = ty_node.start_byte(),
                    offset = self.offset,
                    "extracted local var"
                );

                let raw_ty = ty.split('<').next().unwrap_or(ty).trim();
                Some(LocalVar {
                    name: Arc::from(name),
                    type_internal: Arc::from(java_type_to_internal(raw_ty).as_str()),
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
            (formal_parameter
                type: (_) @type
                name: (identifier) @name)
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
                })
            })
            .collect()
    }

    fn extract_enclosing_class(&self, cursor_node: Option<Node>) -> Option<Arc<str>> {
        let class_node = cursor_node.and_then(|n| find_ancestor(n, "class_declaration"))?;
        let name_node = class_node.child_by_field_name("name")?;
        Some(Arc::from(self.node_text(name_node)))
    }

    fn extract_imports(&self, root: Node) -> Vec<String> {
        let query_src = r#"(import_declaration) @import"#;
        let q = match Query::new(&tree_sitter_java::LANGUAGE.into(), query_src) {
            Ok(q) => q,
            Err(_) => return vec![],
        };
        let idx = q.capture_index_for_name("import").unwrap();

        run_query(&q, root, self.bytes, None)
            .into_iter()
            .filter_map(|captures| {
                let text = capture_text(&captures, idx, self.bytes)?;
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
        let q_src = r#"(package_declaration) @pkg"#;
        let q = Query::new(&tree_sitter_java::LANGUAGE.into(), q_src).ok()?;
        let idx = q.capture_index_for_name("pkg")?;
        let results = run_query(&q, root, self.bytes, None);
        let text = results
            .first()
            .and_then(|caps| capture_text(caps, idx, self.bytes))?;
        // "package org.cubewhy;" → "org/cubewhy"
        let pkg = text
            .trim_start_matches("package")
            .trim()
            .trim_end_matches(';')
            .trim()
            .replace('.', "/");
        Some(Arc::from(pkg.as_str()))
    }

    /// Extract all methods and fields directly from the class_body AST (full text, not limited by cursor)
    fn extract_class_members_from_body(&self, body: Node) -> Vec<CurrentClassMember> {
        let mut members = Vec::new();
        let mut cursor = body.walk();

        for child in body.children(&mut cursor) {
            match child.kind() {
                "method_declaration" => {
                    // Extract method name and modifiers
                    let mut name: Option<&str> = None;
                    let mut is_static = false;
                    let mut is_private = false;
                    let mut ret_type = "void";
                    let mut params = "()";

                    let mut wc = child.walk();
                    for c in child.children(&mut wc) {
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
                    if let Some(n) = name
                        && n != "<init>"
                        && n != "<clinit>"
                    {
                        members.push(CurrentClassMember {
                            name: Arc::from(n),
                            is_method: true,
                            is_static,
                            is_private,
                            descriptor: Arc::from(
                                crate::index::source::build_java_descriptor(params, ret_type)
                                    .as_str(),
                            ),
                        });
                    }
                }
                "field_declaration" => {
                    let mut is_static = false;
                    let mut is_private = false;
                    let mut field_type = "Object";
                    let mut names = Vec::new();

                    let mut wc = child.walk();
                    for c in child.children(&mut wc) {
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
                    for n in names {
                        members.push(CurrentClassMember {
                            name: Arc::from(n.as_str()),
                            is_method: false,
                            is_static,
                            is_private,
                            descriptor: Arc::from(
                                crate::index::source::java_type_to_descriptor(field_type).as_str(),
                            ),
                        });
                    }
                }
                _ => {}
            }
        }

        members
    }

    /// Resolve a single method_declaration node into CurrentClassMember
    fn extract_method_as_member(&self, method: Node) -> Option<CurrentClassMember> {
        let mut name: Option<&str> = None;
        let mut is_static = false;
        let mut is_private = false;
        let mut ret_type = "void";
        let mut params = "()";

        let mut wc = method.walk();
        for c in method.children(&mut wc) {
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

        let name = name?;
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

    fn node_text(&self, node: Node) -> &str {
        node.utf8_text(self.bytes).unwrap_or("")
    }

    /// Cheap text heuristic to extract the LHS declared type from a line like
    /// `"    String a = new "` or `"    List<Integer> items = new "`.
    /// Returns `None` if the pattern is not recognised.
    fn infer_expected_type_from_text(&self, line: &str) -> Option<String> {
        // Split on '=' — take the LHS
        let lhs = line.split('=').next()?.trim();
        // lhs is like "String a" or "List<Integer> items" or "    String a"
        // Take the first whitespace-separated token (the type)
        let first_token = lhs.split_whitespace().next()?;
        // Strip generics: "List<Integer>" → "List"
        let simple = first_token.split('<').next()?.trim();
        // Must start with uppercase — otherwise it's a keyword / modifier
        if simple.chars().next().is_some_and(|c| c.is_uppercase()) {
            Some(simple.to_string())
        } else {
            None
        }
    }
}

/// Returns true if `rest` (the part after "new ") contains a method chain dot.
/// e.g. "Foo()."  → true
///      "Foo(a)." → true
///      "Foo"     → false
///      "Foo()"   → false
fn has_chained_dot(rest: &str) -> bool {
    // Find the closing paren that matches the first open paren
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

/// 从当前行提取方法参数位置的前缀
/// "foo(arg1, aV" → "aV"
/// "foo(arg1, )"  → ""  (光标在 ')' 前的空白)
/// "foo("          → ""
fn extract_arg_prefix(last_line: &str) -> String {
    // 找最后一个未闭合的 '('
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
        _ => {
            // 括号已闭合或找不到，取整行
            last_line
        }
    };
    // 取最后一个逗号之后的内容
    let after_comma = match inside.rfind(',') {
        Some(pos) => &inside[pos + 1..],
        None => inside,
    };
    after_comma.trim().to_string()
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

pub fn line_col_to_offset(source: &str, line: u32, character: u32) -> Option<usize> {
    let mut current_line = 0u32;
    let mut offset = 0usize;
    for ch in source.chars() {
        if current_line == line {
            break;
        }
        if ch == '\n' {
            current_line += 1;
        }
        offset += ch.len_utf8();
    }
    if current_line != line {
        return None;
    }
    let mut char_count = 0u32;
    let mut char_offset = 0usize;
    for ch in source[offset..].chars() {
        if char_count >= character {
            break;
        }
        char_count += ch.len_utf16() as u32;
        char_offset += ch.len_utf8();
    }
    Some(offset + char_offset)
}

// TODO: we should only trust the java classpath, don't hard-encode the type name there
pub fn java_type_to_internal(ty: &str) -> String {
    match ty {
        "String" => "java/lang/String",
        "Object" => "java/lang/Object",
        "Integer" => "java/lang/Integer",
        "Long" => "java/lang/Long",
        "Double" => "java/lang/Double",
        "Boolean" => "java/lang/Boolean",
        "Float" => "java/lang/Float",
        "Byte" => "java/lang/Byte",
        "Short" => "java/lang/Short",
        "Character" => "java/lang/Character",
        "List" => "java/util/List",
        "ArrayList" => "java/util/ArrayList",
        "Map" => "java/util/Map",
        "HashMap" => "java/util/HashMap",
        "Set" => "java/util/Set",
        "HashSet" => "java/util/HashSet",
        "Optional" => "java/util/Optional",
        "Stream" => "java/util/stream/Stream",
        "File" => "java/io/File",
        "Path" => "java/nio/file/Path",
        other => return other.replace('.', "/"),
    }
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::completion::context::CursorLocation;

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
    fn test_line_col_to_offset() {
        let src = "hello\nworld";
        assert_eq!(line_col_to_offset(src, 0, 5), Some(5));
        assert_eq!(line_col_to_offset(src, 1, 3), Some(9));
        assert_eq!(line_col_to_offset(src, 5, 0), None);
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

    // ── infer_expected_type_from_lhs (via handle_constructor) ─────────────

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

    // ── infer_expected_type_from_text unit tests ───────────────────────────

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
        // new RandomClass| → ConstructorCall (not affected by fix)
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
}
