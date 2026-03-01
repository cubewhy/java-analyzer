use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tree_sitter::Node;

use crate::lsp::semantic_tokens::{get_modifier_mask, get_type_idx};
use crate::workspace::Workspace;

pub async fn handle_semantic_tokens_full(
    workspace: Arc<Workspace>,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let uri = params.text_document.uri;
    let doc = workspace.documents.get(&uri)?;

    // 仅支持 Java，后续可按需扩展
    if doc.language_id != "java" {
        return None;
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_java::LANGUAGE.into())
        .ok()?;
    let tree = parser.parse(doc.content.as_ref(), None)?;

    let mut collector = TokenCollector::new(doc.content.as_bytes());
    collector.collect(tree.root_node());

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: collector.finish(),
    }))
}

struct TokenCollector<'a> {
    bytes: &'a [u8],
    data: Vec<SemanticToken>,
    last_line: u32,
    last_col: u32,
}

impl<'a> TokenCollector<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            data: Vec::new(),
            last_line: 0,
            last_col: 0,
        }
    }

    fn push_token(&mut self, node: Node, t: SemanticTokenType, modifiers: u32) {
        let start = node.start_position();
        let line = start.row as u32;
        let col = start.column as u32;
        let length = (node.end_byte() - node.start_byte()) as u32;

        let delta_line = line - self.last_line;
        let delta_start = if delta_line == 0 {
            col - self.last_col
        } else {
            col
        };

        // 修复：直接构造 SemanticToken 结构体
        self.data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: get_type_idx(&t),
            token_modifiers_bitset: modifiers,
        });

        self.last_line = line;
        self.last_col = col;
    }

    fn collect(&mut self, node: Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let mut handled = true;
            let kind = child.kind();

            match kind {
                "type_identifier" => self.push_token(child, SemanticTokenType::CLASS, 0),

                "identifier" => {
                    let parent = child.parent().map(|p| p.kind()).unwrap_or("");
                    match parent {
                        "method_declaration" | "method_invocation" => {
                            let mut mods = 0;
                            if self.is_static(child.parent().unwrap()) {
                                mods |= get_modifier_mask(&SemanticTokenModifier::STATIC);
                            }
                            self.push_token(child, SemanticTokenType::METHOD, mods);
                        }
                        "variable_declarator" => {
                            let is_field = child
                                .parent()
                                .and_then(|p| p.parent())
                                .is_some_and(|gp| gp.kind() == "field_declaration");

                            let t = if is_field {
                                SemanticTokenType::PROPERTY
                            } else {
                                SemanticTokenType::VARIABLE
                            };
                            let mut mods = 0;
                            if is_field && self.is_final(child.parent().unwrap().parent().unwrap())
                            {
                                mods |= get_modifier_mask(&SemanticTokenModifier::READONLY);
                            }
                            self.push_token(child, t, mods);
                        }
                        "formal_parameter" => {
                            self.push_token(child, SemanticTokenType::PARAMETER, 0)
                        }
                        _ => handled = false,
                    }
                }

                // 关键字与修饰符
                "static" => self.push_token(
                    child,
                    SemanticTokenType::MODIFIER,
                    get_modifier_mask(&SemanticTokenModifier::STATIC),
                ),
                "final" => self.push_token(
                    child,
                    SemanticTokenType::MODIFIER,
                    get_modifier_mask(&SemanticTokenModifier::READONLY),
                ),

                "string_literal" => self.push_token(child, SemanticTokenType::STRING, 0),
                "marker_annotation" | "annotation" => {
                    self.push_token(child, SemanticTokenType::DECORATOR, 0)
                }

                _ => handled = false,
            }

            // 如果当前节点没被消耗，或者它还有子节点，继续深入递归
            if !handled || child.child_count() > 0 {
                self.collect(child);
            }
        }
    }

    fn is_static(&self, node: Node) -> bool {
        node.child_by_field_name("modifiers")
            .and_then(|m| m.utf8_text(self.bytes).ok())
            .is_some_and(|text| text.contains("static"))
    }

    fn is_final(&self, node: Node) -> bool {
        node.child_by_field_name("modifiers")
            .and_then(|m| m.utf8_text(self.bytes).ok())
            .is_some_and(|text| text.contains("final"))
    }

    fn finish(self) -> Vec<SemanticToken> {
        self.data
    }
}
