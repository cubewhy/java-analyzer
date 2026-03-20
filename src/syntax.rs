use std::sync::Arc;

use rowan::{GreenNode, GreenNodeBuilder};
use tree_sitter::Tree;

const TRIVIA_KIND_RAW: u16 = u16::MAX - 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RawSyntaxKind(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnifiedLanguage {
    Java,
    Kotlin,
    Unknown,
}

impl UnifiedLanguage {
    pub fn from_language_id(language_id: &str) -> Self {
        match language_id {
            "java" => Self::Java,
            "kotlin" => Self::Kotlin,
            _ => Self::Unknown,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Java => "java",
            Self::Kotlin => "kotlin",
            Self::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UnifiedLanguageTag;

impl rowan::Language for UnifiedLanguageTag {
    type Kind = RawSyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        RawSyntaxKind(raw.0)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.0)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<UnifiedLanguageTag>;
pub type SyntaxToken = rowan::SyntaxToken<UnifiedLanguageTag>;
pub type SyntaxElement = rowan::SyntaxElement<UnifiedLanguageTag>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<UnifiedLanguageTag>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<UnifiedLanguageTag>;
pub type TextRange = rowan::TextRange;
pub type TextSize = rowan::TextSize;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub kind: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone)]
pub struct SyntaxSnapshot {
    language: UnifiedLanguage,
    green: GreenNode,
    errors: Arc<[SyntaxError]>,
}

impl SyntaxSnapshot {
    pub fn from_tree(language_id: &str, text: &str, tree: &Tree) -> Self {
        let language = UnifiedLanguage::from_language_id(language_id);
        let mut builder = GreenNodeBuilder::new();
        let mut errors = Vec::new();
        build_green_from_ts_node(tree.root_node(), text, &mut builder, &mut errors);
        let green = builder.finish();

        Self {
            language,
            green,
            errors: errors.into(),
        }
    }

    pub fn language(&self) -> UnifiedLanguage {
        self.language
    }

    pub fn root(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

fn build_green_from_ts_node(
    node: tree_sitter::Node,
    text: &str,
    builder: &mut GreenNodeBuilder<'_>,
    errors: &mut Vec<SyntaxError>,
) {
    if node.child_count() == 0 {
        let token_text = &text[node.start_byte()..node.end_byte()];
        builder.token(rowan::SyntaxKind(node.kind_id()), token_text);

        if node.is_error() || node.is_missing() {
            errors.push(SyntaxError {
                kind: Arc::from(node.kind()),
                range: TextRange::new(
                    TextSize::from(node.start_byte() as u32),
                    TextSize::from(node.end_byte() as u32),
                ),
            });
        }
        return;
    }

    builder.start_node(rowan::SyntaxKind(node.kind_id()));

    if node.is_error() || node.is_missing() {
        errors.push(SyntaxError {
            kind: Arc::from(node.kind()),
            range: TextRange::new(
                TextSize::from(node.start_byte() as u32),
                TextSize::from(node.end_byte() as u32),
            ),
        });
    }

    let mut cursor = node.walk();
    let mut next_start = node.start_byte();
    for child in node.children(&mut cursor) {
        if next_start < child.start_byte() {
            builder.token(
                rowan::SyntaxKind(TRIVIA_KIND_RAW),
                &text[next_start..child.start_byte()],
            );
        }
        build_green_from_ts_node(child, text, builder, errors);
        next_start = child.end_byte();
    }

    if next_start < node.end_byte() {
        builder.token(
            rowan::SyntaxKind(TRIVIA_KIND_RAW),
            &text[next_start..node.end_byte()],
        );
    }

    builder.finish_node();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_rowan_snapshot_from_java_tree() {
        let text = "class Test { int x; }";
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_java::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(text, None).unwrap();

        let snapshot = SyntaxSnapshot::from_tree("java", text, &tree);

        assert_eq!(snapshot.language().as_str(), "java");
        assert_eq!(snapshot.root().text().to_string(), text);
    }
}
