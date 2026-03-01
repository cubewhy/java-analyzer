use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType};

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::CLASS,          // 0
    SemanticTokenType::INTERFACE,      // 1
    SemanticTokenType::ENUM,           // 2
    SemanticTokenType::TYPE_PARAMETER, // 3
    SemanticTokenType::METHOD,         // 4
    SemanticTokenType::PROPERTY,       // 5
    SemanticTokenType::VARIABLE,       // 6
    SemanticTokenType::PARAMETER,      // 7
    SemanticTokenType::KEYWORD,        // 8
    SemanticTokenType::MODIFIER,       // 9
    SemanticTokenType::DECORATOR,      // 10 Java Annotation
    SemanticTokenType::STRING,         // 11
];

pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::STATIC,   // 1 << 0
    SemanticTokenModifier::ABSTRACT, // 1 << 1
    SemanticTokenModifier::READONLY, // 1 << 2 Java final
];

pub fn get_type_idx(t: &SemanticTokenType) -> u32 {
    TOKEN_TYPES.iter().position(|it| it == t).unwrap_or(0) as u32
}

pub fn get_modifier_mask(m: &SemanticTokenModifier) -> u32 {
    TOKEN_MODIFIERS
        .iter()
        .position(|it| it == m)
        .map(|idx| 1 << idx)
        .unwrap_or(0)
}
