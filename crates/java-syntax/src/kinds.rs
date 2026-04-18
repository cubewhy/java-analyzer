#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    L_PAREN, // (
    R_PAREN, // )

    L_BRACE, // {
    R_BRACE, // }

    L_BRACKET, // [
    R_BRACKET, // ]

    STRING_LIT, // ""
    NUMBER_LIT, // dec, hex, oct, bin
    STRING_TEMPLATE_BEGIN,
    STRING_TEMPLATE_MID,
    STRING_TEMPLATE_END,
    TEXT_BLOCK_TEMPLATE_BEGIN,
    TEXT_BLOCK_TEMPLATE_MID,
    TEXT_BLOCK_TEMPLATE_END,
    NULL_LIT,       // null
    TRUE_LIT,       // true
    FALSE_LIT,      // false
    CHAR_LIT,       // ''
    SEMICOLON,      // ;
    DOT,            // .
    AT,             // @
    PLUS,           // +
    MINUS,          // -
    STAR,           // *
    SLASH,          // /
    LESS_EQUAL,     // <=
    LESS,           // <
    GREATER,        // >
    GREATER_EQUAL,  // >=
    EQUAL_EQUAL,    // ==
    EQUAL,          // =
    OR,             // ||
    BIT_OR,         // |
    BIT_OR_EQUAL,   // |=
    OR_EQUAL,       // |=
    AND,            // &&
    BIT_AND,        // &
    AND_EQUAL,      // &=
    NOT,            // !
    MODULO,         //
    CARET,          // ^
    DIVIDE_EQUAL,   // /=
    NOT_EQUAL,      // !=
    MULTIPLE_EQUAL, // *=
    PLUS_EQUAL,     // +=
    PLUS_PLUS,      // ++
    MINUS_EQUAL,    // -=
    MINUS_MINUS,    // --
    XOR_EQUAL,      // ^=
    MODULO_EQUAL,   // %=
    COMMA,          // ,
    QUESTION,       // ?
    ARROW,          // ->
    COLON_COLON,    // ::
    COLON,          // :
    ELLIPSIS,       // ...
    TEXT_BLOCK,     // """ """

    // Keywords
    PACKAGE_KW,      // package
    IMPORT_KW,       // import
    CLASS_KW,        // class
    PUBLIC_KW,       // public
    PRIVATE_KW,      // private
    PROTECTED_KW,    // protected
    FINAL_KW,        // final
    STATIC_KW,       // static
    VOID_KW,         // void
    BYTE_KW,         // byte
    ENUM_KW,         // enum
    INTERFACE_KW,    // interface
    ABSTRACT_KW,     // abstract
    FOR_KW,          // for
    WHILE_KW,        // while
    CONTINUE_KW,     // continue
    BREAK_KW,        // break
    INSTANCEOF_KW,   // instanceof
    RETURN_KW,       // return
    TRANSIENT_KW,    // transient
    EXTENDS_KW,      // extends
    IMPLEMENTS_KW,   // implements
    NEW_KW,          // new
    ASSERT_KW,       // assert
    SWITCH_KW,       // switch
    DEFAULT_KW,      // default
    SYNCHRONIZED_KW, // synchronized
    DO_KW,           // do
    IF_KW,           // if
    ELSE_KW,         // else
    THIS_KW,         // this
    SUPER_KW,        // super
    VOLATILE_KW,     // volatile
    NATIVE_KW,       // native
    THROW_KW,        // throw
    THROWS_KW,       // throws
    TRY_KW,          // try
    CATCH_KW,        // catch
    FINALLY_KW,      // finally
    STRICTFP_KW,     // strictfp
    DOUBLE_KW,       // double
    INT_KW,          // int
    SHORT_KW,        // short
    LONG_KW,         // long
    FLOAT_KW,        // float
    CHAR_KW,         // char
    BOOLEAN_KW,      // boolean

    // reserved keywords
    GOTO_KW,  // goto
    CONST_KW, // const

    // Trivia
    LINE_COMMENT,
    BLOCK_COMMENT,
    JAVADOC,
    WHITESPACE,
    UNKNOWN,

    // Internal
    IDENTIFIER,
    EOF,

    // Nodes
    ROOT,
    MISSING,
    ERROR,
}

impl SyntaxKind {
    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            SyntaxKind::WHITESPACE | SyntaxKind::LINE_COMMENT | SyntaxKind::BLOCK_COMMENT
        )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
