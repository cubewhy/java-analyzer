use unicode_categories::UnicodeCategories;

pub fn is_java_identifier_start(c: char) -> bool {
    c.is_letter() || c.is_number_letter() || c.is_symbol_currency() || c.is_punctuation_connector()
}

fn is_java_identifier_ignorable(c: char) -> bool {
    matches!(
        c,
        '\u{0000}'..='\u{0008}' |
        '\u{000E}'..='\u{001B}' |
        '\u{007F}'..='\u{009F}'
    ) || c.is_other_format()
}

pub fn is_java_identifier_part(c: char) -> bool {
    is_java_identifier_start(c)
        || c.is_number_decimal_digit()
        || c.is_mark_spacing_combining()
        || c.is_mark_nonspacing()
        || is_java_identifier_ignorable(c)
}
