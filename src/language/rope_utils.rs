use ropey::Rope;

pub fn rope_line_col_to_offset(rope: &Rope, line: u32, character: u32) -> Option<usize> {
    let line_idx = line as usize;
    if line_idx >= rope.len_lines() {
        return None;
    }

    let line_byte_start = rope.line_to_byte(line_idx);
    let line_slice = rope.line(line_idx);

    let mut utf16_units = 0usize;
    let mut byte_offset = 0usize;

    for ch in line_slice.chars() {
        if utf16_units >= character as usize {
            break;
        }
        utf16_units += ch.len_utf16();
        byte_offset += ch.len_utf8();
    }

    Some(line_byte_start + byte_offset)
}

pub fn line_col_to_offset(source: &str, line: u32, character: u32) -> Option<usize> {
    let rope = Rope::from_str(source);
    rope_line_col_to_offset(&rope, line, character)
}

#[cfg(test)]
mod tests {
    use crate::language::rope_utils::line_col_to_offset;

    #[test]
    fn test_line_col_to_offset() {
        let src = "hello\nworld";
        assert_eq!(line_col_to_offset(src, 0, 5), Some(5));
        assert_eq!(line_col_to_offset(src, 1, 3), Some(9));
        assert_eq!(line_col_to_offset(src, 5, 0), None);
    }
}
