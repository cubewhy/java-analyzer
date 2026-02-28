use crate::completion::type_resolver::ChainSegment;

pub(crate) fn parse_chain_from_expr(expr: &str) -> Vec<ChainSegment> {
    let mut segments = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    let mut in_method = false;
    let mut arg_start = 0usize;
    let mut arg_texts: Vec<String> = Vec::new();

    for (char_pos, ch) in expr.char_indices() {
        match ch {
            '(' => {
                depth += 1;
                if depth == 1 {
                    in_method = true;
                    arg_start = char_pos + 1;
                    arg_texts = Vec::new();
                }
            }
            ')' => {
                depth -= 1;
                if depth == 0 && in_method {
                    let arg = expr[arg_start..char_pos].trim();
                    let has_any = !arg.is_empty();
                    if has_any {
                        arg_texts.push(arg.to_string());
                    }
                    let arg_count = if arg_texts.is_empty() {
                        0
                    } else {
                        arg_texts.len() as i32
                    };
                    segments.push(ChainSegment::method_with_types(
                        current.trim(),
                        arg_count,
                        vec![],
                        arg_texts.clone(),
                    ));
                    current = String::new();
                    arg_texts = Vec::new();
                    in_method = false;
                }
            }
            ',' if depth == 1 => {
                let arg = expr[arg_start..char_pos].trim();
                arg_texts.push(arg.to_string());
                arg_start = char_pos + 1;
            }
            '.' if depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() && !in_method {
                    segments.push(ChainSegment::variable(trimmed));
                }
                current = String::new();
            }
            '[' if depth == 0 && !in_method => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    segments.push(ChainSegment::variable(trimmed));
                }
                current = "[".to_string();
            }
            ']' if depth == 0 && !in_method && current.starts_with('[') => {
                current.push(']');
                segments.push(ChainSegment::variable(current.clone()));
                current = String::new();
            }
            c => {
                if depth == 0 {
                    current.push(c);
                }
            }
        }
    }

    let trimmed = current.trim();
    if !trimmed.is_empty() && depth == 0 && !in_method {
        segments.push(ChainSegment::variable(trimmed.to_string()));
    }

    segments
}

#[cfg(test)]
mod tests {
    use crate::completion::parser::parse_chain_from_expr;

    #[test]
    fn test_chain_multi_dimensional_array() {
        // 测试解析 m.arr[0][0] 是否被正确切割
        let segments = parse_chain_from_expr("m.arr[0][1]");
        let names: Vec<String> = segments.into_iter().map(|s| s.name).collect();
        assert_eq!(
            names,
            vec![
                "m".to_string(),
                "arr".to_string(),
                "[0]".to_string(),
                "[1]".to_string()
            ]
        );
    }

    #[test]
    fn test_chain_method_returning_array() {
        // 测试解析 getMatrix()[0][1].
        let segments = parse_chain_from_expr("getMatrix()[0][1]");
        let names: Vec<String> = segments.into_iter().map(|s| s.name).collect();
        assert_eq!(
            names,
            vec![
                "getMatrix".to_string(),
                "[0]".to_string(),
                "[1]".to_string()
            ]
        );
    }
}
