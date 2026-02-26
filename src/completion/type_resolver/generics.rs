use crate::completion::type_resolver::type_name::TypeName;

#[derive(Debug, Clone, PartialEq)]
pub enum JvmType {
    Object(String, Vec<JvmType>),      // e.g. "java/util/List", [String]
    TypeVar(String),                   // e.g. "T", "E"
    Array(Box<JvmType>),               // e.g. String[]
    Primitive(char),                   // e.g. 'I'
    Wildcard,                          // e.g. *
    WildcardBound(char, Box<JvmType>), // e.g. + (extends) or - (super)
}

impl JvmType {
    /// Parse the JVM signature, for example, `Ljava/util/List<Ljava/lang/String;>;`
    pub fn parse(s: &str) -> Option<(Self, &str)> {
        let first = s.chars().next()?;
        match first {
            'L' => {
                let mut i = 1;
                let bytes = s.as_bytes();
                while i < bytes.len() && bytes[i] != b'<' && bytes[i] != b';' {
                    i += 1;
                }
                let internal_name = &s[1..i];
                let mut args = Vec::new();
                let mut rest = &s[i..];

                if rest.starts_with('<') {
                    rest = &rest[1..];
                    while !rest.starts_with('>') {
                        let (arg, next_rest) = JvmType::parse(rest)?;
                        args.push(arg);
                        rest = next_rest;
                    }
                    rest = &rest[1..]; // Consume '>'
                }
                if rest.starts_with(';') {
                    rest = &rest[1..]; // Consume ';'
                }
                Some((JvmType::Object(internal_name.to_string(), args), rest))
            }
            'T' => {
                let end = s.find(';')?;
                Some((JvmType::TypeVar(s[1..end].to_string()), &s[end + 1..]))
            }
            '[' => {
                let (inner, rest) = JvmType::parse(&s[1..])?;
                Some((JvmType::Array(Box::new(inner)), rest))
            }
            '*' => Some((JvmType::Wildcard, &s[1..])),
            '+' | '-' => {
                let (inner, rest) = JvmType::parse(&s[1..])?;
                Some((JvmType::WildcardBound(first, Box::new(inner)), rest))
            }
            'V' | 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => {
                Some((JvmType::Primitive(first), &s[1..]))
            }
            _ => None,
        }
    }

    /// Replace generic variables, for example, replace `T` with the actual `Ljava/lang/String;`
    pub fn substitute(&self, type_params: &[String], type_args: &[JvmType]) -> Self {
        match self {
            JvmType::TypeVar(name) => {
                if let Some(pos) = type_params.iter().position(|p| p == name)
                    && pos < type_args.len()
                {
                    return type_args[pos].clone();
                }
                self.clone()
            }
            JvmType::Object(name, args) => {
                let new_args = args
                    .iter()
                    .map(|a| a.substitute(type_params, type_args))
                    .collect();
                JvmType::Object(name.clone(), new_args)
            }
            JvmType::Array(inner) => {
                JvmType::Array(Box::new(inner.substitute(type_params, type_args)))
            }
            JvmType::WildcardBound(c, inner) => {
                JvmType::WildcardBound(*c, Box::new(inner.substitute(type_params, type_args)))
            }
            _ => self.clone(),
        }
    }

    /// Convert to the format used internally by TypeResolver: `java/util/List<Ljava/lang/String;>`
    pub fn to_type_name(&self) -> TypeName {
        match self {
            JvmType::Object(name, args) => {
                if args.is_empty() {
                    TypeName::new(name)
                } else {
                    let arg_strs: Vec<_> = args.iter().map(|a| a.to_type_name().0).collect();
                    TypeName::new(format!("{}<{}>", name, arg_strs.join("")))
                }
            }
            JvmType::TypeVar(name) => TypeName::new(name),
            JvmType::Array(inner) => inner.to_type_name().wrap_array(),
            JvmType::Wildcard => TypeName::new("*"),
            JvmType::WildcardBound(c, inner) => {
                let kw = match c {
                    '+' => "? extends ",
                    '-' => "? super ",
                    _ => "",
                };
                TypeName::new(format!("{}{}", kw, inner.to_type_name().as_str()))
            }
            JvmType::Primitive(c) => TypeName::new(java_primitive_char_to_name(*c)),
        }
    }

    pub fn to_internal_name_string(&self) -> String {
        self.to_type_name().0
    }

    /// Convert to the standard JVM signature format: `Ljava/util/List<Ljava/lang/String;>;`
    pub fn to_signature_string(&self) -> String {
        match self {
            JvmType::Object(name, args) => {
                if args.is_empty() {
                    format!("L{};", name)
                } else {
                    let arg_strs: Vec<_> = args.iter().map(|a| a.to_signature_string()).collect();
                    format!("L{}<{}>;", name, arg_strs.join(""))
                }
            }
            JvmType::TypeVar(name) => format!("T{};", name),
            JvmType::Array(inner) => format!("[{}", inner.to_signature_string()),
            JvmType::Wildcard => "*".to_string(),
            JvmType::WildcardBound(c, inner) => format!("{}{}", c, inner.to_signature_string()),
            JvmType::Primitive(c) => c.to_string(),
        }
    }
}

// Extract generic parameters from the current variable type
// For example, extract base="java/util/List" and args=[String] from "java/util/List<Ljava/lang/String;>".
pub fn split_internal_name(internal: &str) -> (&str, Vec<JvmType>) {
    if let Some(pos) = internal.find('<') {
        let base = &internal[..pos];
        let args_str = &internal[pos + 1..internal.len() - 1]; // "Ljava/lang/String;"
        let mut args = Vec::new();
        let mut rest = args_str;
        while !rest.is_empty() {
            if let Some((ty, next_rest)) = JvmType::parse(rest) {
                args.push(ty);
                rest = next_rest;
            } else {
                // If parsing fails, you must exit to prevent an infinite loop.
                break;
            }
        }
        (base, args)
    } else {
        (internal, vec![])
    }
}

/// Extract the declared generic parameter names from the class's JVM signature.
/// Example: "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/lang/Object;" -> ["K", "V"]
pub fn parse_class_type_parameters(signature: &str) -> Vec<String> {
    let mut params = Vec::new();
    if !signature.starts_with('<') {
        return params;
    }

    let mut depth = 0;
    let mut end_angle = 0;
    for (i, c) in signature.char_indices() {
        if c == '<' {
            depth += 1;
        } else if c == '>' {
            depth -= 1;
            if depth == 0 {
                end_angle = i;
                break;
            }
        }
    }
    if end_angle == 0 {
        return params;
    }

    let mut rest = &signature[1..end_angle];
    while !rest.is_empty() {
        if let Some(colon_pos) = rest.find(':') {
            let param_name = rest[..colon_pos].trim();
            if !param_name.is_empty() {
                params.push(param_name.to_string());
            }

            rest = &rest[colon_pos + 1..];
            // 跳过泛型约束(Bounds)直到遇到 ';'
            let mut bound_depth = 0;
            let mut next_start = rest.len();
            for (i, c) in rest.char_indices() {
                match c {
                    '<' => bound_depth += 1,
                    '>' => bound_depth -= 1,
                    ';' if bound_depth == 0 => {
                        next_start = i + 1;
                        break;
                    }
                    _ => {}
                }
            }
            rest = &rest[next_start..];
            // 处理多重约束 (如 ::Ljava/lang/Comparable;)
            while rest.starts_with(':') {
                rest = &rest[1..];
                // 再次查找结束符...
                let mut bound_depth = 0;
                let mut inner_next = rest.len();
                for (i, c) in rest.char_indices() {
                    if c == '<' {
                        bound_depth += 1;
                    } else if c == '>' {
                        bound_depth -= 1;
                    } else if c == ';' && bound_depth == 0 {
                        inner_next = i + 1;
                        break;
                    }
                }
                rest = &rest[inner_next..];
            }
        } else {
            break;
        }
    }
    params
}

/// 执行类型替换。如果 receiver_internal 包含泛型（如 List<LString;>），则尝试将 target_jvm_type (如 TE;) 替换为 String。
pub fn substitute_type(
    receiver_internal: &str,
    class_generic_signature: Option<&str>,
    target_jvm_type_str: &str,
) -> Option<TypeName> {
    let (_, receiver_type_args) = split_internal_name(receiver_internal);
    if receiver_type_args.is_empty() {
        return None;
    }

    let class_type_params = class_generic_signature
        .map(parse_class_type_parameters)
        .unwrap_or_default();

    if class_type_params.is_empty() {
        return None;
    }

    let (mut ret_jvm_type, _) = JvmType::parse(target_jvm_type_str)?;
    ret_jvm_type = ret_jvm_type.substitute(&class_type_params, &receiver_type_args);

    Some(ret_jvm_type.to_type_name())
}

fn java_primitive_char_to_name(c: char) -> &'static str {
    match c {
        'I' => "int",
        'Z' => "boolean",
        'J' => "long",
        'F' => "float",
        'D' => "double",
        'B' => "byte",
        'C' => "char",
        'S' => "short",
        'V' => "void",
        _ => "unknown",
    }
}

#[cfg(test)]
mod tests {
    use crate::completion::type_resolver::generics::JvmType;

    #[test]
    fn test_wildcard_bound_display() {
        let ty = JvmType::WildcardBound(
            '-',
            Box::new(JvmType::Object("java/lang/String".to_string(), vec![])),
        );
        assert_eq!(ty.to_internal_name_string(), "? super java/lang/String");
    }

    #[test]
    fn test_type_var_display() {
        let ty = JvmType::TypeVar("E".to_string());
        assert_eq!(ty.to_internal_name_string(), "E");
        // 确保不输出原始 JVM 格式 "TE;"
    }

    #[test]
    fn test_array_of_type_var_display() {
        // toArray(T[]) 的情形
        let ty = JvmType::Array(Box::new(JvmType::TypeVar("T".to_string())));
        assert_eq!(ty.to_internal_name_string(), "T[]");
    }
}
