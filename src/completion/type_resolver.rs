use super::context::LocalVar;
use crate::{
    completion::type_resolver::{
        generics::{JvmType, split_internal_name, substitute_type},
        type_name::TypeName,
    },
    index::{GlobalIndex, MethodSummary},
};
use std::sync::Arc;

pub mod generics;
pub mod type_name;

pub struct TypeResolver<'idx> {
    index: &'idx GlobalIndex,
}

impl<'idx> TypeResolver<'idx> {
    pub fn new(index: &'idx GlobalIndex) -> Self {
        Self { index }
    }

    pub fn resolve(
        &self,
        expr: &str,
        locals: &[LocalVar],
        enclosing: Option<&Arc<str>>,
    ) -> Option<TypeName> {
        let expr = expr.trim();

        // array init
        if expr.ends_with(']')
            && let Some(bracket_idx) = expr.rfind('[')
        {
            let array_expr = expr[..bracket_idx].trim();
            if !array_expr.is_empty()
                && let Some(array_type) = self.resolve(array_expr, locals, enclosing)
            {
                return array_type.element_type();
            }
        }

        // `this`
        if expr == "this" {
            return enclosing.map(|arc| TypeName::new(arc.to_string()));
        }

        // Strings
        if expr.starts_with('"') {
            return Some(TypeName::new("java/lang/String"));
        }

        // Local variables take precedence over literals in the evaluation
        if let Some(lv) = locals.iter().find(|lv| lv.name.as_ref() == expr) {
            return Some(lv.type_internal.clone());
        }

        if let Some(enc) = enclosing {
            for class in self.index.mro(enc) {
                if let Some(f) = class.fields.iter().find(|f| f.name.as_ref() == expr) {
                    if let Some(ty) = singleton_descriptor_to_type(&f.descriptor) {
                        return Some(TypeName::new(ty));
                    } else {
                        return parse_single_type_to_internal(&f.descriptor);
                    }
                }
            }
        }

        // Class name (index lookup)
        if self.index.get_class(expr).is_some() {
            return Some(TypeName::new(expr));
        }

        // Literal checks should be placed last, with strict numeric prefix validation.
        if expr.parse::<i64>().is_ok() {
            return Some(TypeName::new("int"));
        }
        if let Some(prefix) = expr.strip_suffix('L').or_else(|| expr.strip_suffix('l'))
            && prefix.chars().all(|c| c.is_ascii_digit())
            && !prefix.is_empty()
        {
            return Some(TypeName::new("long"));
        }
        if let Some(prefix) = expr.strip_suffix('f').or_else(|| expr.strip_suffix('F'))
            && prefix.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !prefix.is_empty()
        {
            return Some(TypeName::new("float"));
        }
        if let Some(prefix) = expr.strip_suffix('d').or_else(|| expr.strip_suffix('D'))
            && prefix.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !prefix.is_empty()
        {
            return Some(TypeName::new("double"));
        }

        // Pure decimal (no suffix)
        if expr.contains('.')
            && expr.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !expr.starts_with('.')
            && !expr.ends_with('.')
        {
            return Some(TypeName::new("double"));
        }

        None
    }

    pub fn resolve_method_return(
        &self,
        receiver_internal: &str,
        method_name: &str,
        arg_count: i32,
        arg_types: &[TypeName],
    ) -> Option<TypeName> {
        let (base_receiver, _receiver_type_args) = split_internal_name(receiver_internal);

        // Use base_receiver to find MRO in the index
        for class in self.index.mro(base_receiver) {
            let candidates: Vec<&MethodSummary> = class
                .methods
                .iter()
                .filter(|m| m.name.as_ref() == method_name)
                .collect();
            if candidates.is_empty() {
                continue;
            }

            let method = Self::select_overload(&candidates, arg_count, arg_types);
            let sig = method
                .generic_signature
                .as_deref()
                .unwrap_or(&method.descriptor);

            let ret_idx = sig.find(')')?;
            let ret_jvm_str = &sig[ret_idx + 1..];
            let (ret_jvm_type, _) = JvmType::parse(&sig[ret_idx + 1..])?;

            if let Some(substituted) = substitute_type(
                receiver_internal,
                class.generic_signature.as_deref(),
                ret_jvm_str,
            ) {
                if substituted.as_str() == "void" {
                    return None;
                }
                return Some(substituted);
            }

            if let JvmType::Primitive('V') = ret_jvm_type {
                return None;
            }
            return Some(ret_jvm_type.to_type_name());
        }
        None
    }

    fn select_overload<'a>(
        candidates: &[&'a MethodSummary],
        arg_count: i32,
        arg_types: &[TypeName],
    ) -> &'a MethodSummary {
        match candidates.len() {
            1 => candidates[0],
            _ => {
                if arg_count >= 0 {
                    let by_count: Vec<&MethodSummary> = candidates
                        .iter()
                        .copied()
                        .filter(|m| count_params(&m.descriptor) == arg_count as usize)
                        .collect();
                    match by_count.len() {
                        0 => candidates[0],
                        1 => by_count[0],
                        _ if !arg_types.is_empty() => by_count
                            .iter()
                            .copied()
                            .find(|m| params_match(&m.descriptor, arg_types))
                            .unwrap_or(by_count[0]),
                        _ => by_count[0],
                    }
                } else {
                    candidates[0]
                }
            }
        }
    }

    pub fn resolve_chain(
        &self,
        chain: &[ChainSegment],
        locals: &[LocalVar],
        enclosing_internal_name: Option<&Arc<str>>,
    ) -> Option<TypeName> {
        let mut current_type: Option<TypeName> = None;
        for (i, seg) in chain.iter().enumerate() {
            if i == 0 {
                if seg.arg_count.is_some() {
                    // Bare method call: receiver is the enclosing class
                    let recv = enclosing_internal_name?;
                    current_type = self.resolve_method_return(
                        recv.as_ref(),
                        &seg.name,
                        seg.arg_count.unwrap_or(-1),
                        &seg.arg_types,
                    );
                } else {
                    current_type = self.resolve(&seg.name, locals, enclosing_internal_name);
                }
            } else {
                let receiver = current_type.as_ref()?;

                // 处理连缀的数组下标，例如 `getCharArr()[0]` 解析出的独立 segment `[0]`
                if seg.arg_count.is_none() && seg.name.starts_with('[') && seg.name.ends_with(']') {
                    let dimensions = seg.name.matches('[').count();
                    let mut arr_ty = receiver.clone();
                    for _ in 0..dimensions {
                        arr_ty = arr_ty.element_type()?; // 直接用 TypeName::element_type()
                    }
                    current_type = Some(arr_ty);
                    continue;
                }

                // 常规方法或字段访问，尝试剥离可能附着的数组下标 e.g., `field[0]`
                let bracket_idx = seg.name.find('[').unwrap_or(seg.name.len());
                let actual_name = &seg.name[..bracket_idx];
                let dimensions = seg.name[bracket_idx..].matches('[').count();

                if seg.arg_count.is_some() {
                    // 方法返回
                    current_type = self.resolve_method_return(
                        receiver.as_str(),
                        actual_name,
                        seg.arg_count.unwrap_or(-1),
                        &seg.arg_types,
                    );
                } else {
                    // 字段读取
                    let mut found_field: Option<TypeName> = None;
                    for class in self.index.mro(receiver.as_str()) {
                        if let Some(f) =
                            class.fields.iter().find(|f| f.name.as_ref() == actual_name)
                        {
                            if let Some(ty) = singleton_descriptor_to_type(&f.descriptor) {
                                found_field = Some(TypeName::new(ty));
                            } else {
                                found_field = parse_single_type_to_internal(&f.descriptor);
                            }
                            break;
                        }
                    }
                    current_type = found_field;
                }

                // 如果带有下标附着，则降维
                if dimensions > 0
                    && let Some(ty) = current_type
                {
                    let mut arr_ty = ty;
                    for _ in 0..dimensions {
                        arr_ty = arr_ty.element_type()?;
                    }
                    current_type = Some(arr_ty);
                }
            }
        }
        current_type
    }
}

/// Check if the descriptor's parameter types match the given arg_types.
/// arg_types are internal names (e.g. "java/lang/String", "long", "int").
/// Matching is best-effort: primitive types and object types are compared.
fn params_match(descriptor: &str, arg_types: &[TypeName]) -> bool {
    let inner = match descriptor.find('(').zip(descriptor.find(')')) {
        Some((l, r)) => &descriptor[l + 1..r],
        None => return false,
    };

    let mut param_descs = Vec::new();
    let mut s = inner;
    while !s.is_empty() {
        let (ty, rest) = consume_one_descriptor_type(s);
        param_descs.push(ty);
        s = rest;
    }

    if param_descs.len() != arg_types.len() {
        return false;
    }

    param_descs
        .iter()
        .zip(arg_types.iter())
        .all(|(desc, arg_ty)| singleton_descriptor_matches_type(desc, arg_ty.as_str()))
}

/// Convert JVM descriptor into source code style type name
///
/// # Examples
/// - "I" -> "int"
/// - "[B" -> "byte[]"
/// - "[[J" -> "long[][]"
/// - "Ljava/lang/String;" -> "java/lang/String"
/// - "[Ljava/lang/String;" -> "java/lang/String[]"
pub fn descriptor_to_source_code_style_type(desc: &str) -> String {
    let mut array_depth = 0;
    let mut s = desc;
    while s.starts_with('[') {
        array_depth += 1;
        s = &s[1..];
    }

    let base_type = match s {
        "B" => "byte",
        "C" => "char",
        "D" => "double",
        "F" => "float",
        "I" => "int",
        "J" => "long",
        "S" => "short",
        "Z" => "boolean",
        "V" => "void",
        _ if s.starts_with('L') && s.ends_with(';') => &s[1..s.len() - 1],
        // unknown type
        _ => s,
    };

    let mut result = String::with_capacity(base_type.len() + array_depth * 2);
    result.push_str(base_type);
    for _ in 0..array_depth {
        result.push_str("[]");
    }

    result
}

pub fn parse_method_descriptor(descriptor: &str) -> Vec<String> {
    let inner = match descriptor.find('(').zip(descriptor.find(')')) {
        Some((l, r)) => &descriptor[l + 1..r],
        None => return Vec::new(),
    };

    let mut types = Vec::new();
    let mut s = inner;

    while !s.is_empty() {
        let (ty_desc, rest) = consume_one_descriptor_type(s);
        types.push(descriptor_to_source_code_style_type(ty_desc));
        s = rest;
    }

    types
}

/// Converts a singleton descriptor to an internal type name
pub(crate) fn singleton_descriptor_to_type(desc: &str) -> Option<&str> {
    match desc {
        "B" => Some("byte"),
        "C" => Some("char"),
        "D" => Some("double"),
        "F" => Some("float"),
        "I" => Some("int"),
        "J" => Some("long"),
        "S" => Some("short"),
        "Z" => Some("boolean"),
        "V" => Some("void"),
        _ if desc.starts_with('L') && desc.ends_with(';') => Some(&desc[1..desc.len() - 1]),
        _ => None,
    }
}

/// Compare a single parameter descriptor against an inferred type internal name.
fn singleton_descriptor_matches_type(desc: &str, ty: &str) -> bool {
    let Some(resolved_ty) = singleton_descriptor_to_type(desc) else {
        return false;
    };

    if resolved_ty == ty {
        return true;
    }

    if desc.starts_with('L') {
        let s1 = resolved_ty.rsplit('/').next();
        let s2 = ty.rsplit('/').next();

        return s1.is_some() && s1 == s2;
    }

    false
}

fn consume_one_descriptor_type(s: &str) -> (&str, &str) {
    match s.chars().next() {
        Some('L') => {
            if let Some(end) = s.find(';') {
                (&s[..=end], &s[end + 1..])
            } else {
                (s, "")
            }
        }
        Some('[') => {
            let (_, rest) = consume_one_descriptor_type(&s[1..]);
            let consumed = s.len() - rest.len();
            (&s[..consumed], rest)
        }
        Some(_) => (&s[..1], &s[1..]),
        None => ("", ""),
    }
}

#[derive(Debug, Clone)]
pub struct ChainSegment {
    /// Variable name or method name
    pub name: String,
    /// If it's a method call, specify the number of arguments; if it's a field/variable, specify None.
    pub arg_count: Option<i32>,
    /// Inferred types of arguments (internal names), used for overload resolution.
    pub arg_types: Vec<TypeName>,
    pub arg_texts: Vec<String>, // raw text of each argument
}

impl ChainSegment {
    pub fn variable(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            arg_count: None,
            arg_types: vec![],
            arg_texts: vec![],
        }
    }
    pub fn method(name: impl Into<String>, arg_count: i32) -> Self {
        Self {
            name: name.into(),
            arg_count: Some(arg_count),
            arg_types: vec![],
            arg_texts: vec![],
        }
    }
    pub fn method_with_types(
        name: impl Into<String>,
        arg_count: i32,
        arg_types: Vec<TypeName>,
        arg_texts: Vec<String>,
    ) -> Self {
        Self {
            name: name.into(),
            arg_count: Some(arg_count),
            arg_types,
            arg_texts,
        }
    }
}

/// Extract the internal name of the return type from the method descriptor or generic signature
///
// Input example:
/// - `"(I)Ljava/util/List;"` → `Some("java/util/List")`
/// - `"(I)Ljava/util/List<Ljava/lang/String;>;"` → `Some("java/util/List")` (after generic erasure)
/// - `"()V"` → `None` (void)
/// - `"()I"` → `None` (primitive type, no further dot chaining)
/// - `"()[Ljava/lang/String;"` → `Some("[Ljava/lang/String;")` (array type)
pub fn extract_return_internal_name(descriptor: &str) -> Option<TypeName> {
    let ret_idx = descriptor.find(')')?;
    let ret_str = &descriptor[ret_idx + 1..];

    let (jvm_type, _) = JvmType::parse(ret_str)?;

    if let JvmType::Primitive('V') = jvm_type {
        return None;
    }

    Some(jvm_type.to_type_name())
}

/// Convert a single type descriptor to an internal name (remove generic parameters, retain the primitive type)
///
/// - `"Ljava/util/List;"` → `Some("java/util/List")`
/// - `"Ljava/util/List<Ljava/lang/String;>;"` → `Some("java/util/List")`
/// - `"[Ljava/lang/String;"` → `Some("[Ljava/lang/String;")` (Arrays retain complete descriptors)
/// - Primitive types such as `"V"` / `"I"` / `"Z"` → `None`
pub fn parse_single_type_to_internal(ty: &str) -> Option<TypeName> {
    let (jvm_type, _) = JvmType::parse(ty)?;
    match jvm_type {
        JvmType::Primitive(_) => None,
        _ => Some(jvm_type.to_type_name()),
    }
}

/// Counts the number of parameters in the method descriptor
///
/// `"(ILjava/lang/String;[B)V"` → 3
pub fn count_params(descriptor: &str) -> usize {
    let inner = match descriptor.find('(').zip(descriptor.find(')')) {
        Some((l, r)) => &descriptor[l + 1..r],
        None => return 0,
    };
    let mut count = 0;
    let mut s = inner;
    while !s.is_empty() {
        // Skip array dimensions
        let s_trimmed = s.trim_start_matches('[');
        match s_trimmed.chars().next() {
            Some('L') => {
                // Object type, find ';'
                if let Some(end) = s_trimmed.find(';') {
                    s = &s_trimmed[end + 1..];
                } else {
                    break;
                }
            }
            Some(_) => {
                // primitive type (single char)
                let skipped = s.len() - s_trimmed.len() + 1;
                s = &s[skipped..];
            }
            None => break,
        }
        count += 1;
    }
    count
}

/// Extract return type from descriptor
pub fn parse_return_type_from_descriptor(descriptor: &str) -> Option<Arc<str>> {
    extract_return_internal_name(descriptor).map(|t| t.to_arc())
}

pub fn java_primitive_char_to_name(c: char) -> &'static str {
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

/// Java source code type name → JVM descriptor
pub fn java_source_type_to_descriptor(ty: &str) -> String {
    // Remove generics
    let base = ty.split('<').next().unwrap_or(ty).trim();
    match base {
        "void" => "V".into(),
        "boolean" => "Z".into(),
        "byte" => "B".into(),
        "char" => "C".into(),
        "short" => "S".into(),
        "int" => "I".into(),
        "long" => "J".into(),
        "float" => "F".into(),
        "double" => "D".into(),
        other => {
            let internal = other.replace('.', "/");
            format!("L{};", internal)
        }
    }
}

/// Java source code type name → JVM descriptor (used for fields)
pub fn java_type_to_descriptor(ty: &str) -> String {
    let base = ty.split('<').next().unwrap_or(ty).trim();
    let array_depth = ty.chars().filter(|&c| c == '[').count();
    let brackets: String = (0..array_depth).map(|_| '[').collect();
    format!("{}{}", brackets, java_source_type_to_descriptor(base))
}

/// 将 Java 源码类型转换为携带泛型的 JVM internal name
///
/// # Examples
/// - `List<String>` -> `java/util/List<Ljava/lang/String;>`
/// - `Map<String, List<User>>` -> `java/util/Map<Ljava/lang/String;Ljava/util/List<Luser/User;>;>`
/// - `String[]` -> `[Ljava/lang/String;`
pub fn java_source_type_to_jvm_generic(
    source_ty: &str,
    resolve_simple_name: &impl Fn(&str) -> String,
) -> String {
    let mut ty = source_ty.trim();

    // 1. 处理数组后缀 (String[] -> array_depth = 1)
    let mut array_depth = 0;
    while let Some(stripped) = ty.strip_suffix("[]") {
        array_depth += 1;
        ty = stripped.trim();
    }

    // 2. 处理泛型
    let mut result = if let Some(pos) = ty.find('<') {
        if ty.ends_with('>') {
            let base = &ty[..pos];
            let args_str = &ty[pos + 1..ty.len() - 1];
            // 解析基类：List -> java/util/List
            let base_internal = resolve_simple_name(base.trim());

            // 按逗号分割泛型参数，这里必须忽略嵌套的 <> (例如 Map<String, List<User>>)
            let mut args = Vec::new();
            let mut depth = 0;
            let mut start = 0;
            for (i, c) in args_str.char_indices() {
                match c {
                    '<' => depth += 1,
                    '>' => depth -= 1,
                    ',' if depth == 0 => {
                        args.push(&args_str[start..i]);
                        start = i + 1;
                    }
                    _ => {}
                }
            }
            args.push(&args_str[start..]);

            // 递归转换内部参数，并包装成 L...;
            let resolved_args: Vec<_> = args
                .into_iter()
                .map(|a| {
                    let arg_ty = a.trim();
                    if arg_ty == "?" {
                        return "*".to_string(); // 通配符
                    }
                    let inner = java_source_type_to_jvm_generic(arg_ty, resolve_simple_name);

                    // 如果 inner 已经是数组（以 '[' 开头），就不要再套 'L' 了
                    if inner.starts_with('[') {
                        inner
                    } else {
                        format!("L{};", inner)
                    }
                })
                .collect();

            format!("{}<{}>", base_internal, resolved_args.join(""))
        } else {
            resolve_simple_name(ty)
        }
    } else {
        resolve_simple_name(ty)
    };

    // 3. 数组包装恢复
    for _ in 0..array_depth {
        if !result.starts_with('[') {
            result = format!("L{};", result);
        }
        result = format!("[{}", result);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::index::GlobalIndex;

    fn make_resolver() -> (GlobalIndex, Vec<LocalVar>) {
        let idx = GlobalIndex::new();
        let locals = vec![
            LocalVar {
                name: Arc::from("cl"),
                type_internal: TypeName::new("RandomClass"),
                init_expr: None,
            },
            LocalVar {
                name: Arc::from("sf"),
                type_internal: TypeName::new("float"),
                init_expr: None,
            },
            LocalVar {
                name: Arc::from("result"),
                type_internal: TypeName::new("java/lang/String"),
                init_expr: None,
            },
            LocalVar {
                name: Arc::from("myList"),
                type_internal: TypeName::new("java/util/List"),
                init_expr: None,
            },
        ];
        (idx, locals)
    }

    #[test]
    fn test_variable_ending_with_l_not_confused_with_long() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        // "cl" ends with 'l', but it's a local variable and shouldn't be evaluated as long.
        assert_eq!(
            r.resolve("cl", &locals, None).as_deref(),
            Some("RandomClass"),
            "'cl' should resolve to RandomClass, not long"
        );
    }

    #[test]
    fn test_variable_ending_with_f_not_confused_with_float() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        // "sf" ends with 'f', but it's a local variable
        assert_eq!(
            r.resolve("sf", &locals, None).as_deref(),
            Some("float"), // float is the variable's type, not a literal value.
            "'sf' should resolve to its declared type"
        );
    }

    #[test]
    fn test_long_literal_recognized() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(r.resolve("123L", &locals, None).as_deref(), Some("long"));
        assert_eq!(r.resolve("0l", &locals, None).as_deref(), Some("long"));
        assert_eq!(r.resolve("999L", &locals, None).as_deref(), Some("long"));
    }

    #[test]
    fn test_float_literal_recognized() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(r.resolve("1.5f", &locals, None).as_deref(), Some("float"));
        assert_eq!(r.resolve("3F", &locals, None).as_deref(), Some("float"));
    }

    #[test]
    fn test_double_literal_recognized() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(r.resolve("1.5d", &locals, None).as_deref(), Some("double"));
        assert_eq!(r.resolve("3.14", &locals, None).as_deref(), Some("double"));
    }

    #[test]
    fn test_int_literal_recognized() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(r.resolve("42", &locals, None).as_deref(), Some("int"));
        assert_eq!(r.resolve("0", &locals, None).as_deref(), Some("int"));
    }

    #[test]
    fn test_string_literal_recognized() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(
            r.resolve("\"hello\"", &locals, None).as_deref(),
            Some("java/lang/String")
        );
    }

    #[test]
    fn test_this_resolves_to_enclosing() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        let enclosing = Arc::from("org/cubewhy/Main");
        assert_eq!(
            r.resolve("this", &locals, Some(&enclosing)).as_deref(),
            Some("org/cubewhy/Main")
        );
    }

    #[test]
    fn test_unknown_expr_returns_none() {
        let (idx, locals) = make_resolver();
        let r = TypeResolver::new(&idx);
        assert_eq!(r.resolve("unknownVar", &locals, None), None);
    }

    #[test]
    fn test_local_var_takes_priority_over_literal_heuristic() {
        // Even if the variable name looks like a literal, local variable lookup takes precedence.
        let idx = GlobalIndex::new();
        let locals = vec![
            // Extreme case: The variable name is "123" (invalid in Java, but with test priority)
            LocalVar {
                name: Arc::from("myL"),
                type_internal: TypeName::new("SomeClass"),
                init_expr: None,
            },
        ];
        let r = TypeResolver::new(&idx);
        // "myL" ends with 'L' but is not a numeric prefix, and should not be recognized as long.
        assert_eq!(
            r.resolve("myL", &locals, None).as_deref(),
            Some("SomeClass")
        );
    }

    #[test]
    fn test_resolve_method_return_overload_by_type_long() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("NestedClass"),
            internal_name: Arc::from("NestedClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![
                MethodSummary {
                    name: Arc::from("randomFunction"),
                    descriptor: Arc::from("(Ljava/lang/String;I)LRandomClass;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("RandomClass")),
                },
                MethodSummary {
                    name: Arc::from("randomFunction"),
                    descriptor: Arc::from("(Ljava/lang/String;J)LMain2;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("Main2")),
                },
            ],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let resolver = TypeResolver::new(&idx);

        // arg_types: String + long → should match (String, J) → Main2
        let result = resolver.resolve_method_return(
            "NestedClass",
            "randomFunction",
            2,
            &[TypeName::new("java/lang/String"), TypeName::new("long")],
        );
        assert_eq!(
            result.as_deref(),
            Some("Main2"),
            "long arg should select Main2 overload"
        );

        // arg_types: String + int → RandomClass
        let result2 = resolver.resolve_method_return(
            "NestedClass",
            "randomFunction",
            2,
            &[TypeName::new("java/lang/String"), TypeName::new("int")],
        );
        assert_eq!(
            result2.as_deref(),
            Some("RandomClass"),
            "int arg should select RandomClass overload"
        );
    }

    #[test]
    fn test_params_match_primitive_long() {
        assert!(singleton_descriptor_matches_type("J", "long"));
        assert!(!singleton_descriptor_matches_type("J", "int"));
        assert!(singleton_descriptor_matches_type("I", "int"));
        assert!(!singleton_descriptor_matches_type("I", "long"));
    }

    #[test]
    fn test_params_match_object_type() {
        assert!(singleton_descriptor_matches_type(
            "Ljava/lang/String;",
            "java/lang/String"
        ));
        assert!(singleton_descriptor_matches_type(
            "Ljava/lang/String;",
            "String"
        )); // simple name match
        assert!(!singleton_descriptor_matches_type(
            "Ljava/util/List;",
            "java/lang/String"
        ));
    }

    #[test]
    fn test_resolve_chain_bare_method_call() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        // Main has getMain2() returning Main2
        // Main2 has func()
        idx.add_classes(vec![
            ClassMetadata {
                package: None,
                name: Arc::from("Main"),
                internal_name: Arc::from("Main"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("getMain2"),
                    descriptor: Arc::from("()LMain2;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("Main2")),
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
            ClassMetadata {
                package: None,
                name: Arc::from("Main2"),
                internal_name: Arc::from("Main2"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("func"),
                    descriptor: Arc::from("()V"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: None,
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
        ]);

        let resolver = TypeResolver::new(&idx);
        let enclosing = Arc::from("Main");

        // "getMain2()" → chain = [method("getMain2", 0)]
        let chain = crate::completion::engine::parse_chain_from_expr("getMain2()");
        let result = resolver.resolve_chain(&chain, &[], Some(&enclosing));
        assert_eq!(
            result.as_deref(),
            Some("Main2"),
            "bare method call should resolve via enclosing class"
        );
    }

    #[test]
    fn test_descriptor_to_type_intellij_style() {
        // primitive arrays
        assert_eq!(descriptor_to_source_code_style_type("[I"), "int[]");
        assert_eq!(descriptor_to_source_code_style_type("[[D"), "double[][]");

        // objects
        assert_eq!(
            descriptor_to_source_code_style_type("Ljava/lang/String;"),
            "java/lang/String"
        );

        // object arrays
        assert_eq!(
            descriptor_to_source_code_style_type("[Ljava/lang/Object;"),
            "java/lang/Object[]"
        );
        assert_eq!(
            descriptor_to_source_code_style_type("[[Ljava/util/List;"),
            "java/util/List[][]"
        );
    }

    #[test]
    fn test_parse_method() {
        // void method(int[], String[][])
        let desc = "([I[[Ljava/lang/String;)V";
        let params = parse_method_descriptor(desc);

        assert_eq!(params, vec!["int[]", "java/lang/String[][]"]);
    }

    #[test]
    fn test_generics_substitution_list_get() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/util")),
            name: Arc::from("List"),
            internal_name: Arc::from("java/util/List"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("get"),
                descriptor: Arc::from("(I)Ljava/lang/Object;"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                // 这里代表泛型方法返回类型是 E
                generic_signature: Some(Arc::from("(I)TE;")),
                return_type: None,
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            generic_signature: Some(Arc::from("<E:Ljava/lang/Object;>Ljava/lang/Object;")),
            origin: ClassOrigin::Unknown,
        }]);

        let resolver = TypeResolver::new(&idx);

        // 模拟推导 `myList.get()`
        // receiver 是我们带有泛型尾巴的完整形式
        let result = resolver.resolve_method_return(
            "java/util/List<Ljava/lang/String;>",
            "get",
            1,
            &[TypeName::new("int")],
        );

        assert_eq!(
            result.as_deref(),
            Some("java/lang/String"),
            "Generic type TE; should be correctly substituted to java/lang/String"
        );
    }

    #[test]
    fn test_resolve_variable_array_access() {
        // 模拟 `var c = arr[0];` 场景
        let (idx, locals) = make_resolver();
        let mut locals = locals;
        locals.push(LocalVar {
            name: Arc::from("arr"),
            type_internal: TypeName::new("char[]"),
            init_expr: None,
        });

        let resolver = TypeResolver::new(&idx);
        let result = resolver.resolve("arr[0]", &locals, None);
        assert_eq!(result.as_deref(), Some("char"));
    }

    #[test]
    fn test_resolve_array_after_method_call() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("Main"),
            internal_name: Arc::from("Main"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("getCharArr"),
                descriptor: Arc::from("()[[C"), // 代表方法返回 char[][]
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: None, // 基本数据类型数组这里通常为 None
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let resolver = TypeResolver::new(&idx);
        let enclosing = Arc::from("Main");

        // 模拟解析连缀调用 `getCharArr()[0]`
        let chain = crate::completion::engine::parse_chain_from_expr("getCharArr()[0]");
        let result = resolver.resolve_chain(&chain, &[], Some(&enclosing));

        // char[][] 提取出一层下标后应为 char[]，底层 JVM internal_name 表示为 `[C`
        assert_eq!(result.as_deref(), Some("char[]"));
    }
}
