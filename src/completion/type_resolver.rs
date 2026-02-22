use super::context::LocalVar;
use crate::index::{GlobalIndex, MethodSummary};
use std::sync::Arc;

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
    ) -> Option<Arc<str>> {
        let expr = expr.trim();

        // `this`
        if expr == "this" {
            return enclosing.cloned();
        }

        // Strings
        if expr.starts_with('"') {
            return Some(Arc::from("java/lang/String"));
        }

        // Local variables take precedence over literals in the evaluation
        if let Some(lv) = locals.iter().find(|lv| lv.name.as_ref() == expr) {
            return Some(Arc::clone(&lv.type_internal));
        }

        // Class name (index lookup)
        if self.index.get_class(expr).is_some() {
            return Some(Arc::from(expr));
        }

        // Literal checks should be placed last, with strict numeric prefix validation.
        if expr.parse::<i64>().is_ok() {
            return Some(Arc::from("int"));
        }
        if let Some(prefix) = expr.strip_suffix('L').or_else(|| expr.strip_suffix('l'))
            && prefix.chars().all(|c| c.is_ascii_digit())
            && !prefix.is_empty()
        {
            return Some(Arc::from("long"));
        }
        if let Some(prefix) = expr.strip_suffix('f').or_else(|| expr.strip_suffix('F'))
            && prefix.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !prefix.is_empty()
        {
            return Some(Arc::from("float"));
        }
        if let Some(prefix) = expr.strip_suffix('d').or_else(|| expr.strip_suffix('D'))
            && prefix.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !prefix.is_empty()
        {
            return Some(Arc::from("double"));
        }

        // Pure decimal (no suffix)
        if expr.contains('.')
            && expr.chars().all(|c| c.is_ascii_digit() || c == '.')
            && !expr.starts_with('.')
            && !expr.ends_with('.')
        {
            return Some(Arc::from("double"));
        }

        None
    }

    pub fn resolve_method_return(
        &self,
        receiver_internal: &str,
        method_name: &str,
        arg_count: i32,
    ) -> Option<Arc<str>> {
        let class = self.index.get_class(receiver_internal)?;

        // Collect all methods with the same name
        let candidates: Vec<&MethodSummary> = class
            .methods
            .iter()
            .filter(|m| m.name.as_ref() == method_name)
            .collect();

        let method = match candidates.len() {
            0 => return None,
            1 => candidates[0],
            _ => {
                // Multiple overloads: disambiguation based on the number of parameters
                if arg_count >= 0 {
                    candidates
                        .iter()
                        .copied()
                        .find(|m| count_params(&m.descriptor) == arg_count as usize)
                        .unwrap_or(candidates[0])
                } else {
                    candidates[0]
                }
            }
        };

        // Prioritize generic_signature (containing generic information), fall back to descriptor
        let sig = method
            .generic_signature
            .as_deref()
            .unwrap_or(&method.descriptor);

        extract_return_internal_name(sig)
    }

    pub fn resolve_chain(
        &self,
        chain: &[ChainSegment],
        locals: &[LocalVar],
        enclosing: Option<&Arc<str>>,
    ) -> Option<Arc<str>> {
        let mut current_type: Option<Arc<str>> = None;

        for (i, seg) in chain.iter().enumerate() {
            if i == 0 {
                // First section: Variable name or class name
                current_type = self.resolve(&seg.name, locals, enclosing);
            } else {
                // Subsequent section: Method call
                let receiver = current_type.as_deref()?;
                current_type =
                    self.resolve_method_return(receiver, &seg.name, seg.arg_count.unwrap_or(-1));
            }
        }

        current_type
    }
}

#[derive(Debug, Clone)]
pub struct ChainSegment {
    /// Variable name or method name
    pub name: String,
    /// If it's a method call, specify the number of arguments; if it's a field/variable, specify None.
    pub arg_count: Option<i32>,
}

impl ChainSegment {
    pub fn variable(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            arg_count: None,
        }
    }
    pub fn method(name: impl Into<String>, arg_count: i32) -> Self {
        Self {
            name: name.into(),
            arg_count: Some(arg_count),
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
pub fn extract_return_internal_name(descriptor: &str) -> Option<Arc<str>> {
    // Find the return type part after ')'
    let ret = descriptor.find(')').map(|i| &descriptor[i + 1..])?;

    parse_single_type_to_internal(ret)
}

/// Convert a single type descriptor to an internal name (remove generic parameters, retain the primitive type)
///
/// - `"Ljava/util/List;"` → `Some("java/util/List")`
/// - `"Ljava/util/List<Ljava/lang/String;>;"` → `Some("java/util/List")`
/// - `"[Ljava/lang/String;"` → `Some("[Ljava/lang/String;")` (Arrays retain complete descriptors)
/// - Primitive types such as `"V"` / `"I"` / `"Z"` → `None`
pub fn parse_single_type_to_internal(ty: &str) -> Option<Arc<str>> {
    match ty.chars().next()? {
        // Object type: L...; or L...<...>;
        'L' => {
            // Find the first ';' or '<', and take the part before it as the internal name.
            let end = ty[1..].find([';', '<'])? + 1;
            Some(Arc::from(&ty[1..end]))
        }
        // Array type: retain the complete descriptor (because arrays in GlobalIndex do not have individual entries)
        '[' => {
            // Find the type of the element after the last '['
            let element_start = ty.rfind('[').unwrap_or(0) + 1;
            let element = &ty[element_start..];
            // If the element is an object type, extract its internal name.
            if let Some(_stripped) = element.strip_prefix('L') {
                // TODO: wtf, Claude what are you doing

                // let end = stripped.find([';', '<'])? + 1;
                // Returns the fully internal name of the array, such as "[Ljava/util/List;"
                Some(Arc::from(ty))
            } else {
                // Arrays of primitive types, such as "[I"
                Some(Arc::from(ty))
            }
        }
        // void and primitive types: cannot continue the dot chain, returns None
        'V' | 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => None,
        _ => None,
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
    extract_return_internal_name(descriptor)
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
                type_internal: Arc::from("RandomClass"),
            },
            LocalVar {
                name: Arc::from("sf"),
                type_internal: Arc::from("float"),
            },
            LocalVar {
                name: Arc::from("result"),
                type_internal: Arc::from("java/lang/String"),
            },
            LocalVar {
                name: Arc::from("myList"),
                type_internal: Arc::from("java/util/List"),
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
                type_internal: Arc::from("SomeClass"),
            },
        ];
        let r = TypeResolver::new(&idx);
        // "myL" ends with 'L' but is not a numeric prefix, and should not be recognized as long.
        assert_eq!(
            r.resolve("myL", &locals, None).as_deref(),
            Some("SomeClass")
        );
    }
}
