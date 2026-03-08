use std::sync::Arc;

/// Structured internal type names, using JVM-internal base names plus generic args and array dims.
/// - Objects: base_internal = "java/lang/String"
/// - Primitive types: base_internal = "int"
/// - Arrays: array_dims > 0
/// - With generics: args = [TypeName], rendered as "java/util/List<Ljava/lang/String;>"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName {
    pub base_internal: Arc<str>,
    pub args: Vec<TypeName>,
    pub array_dims: usize,
}

impl TypeName {
    pub fn new(base_internal: impl Into<Arc<str>>) -> Self {
        let raw: Arc<str> = base_internal.into();
        let mut base = raw.as_ref().trim();
        let mut dims = 0usize;
        while let Some(stripped) = base.strip_suffix("[]") {
            dims += 1;
            base = stripped.trim_end();
        }
        TypeName {
            base_internal: Arc::from(base),
            args: Vec::new(),
            array_dims: dims,
        }
    }

    pub fn with_args(base_internal: impl Into<Arc<str>>, args: Vec<TypeName>) -> Self {
        TypeName {
            base_internal: base_internal.into(),
            args,
            array_dims: 0,
        }
    }

    pub fn with_array_dims(mut self, dims: usize) -> Self {
        self.array_dims = dims;
        self
    }

    pub fn is_array(&self) -> bool {
        self.array_dims > 0
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self.base_internal.as_ref(),
            "int" | "long" | "short" | "byte" | "char" | "float" | "double" | "boolean" | "void"
        )
    }

    pub fn has_generics(&self) -> bool {
        !self.args.is_empty()
    }

    /// Erase generic parameters and array dims for index lookup.
    pub fn erased_internal(&self) -> &str {
        &self.base_internal
    }

    /// Erase generics but keep array dims, e.g. "java/lang/String[]".
    pub fn erased_internal_with_arrays(&self) -> String {
        let mut s = self.base_internal.to_string();
        if self.array_dims > 0 {
            s.push_str(&"[]".repeat(self.array_dims));
        }
        s
    }

    pub fn contains_slash(&self) -> bool {
        self.base_internal.contains('/')
    }

    pub fn wildcard_upper_bound(&self) -> Option<&TypeName> {
        if self.base_internal.as_ref() == "+" {
            return self.args.first();
        }
        None
    }

    /// "java/lang/String[][]" → Some("java/lang/String[]")
    pub fn element_type(&self) -> Option<TypeName> {
        if self.array_dims == 0 {
            return None;
        }
        let mut next = self.clone();
        next.array_dims -= 1;
        Some(next)
    }

    /// "java/lang/String" → "java/lang/String[]"
    pub fn wrap_array(&self) -> TypeName {
        let mut next = self.clone();
        next.array_dims += 1;
        next
    }

    /// Internal style with generics, e.g. "java/util/List<Ljava/lang/String;>".
    pub fn to_internal_with_generics(&self) -> String {
        let base = self.base_internal.as_ref();
        let mut s = if self.args.is_empty() {
            base.to_string()
        } else {
            let arg_sigs: Vec<String> = self.args.iter().map(|a| a.to_jvm_signature()).collect();
            format!("{}<{}>", base, arg_sigs.join(""))
        };
        if self.array_dims > 0 {
            s.push_str(&"[]".repeat(self.array_dims));
        }
        s
    }

    /// Internal style with generics for substitution/rendering paths.
    /// Unlike `to_internal_with_generics`, this keeps non-slash class-like names
    /// as object signatures in generic arguments (e.g. `LBox;`), while retaining
    /// likely type variables (e.g. `TR;`).
    pub fn to_internal_with_generics_for_substitution(&self) -> String {
        fn is_likely_type_var(name: &str) -> bool {
            let mut chars = name.chars();
            let Some(first) = chars.next() else {
                return false;
            };
            if !first.is_ascii_uppercase() {
                return false;
            }
            if name.len() == 1 {
                return true;
            }
            chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
                && matches!(
                    first,
                    'E' | 'K' | 'R' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'
                )
        }

        fn to_jvm_sig_for_substitution(ty: &TypeName) -> String {
            let mut sig = match ty.base_internal.as_ref() {
                "byte" => "B".to_string(),
                "char" => "C".to_string(),
                "double" => "D".to_string(),
                "float" => "F".to_string(),
                "int" => "I".to_string(),
                "long" => "J".to_string(),
                "short" => "S".to_string(),
                "boolean" => "Z".to_string(),
                "void" => "V".to_string(),
                "*" => "*".to_string(),
                "+" | "-" => {
                    if let Some(inner) = ty.args.first() {
                        format!("{}{}", ty.base_internal, to_jvm_sig_for_substitution(inner))
                    } else {
                        ty.base_internal.to_string()
                    }
                }
                base if base.contains('/') => {
                    if ty.args.is_empty() {
                        format!("L{};", base)
                    } else {
                        let arg_sigs: Vec<String> =
                            ty.args.iter().map(to_jvm_sig_for_substitution).collect();
                        format!("L{}<{}>;", base, arg_sigs.join(""))
                    }
                }
                other if is_likely_type_var(other) => format!("T{};", other),
                other => {
                    if ty.args.is_empty() {
                        format!("L{};", other)
                    } else {
                        let arg_sigs: Vec<String> =
                            ty.args.iter().map(to_jvm_sig_for_substitution).collect();
                        format!("L{}<{}>;", other, arg_sigs.join(""))
                    }
                }
            };

            if ty.array_dims > 0 {
                sig = format!("{}{}", "[".repeat(ty.array_dims), sig);
            }
            sig
        }

        let base = self.base_internal.as_ref();
        let mut s = if self.args.is_empty() {
            base.to_string()
        } else {
            let arg_sigs: Vec<String> = self.args.iter().map(to_jvm_sig_for_substitution).collect();
            format!("{}<{}>", base, arg_sigs.join(""))
        };
        if self.array_dims > 0 {
            s.push_str(&"[]".repeat(self.array_dims));
        }
        s
    }

    /// JVM signature, e.g. "Ljava/util/List<Ljava/lang/String;>;" or "[I".
    pub fn to_jvm_signature(&self) -> String {
        let mut sig = match self.base_internal.as_ref() {
            "byte" => "B".to_string(),
            "char" => "C".to_string(),
            "double" => "D".to_string(),
            "float" => "F".to_string(),
            "int" => "I".to_string(),
            "long" => "J".to_string(),
            "short" => "S".to_string(),
            "boolean" => "Z".to_string(),
            "void" => "V".to_string(),
            "*" => "*".to_string(),
            "+" | "-" => {
                if let Some(inner) = self.args.first() {
                    format!("{}{}", self.base_internal, inner.to_jvm_signature())
                } else {
                    self.base_internal.to_string()
                }
            }
            base if base.contains('/') => {
                if self.args.is_empty() {
                    format!("L{};", base)
                } else {
                    let arg_sigs: Vec<String> =
                        self.args.iter().map(|a| a.to_jvm_signature()).collect();
                    format!("L{}<{}>;", base, arg_sigs.join(""))
                }
            }
            // Treat other identifiers as type variables (e.g. "T", "E").
            other => format!("T{};", other),
        };

        if self.array_dims > 0 {
            sig = format!("{}{}", "[".repeat(self.array_dims), sig);
        }
        sig
    }
}

impl From<&str> for TypeName {
    fn from(s: &str) -> Self {
        TypeName::new(s)
    }
}

impl From<String> for TypeName {
    fn from(s: String) -> Self {
        TypeName::new(s)
    }
}

impl From<Arc<str>> for TypeName {
    fn from(arc: Arc<str>) -> Self {
        TypeName::new(arc)
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_internal_with_generics())
    }
}
