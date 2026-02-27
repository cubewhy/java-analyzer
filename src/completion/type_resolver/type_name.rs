use std::sync::Arc;

/// Uniform internal type names, always using source style:
/// - Objects: "java/lang/String"
/// - Primitive types: "int", "char"
/// - Arrays: "java/lang/String[]", "int[][]"
/// - With generics: "java/util/List<Ljava/lang/String;>"
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName(pub(crate) Arc<str>);

impl TypeName {
    pub fn new(s: impl Into<Arc<str>>) -> Self {
        TypeName(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn is_array(&self) -> bool {
        self.0.ends_with("[]")
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self.0.as_ref(),
            "int" | "long" | "short" | "byte" | "char" | "float" | "double" | "boolean" | "void"
        )
    }

    /// "java/lang/String[][]" → Some("java/lang/String[]")
    pub fn element_type(&self) -> Option<TypeName> {
        self.0.strip_suffix("[]").map(TypeName::from)
    }

    /// "java/lang/String" → "java/lang/String[]"
    pub fn wrap_array(&self) -> TypeName {
        TypeName::new(format!("{}[]", self.0))
    }

    /// Remove generic parameters: "java/util/List<Ljava/lang/String;>" → "java/util/List
    pub fn base(&self) -> &str {
        self.0.split('<').next().unwrap_or(&self.0)
    }

    pub fn contains_slash(&self) -> bool {
        self.0.contains('/')
    }

    pub fn to_arc(&self) -> Arc<str> {
        self.0.clone()
    }
}

impl From<&str> for TypeName {
    fn from(s: &str) -> Self {
        TypeName(Arc::from(s))
    }
}

impl From<String> for TypeName {
    fn from(s: String) -> Self {
        TypeName(Arc::from(s.as_str()))
    }
}

impl AsRef<str> for TypeName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Deref for TypeName {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl From<Arc<str>> for TypeName {
    fn from(arc: Arc<str>) -> Self {
        TypeName(arc)
    }
}
