use std::{collections::HashMap, sync::Arc};

#[derive(Debug, Clone)]
pub struct CurrentClassMember {
    pub name: Arc<str>,
    pub is_method: bool,
    pub is_static: bool,
    pub is_private: bool,
    pub descriptor: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CursorLocation {
    /// `import com.example.|`
    Import { prefix: String },
    /// `someObj.|` or `someObj.prefix|`
    MemberAccess {
        /// The inferred type of the accessed object (internal name, such as "java/lang/String")
        receiver_type: Option<Arc<str>>,
        /// Prefix of members entered before the cursor
        member_prefix: String,
        /// Receiver expression plaintext, used by TypeResolver
        receiver_expr: String,
    },
    /// `ClassName.|` (static access)
    StaticAccess {
        class_internal_name: Arc<str>,
        member_prefix: String,
    },
    /// `new Foo|`
    ConstructorCall { class_prefix: String },
    /// Type annotation location: the type part of the variable declaration `Ma|in m;`
    // The class name should be completed, not the variable name.
    TypeAnnotation { prefix: String },
    /// Method call parameter location: `foo(aV|)` → Complete local variable
    MethodArgument { prefix: String },
    /// Location of a regular expression (which could be a local variable, static class name, or keyword)
    Expression { prefix: String },
    /// Unrecognized location
    Unknown,
}

#[derive(Debug, Clone)]
pub struct CompletionContext {
    pub location: CursorLocation,
    pub local_variables: Vec<LocalVar>,
    pub enclosing_class: Option<Arc<str>>,
    pub enclosing_internal_name: Option<Arc<str>>,
    pub enclosing_package: Option<Arc<str>>,
    pub existing_imports: Vec<String>,
    pub query: String,
    /// All members of the current class (parsed directly from the source file, without relying on indexes)
    pub current_class_members: HashMap<Arc<str>, CurrentClassMember>,
    /// The method/field member where the cursor is located (None indicates that it is in the field initializer or static block)
    pub enclosing_class_member: Option<CurrentClassMember>,
    pub char_after_cursor: Option<char>,
}

#[derive(Debug, Clone)]
pub struct LocalVar {
    pub name: Arc<str>,
    /// internal class name, like "java/util/List"
    pub type_internal: Arc<str>,
}

impl CompletionContext {
    pub fn new(
        location: CursorLocation,
        query: impl Into<String>,
        local_variables: Vec<LocalVar>,
        enclosing_class: Option<Arc<str>>,
        enclosing_internal_name: Option<Arc<str>>,
        enclosing_package: Option<Arc<str>>,
        existing_imports: Vec<String>,
    ) -> Self {
        Self {
            location,
            local_variables,
            enclosing_class,
            enclosing_internal_name,
            enclosing_package,
            existing_imports,
            query: query.into(),
            current_class_members: HashMap::new(),
            enclosing_class_member: None,
            char_after_cursor: None,
        }
    }

    pub fn with_class_members(
        mut self,
        members: impl IntoIterator<Item = CurrentClassMember>,
    ) -> Self {
        self.current_class_members = members
            .into_iter()
            .map(|m| (Arc::clone(&m.name), m))
            .collect();
        self
    }

    pub fn with_enclosing_member(mut self, member: Option<CurrentClassMember>) -> Self {
        self.enclosing_class_member = member;
        self
    }

    /// Whether the current context is static (static method / static field initializer)
    pub fn is_in_static_context(&self) -> bool {
        self.enclosing_class_member
            .as_ref()
            .is_some_and(|m| m.is_static)
    }

    pub fn with_char_after_cursor(mut self, c: Option<char>) -> Self {
        self.char_after_cursor = c;
        self
    }

    /// The cursor is immediately followed by '(', and method completion does not require additional parentheses.
    pub fn has_paren_after_cursor(&self) -> bool {
        self.char_after_cursor == Some('(')
    }
}
