use serde::Serialize;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum CandidateKind {
    ClassName,
    Package,
    Snippet,
    Method {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    Field {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    StaticMethod {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    StaticField {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    LocalVariable {
        type_descriptor: Arc<str>,
    },
    Constructor {
        descriptor: Arc<str>,
        defining_class: Arc<str>,
    },
    StatementLabel,
    Keyword,
    Annotation,
    AnnotationElement,
    NameSuggestion,
}

#[derive(Debug, Clone, Serialize)]
pub enum InsertTextMode {
    PlainText,
    Snippet,
}

#[derive(Debug, Clone, Serialize)]
pub enum ReplacementMode {
    /// Replace the identifier-like segment right before the cursor.
    Identifier,
    /// Replace only the member segment after a dot.
    MemberSegment,
    /// Replace package-like path including dots.
    PackagePath,
    /// Replace full import path inside import declarations.
    ImportPath,
    /// Replace access-modifier prefix in override stubs.
    AccessModifierPrefix,
    /// Do not enforce textEdit; let client default insertion behavior apply.
    ClientDefault,
}

#[derive(Debug, Clone, Serialize)]
pub struct MemberAccessRewrite {
    pub receiver_expr: String,
    pub cast_type: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct CompletionInsertion {
    pub text: String,
    pub mode: InsertTextMode,
    pub replacement: ReplacementMode,
    pub filter_text: Option<String>,
    pub member_access_rewrite: Option<MemberAccessRewrite>,
}

impl CompletionInsertion {
    pub fn plain(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            mode: InsertTextMode::PlainText,
            replacement: ReplacementMode::Identifier,
            filter_text: None,
            member_access_rewrite: None,
        }
    }

    pub fn snippet(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            mode: InsertTextMode::Snippet,
            replacement: ReplacementMode::Identifier,
            filter_text: None,
            member_access_rewrite: None,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct CompletionCandidate {
    /// The text displayed to the user (method name, field name, class name, etc.)
    pub label: Arc<str>,
    /// The actual text inserted (may differ from the label, for example, if the method requires parentheses).
    pub insert_text: String,
    /// Candidate-owned insertion intent used for mechanical LSP conversion.
    pub insertion: CompletionInsertion,
    /// Candidate Types
    pub kind: CandidateKind,
    /// Overall score, the higher the score, the higher the ranking.
    pub score: f32,
    /// Optional: Documentation comments / Signature
    pub detail: Option<String>,
    /// Marks which provider the candidate was generated from (for debugging/statistics purposes)
    pub source: &'static str,
    /// If this candidate needs to be automatically imported, record it here.
    pub required_import: Option<String>,
}

impl CompletionCandidate {
    pub fn new(
        label: impl Into<Arc<str>>,
        insert_text: impl Into<String>,
        kind: CandidateKind,
        source: &'static str,
    ) -> Self {
        let insert_text = insert_text.into();
        let insertion = if kind == CandidateKind::Snippet {
            CompletionInsertion::snippet(insert_text.clone())
        } else {
            CompletionInsertion::plain(insert_text.clone())
        };

        Self {
            label: label.into(),
            insert_text,
            insertion,
            kind,
            score: 0.0,
            detail: None,
            source,
            required_import: None,
        }
    }

    pub fn with_score(mut self, score: f32) -> Self {
        self.score = score;
        self
    }

    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    pub fn with_import(mut self, import: impl Into<String>) -> Self {
        self.required_import = Some(import.into());
        self
    }

    pub fn with_replacement_mode(mut self, replacement: ReplacementMode) -> Self {
        self.insertion.replacement = replacement;
        self
    }

    pub fn with_insert_mode(mut self, mode: InsertTextMode) -> Self {
        self.insertion.mode = mode;
        self
    }

    pub fn with_filter_text(mut self, filter_text: impl Into<String>) -> Self {
        self.insertion.filter_text = Some(filter_text.into());
        self
    }

    pub fn with_insert_text(mut self, text: impl Into<String>) -> Self {
        let text = text.into();
        self.insert_text = text.clone();
        self.insertion.text = text;
        self
    }

    pub fn with_member_access_cast_rewrite(
        mut self,
        receiver_expr: impl Into<String>,
        cast_type: impl Into<String>,
    ) -> Self {
        let receiver_expr = receiver_expr.into();
        let cast_type = cast_type.into();
        self.insertion.member_access_rewrite = Some(MemberAccessRewrite {
            receiver_expr,
            cast_type,
        });
        self
    }

    pub fn with_callable_insert(
        mut self,
        callable_name: &str,
        param_names: &[Arc<str>],
        has_paren_after_cursor: bool,
    ) -> Self {
        if has_paren_after_cursor {
            return self.with_insert_text(callable_name);
        }

        if param_names.is_empty() {
            self = self.with_insert_text(format!("{callable_name}()"));
            self.insertion.mode = InsertTextMode::PlainText;
            return self;
        }

        let mut args = Vec::with_capacity(param_names.len());
        for (idx, raw) in param_names.iter().enumerate() {
            let safe = sanitize_placeholder_name(raw.as_ref(), idx);
            args.push(format!("${{{}:{}}}", idx + 1, safe));
        }
        let snippet = format!("{callable_name}({})$0", args.join(", "));
        self = self.with_insert_text(snippet);
        self.insertion.mode = InsertTextMode::Snippet;
        self
    }

    pub fn with_generics_callable_insert(
        mut self,
        callable_name: &str,
        param_names: &[Arc<str>],
        has_paren_after_cursor: bool,
    ) -> Self {
        if has_paren_after_cursor {
            return self.with_insert_text(callable_name);
        }

        if param_names.is_empty() {
            self = self.with_insert_text(format!("{callable_name}<$1>()$0"));
            self.insertion.mode = InsertTextMode::Snippet;
            return self;
        }

        let mut args = Vec::with_capacity(param_names.len());
        for (idx, raw) in param_names.iter().enumerate() {
            let idx = idx + 1;
            let safe = sanitize_placeholder_name(raw.as_ref(), idx);
            args.push(format!("${{{}:{}}}", idx + 1, safe));
        }
        let snippet = format!("{callable_name}<$0>({})$1", args.join(", "));
        self = self.with_insert_text(snippet);
        self.insertion.mode = InsertTextMode::Snippet;
        self
    }
}

fn sanitize_placeholder_name(name: &str, index: usize) -> String {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return format!("arg{}", index + 1);
    }

    let mut out = String::with_capacity(trimmed.len());
    for ch in trimmed.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        return format!("arg{}", index + 1);
    }
    if out.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        out.insert(0, 'a');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_callable_insert_zero_arg_adds_parentheses() {
        let c = CompletionCandidate::new(
            Arc::from("foo"),
            "foo",
            CandidateKind::Method {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("T"),
            },
            "test",
        )
        .with_callable_insert("foo", &[], false);
        assert_eq!(c.insert_text, "foo()");
        assert!(matches!(c.insertion.mode, InsertTextMode::PlainText));
    }

    #[test]
    fn test_callable_insert_params_use_snippet_slots() {
        let params = vec![Arc::from("s"), Arc::from("n")];
        let c = CompletionCandidate::new(
            Arc::from("print"),
            "print",
            CandidateKind::Method {
                descriptor: Arc::from("(Ljava/lang/String;I)V"),
                defining_class: Arc::from("T"),
            },
            "test",
        )
        .with_callable_insert("print", &params, false);
        assert_eq!(c.insert_text, "print(${1:s}, ${2:n})$0");
        assert!(matches!(c.insertion.mode, InsertTextMode::Snippet));
    }

    #[test]
    fn test_callable_insert_no_duplicate_paren_when_suffix_exists() {
        let c = CompletionCandidate::new(
            Arc::from("foo"),
            "foo",
            CandidateKind::Method {
                descriptor: Arc::from("(I)V"),
                defining_class: Arc::from("T"),
            },
            "test",
        )
        .with_callable_insert("foo", &[Arc::from("x")], true);
        assert_eq!(c.insert_text, "foo");
    }
}
