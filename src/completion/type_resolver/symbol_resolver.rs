use crate::completion::context::{CompletionContext, CursorLocation};
use crate::index::{FieldSummary, GlobalIndex, MethodSummary};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ResolvedSymbol {
    Class(Arc<str>),
    Method {
        owner: Arc<str>,
        summary: Arc<MethodSummary>,
    },
    Field {
        owner: Arc<str>,
        summary: Arc<FieldSummary>,
    },
}

pub struct SymbolResolver<'a> {
    index: &'a GlobalIndex,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(index: &'a GlobalIndex) -> Self {
        Self { index }
    }

    pub fn resolve(&self, ctx: &CompletionContext) -> Option<ResolvedSymbol> {
        match &ctx.location {
            CursorLocation::MemberAccess {
                receiver_type,
                receiver_expr,
                member_prefix,
                ..
            } => {
                let owner = receiver_type
                    .clone()
                    .or_else(|| self.infer_receiver_type(ctx, receiver_expr));
                tracing::debug!(
                    receiver_expr = %receiver_expr,
                    member = %member_prefix,
                    resolved_owner = ?owner,
                    "resolve: member access"
                );
                self.resolve_member(&owner?, member_prefix)
            }
            CursorLocation::StaticAccess {
                class_internal_name,
                member_prefix,
            } => self.resolve_member(class_internal_name, member_prefix),
            CursorLocation::Expression { prefix } => {
                if prefix.is_empty() {
                    return None;
                }
                self.resolve_bare_id(ctx, prefix)
            }
            CursorLocation::ConstructorCall { class_prefix, .. } => {
                if class_prefix.is_empty() {
                    return None;
                }
                tracing::debug!(class = %class_prefix, "resolve: constructor call");
                self.resolve_type_name(ctx, class_prefix)
                    .map(ResolvedSymbol::Class)
            }
            CursorLocation::TypeAnnotation { prefix } => {
                if prefix.is_empty() {
                    return None;
                }
                self.resolve_type_name(ctx, prefix)
                    .map(ResolvedSymbol::Class)
            }
            _ => None,
        }
    }

    fn resolve_member(&self, owner: &str, name: &str) -> Option<ResolvedSymbol> {
        if name.is_empty() {
            return None;
        }
        let (methods, fields) = self.index.collect_inherited_members(owner);
        tracing::debug!(
            owner = %owner,
            name = %name,
            methods = methods.len(),
            fields = fields.len(),
            "resolve: lookup member"
        );
        if let Some(m) = methods.iter().find(|m| m.name.as_ref() == name) {
            return Some(ResolvedSymbol::Method {
                owner: Arc::from(owner),
                summary: m.clone(),
            });
        }
        if let Some(f) = fields.iter().find(|f| f.name.as_ref() == name) {
            return Some(ResolvedSymbol::Field {
                owner: Arc::from(owner),
                summary: f.clone(),
            });
        }
        tracing::debug!(owner = %owner, name = %name, "resolve: member not found");
        None
    }

    fn resolve_bare_id(&self, ctx: &CompletionContext, id: &str) -> Option<ResolvedSymbol> {
        // 1. 局部变量 → 返回其类型（goto 会在调用方提前处理跳到声明处）
        if let Some(local) = ctx.local_variables.iter().find(|v| v.name.as_ref() == id) {
            let base = local.type_internal.base();
            let resolved_type = self
                .resolve_type_name(ctx, base)
                .unwrap_or_else(|| Arc::from(base));
            return Some(ResolvedSymbol::Class(resolved_type));
        }
        // 2. 当前类成员（隐式 this）
        if let Some(enclosing) = &ctx.enclosing_internal_name {
            tracing::debug!(
                enclosing = %enclosing,
                id = %id,
                "resolve: bare id in enclosing class"
            );
            if let Some(res) = self.resolve_member(enclosing, id) {
                return Some(res);
            }
        } else {
            tracing::debug!(id = %id, "resolve: enclosing_internal_name is None");
        }
        // 3. 类型名
        tracing::debug!(id = %id, "resolve: trying as type name");
        self.resolve_type_name(ctx, id).map(ResolvedSymbol::Class)
    }

    fn infer_receiver_type(&self, ctx: &CompletionContext, expr: &str) -> Option<Arc<str>> {
        let as_internal = expr.replace('.', "/");
        if self.index.get_class(&as_internal).is_some() {
            return Some(Arc::from(as_internal));
        }

        if expr == "this" {
            return ctx.enclosing_internal_name.clone();
        }
        // 直接局部变量
        if let Some(lv) = ctx.local_variables.iter().find(|v| v.name.as_ref() == expr) {
            let t = Arc::from(lv.type_internal.base());
            tracing::debug!(expr = %expr, type_ = %t, "resolve: receiver type from local var");
            return Some(t);
        }
        if !expr.contains('.') {
            // 简单标识符：作为类型名（处理 System.xxx 等静态字段访问）
            return self.resolve_type_name(ctx, expr);
        }
        // 链式字段访问：System.out → java/lang/System → field out → java/io/PrintStream
        self.resolve_chained(ctx, expr)
    }

    /// 迭代地走 dotted 表达式中的每个字段，返回最终类型的 internal name。
    fn resolve_chained(&self, ctx: &CompletionContext, expr: &str) -> Option<Arc<str>> {
        let mut parts = expr.split('.');
        let first = parts.next()?;

        let mut current: Arc<str> = if first == "this" {
            ctx.enclosing_internal_name.clone()?
        } else if let Some(lv) = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == first)
        {
            Arc::from(lv.type_internal.base())
        } else {
            self.resolve_type_name(ctx, first)?
        };

        for part in parts {
            tracing::debug!(owner = %current, field = %part, "resolve: chained field lookup");
            let (_, fields) = self.index.collect_inherited_members(&current);
            let field = fields.iter().find(|f| f.name.as_ref() == part)?;
            current = descriptor_to_internal_arc(&field.descriptor)?;
        }

        tracing::debug!(expr = %expr, result = %current, "resolve: chained resolved");
        Some(current)
    }

    fn resolve_type_name(&self, ctx: &CompletionContext, name: &str) -> Option<Arc<str>> {
        // 已经是 internal name（含 /）
        if name.contains('/') {
            return Some(Arc::from(name));
        }

        let as_internal = name.replace('.', "/");
        if let Some(c) = self.index.get_class(&as_internal) {
            return Some(c.internal_name.clone());
        }

        // 1. 优先验证精确 Import (import java.io.PrintStream;)
        for import in &ctx.existing_imports {
            let s = import.as_ref();
            // 精确 import: ends with .ClassName
            if s.ends_with(&format!(".{}", name)) {
                let internal = s.replace('.', "/");
                if let Some(c) = self.index.get_class(&internal) {
                    return Some(c.internal_name.clone());
                }
            }
            // 通配符 import: com.example.*
            if s.ends_with(".*") {
                let pkg = s.trim_end_matches(".*").replace('.', "/");
                let candidate = format!("{}/{}", pkg, name);
                if let Some(c) = self.index.get_class(&candidate) {
                    return Some(c.internal_name.clone());
                }
            }
        }
        let java_lang = format!("java/lang/{}", name);
        if let Some(c) = self.index.get_class(&java_lang) {
            return Some(c.internal_name.clone());
        }

        // 4. 同包（enclosing class 所在包）
        if let Some(enc) = &ctx.enclosing_internal_name
            && let Some(slash) = enc.rfind('/')
        {
            let pkg = &enc[..slash];
            let candidate = format!("{}/{}", pkg, name);
            if let Some(c) = self.index.get_class(&candidate) {
                return Some(c.internal_name.clone());
            }
        }
        tracing::debug!(name = %name, "resolve: type not found in index");
        None
    }
}

/// `Ljava/io/PrintStream;` → `java/io/PrintStream`
fn descriptor_to_internal_arc(desc: &str) -> Option<Arc<str>> {
    if desc.starts_with('L') && desc.ends_with(';') {
        Some(Arc::from(&desc[1..desc.len() - 1]))
    } else {
        // 基本类型、数组：不可导航为类
        None
    }
}
