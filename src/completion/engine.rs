use super::context::CursorLocation;
use super::type_resolver::TypeResolver;
use super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::CompletionContext,
    providers::{
        CompletionProvider, constructor::ConstructorProvider, import::ImportProvider,
        keyword::KeywordProvider, local_var::LocalVarProvider, member::MemberProvider,
        static_member::StaticMemberProvider,
    },
    scorer::Scorer,
};
use crate::completion::LocalVar;
use crate::completion::import_utils::resolve_simple_to_internal;
use crate::completion::providers::annotation::AnnotationProvider;
use crate::completion::providers::expression::ExpressionProvider;
use crate::completion::providers::import_static::ImportStaticProvider;
use crate::completion::providers::name_suggestion::NameSuggestionProvider;
use crate::completion::providers::package::PackageProvider;
use crate::completion::providers::snippet::SnippetProvider;
use crate::completion::providers::static_import_member::StaticImportMemberProvider;
use crate::completion::providers::this_member::ThisMemberProvider;
use crate::completion::type_resolver::type_name::TypeName;
use crate::completion::type_resolver::{
    ChainSegment, parse_single_type_to_internal, singleton_descriptor_to_type,
};
use crate::index::GlobalIndex;
use std::sync::Arc;

pub struct CompletionEngine {
    providers: Vec<Box<dyn CompletionProvider>>,
}

impl CompletionEngine {
    pub fn new() -> Self {
        Self {
            providers: vec![
                Box::new(LocalVarProvider), // Highest priority: local variables
                Box::new(ThisMemberProvider),
                Box::new(MemberProvider),       // obj.xxx
                Box::new(StaticMemberProvider), // Cls.xxx
                Box::new(ConstructorProvider),  // new Xxx
                Box::new(PackageProvider),
                Box::new(ExpressionProvider), // expression/type position: class name
                Box::new(ImportProvider),     // import statement
                Box::new(ImportStaticProvider),
                Box::new(StaticImportMemberProvider),
                Box::new(KeywordProvider), // Keyword (triggered only upon input)
                Box::new(AnnotationProvider),
                Box::new(SnippetProvider), // Snippets
                Box::new(NameSuggestionProvider),
            ],
        }
    }

    pub fn register_provider(&mut self, provider: Box<dyn CompletionProvider>) {
        self.providers.push(provider);
    }

    pub fn complete(
        &self,
        mut ctx: CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        // infer type
        self.enrich_context(&mut ctx, index);

        let mut candidates: Vec<CompletionCandidate> = self
            .providers
            .iter()
            .flat_map(|p| p.provide(&ctx, index))
            .collect();

        // score
        let scorer = Scorer::new(&ctx.query);
        for c in &mut candidates {
            c.score += scorer.score(c);
        }

        candidates = dedup(candidates);

        // sort results
        candidates.sort_unstable_by(|a, b| {
            b.score
                .partial_cmp(&a.score)
                .unwrap_or(std::cmp::Ordering::Equal)
                .then_with(|| a.label.cmp(&b.label))
        });

        candidates
    }

    fn enrich_context(&self, ctx: &mut CompletionContext, index: &GlobalIndex) {
        if let CursorLocation::MemberAccess {
            receiver_type,
            receiver_expr,
            ..
        } = &mut ctx.location
            && receiver_type.is_none()
            && !receiver_expr.is_empty()
        {
            let resolver = TypeResolver::new(index);
            let resolved = if looks_like_array_access(receiver_expr) {
                resolve_array_access_type(
                    receiver_expr,
                    &ctx.local_variables,
                    ctx.enclosing_internal_name.as_ref(),
                    &resolver,
                    &ctx.existing_imports,
                    ctx.enclosing_package.as_deref(),
                    index,
                )
            } else {
                let chain = parse_chain_from_expr(receiver_expr);
                tracing::debug!(?chain, receiver_expr, "enrich_context: parsed chain");

                if chain.is_empty() {
                    let r = resolver.resolve(
                        receiver_expr,
                        &ctx.local_variables,
                        ctx.enclosing_internal_name.as_ref(),
                    );
                    tracing::debug!(
                        ?r,
                        receiver_expr,
                        "enrich_context: chain is empty, resolver.resolve returned"
                    );
                    r
                } else {
                    let r = evaluate_chain(
                        &chain,
                        &ctx.local_variables,
                        ctx.enclosing_internal_name.as_ref(),
                        &resolver,
                        &ctx.existing_imports,
                        ctx.enclosing_package.as_deref(),
                        index,
                    );
                    tracing::debug!(?r, "enrich_context: evaluate_chain returned");
                    r
                }
            };

            tracing::debug!(?resolved, "enrich_context: resolved before final match");

            // If the result is a simple name (without '/'), it needs to be further parsed into an internal name.
            *receiver_type = match resolved {
                None => {
                    tracing::debug!("enrich_context: final match -> None");
                    None
                }
                Some(ref ty) if ty.contains_slash() => Some(ty.to_arc()),
                Some(ty) => {
                    let r = resolve_simple_to_internal(
                        ty.as_str(),
                        &ctx.existing_imports,
                        ctx.enclosing_package.as_deref(),
                        index,
                    );
                    tracing::debug!(
                        ?r,
                        ?ty,
                        "enrich_context: final match -> resolve_simple_to_internal returned"
                    );
                    r
                }
            };

            // receiver_expr 是已知包名 -> 转成 Import
            let import_location: Option<(CursorLocation, String)> =
                if let CursorLocation::MemberAccess {
                    receiver_type,
                    receiver_expr,
                    member_prefix,
                } = &ctx.location
                    && receiver_type.is_none()
                {
                    let pkg_normalized = receiver_expr.replace('.', "/");
                    if index.has_package(&pkg_normalized) {
                        let prefix = format!("{}.{}", receiver_expr, member_prefix);
                        let query = member_prefix.clone();
                        Some((CursorLocation::Import { prefix }, query))
                    } else {
                        None
                    }
                } else {
                    None
                };

            if let Some((loc, query)) = import_location {
                ctx.location = loc;
                ctx.query = query;
            }
        }

        // Resolve `var` local variables
        {
            let resolver = TypeResolver::new(index);
            let to_resolve: Vec<(usize, String)> = ctx
                .local_variables
                .iter()
                .enumerate()
                .filter_map(|(i, lv)| {
                    if lv.type_internal.as_ref() == "var" {
                        lv.init_expr.as_deref().map(|e| (i, e.to_string()))
                    } else {
                        None
                    }
                })
                .collect();

            for (idx_in_vec, init_expr) in to_resolve {
                if let Some(resolved) = resolve_var_init_expr(
                    &init_expr,
                    &ctx.local_variables,
                    ctx.enclosing_internal_name.as_ref(),
                    &resolver,
                    &ctx.existing_imports,
                    ctx.enclosing_package.as_deref(),
                    index,
                ) {
                    ctx.local_variables[idx_in_vec].type_internal = resolved;
                }
            }
        }
    }
}

fn looks_like_array_access(expr: &str) -> bool {
    expr.contains('[') && expr.trim_end().ends_with(']')
}

fn resolve_array_access_type(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    _existing_imports: &[Arc<str>],
    _enclosing_package: Option<&str>,
    _index: &GlobalIndex,
) -> Option<TypeName> {
    let bracket = expr.rfind('[')?;
    if !expr.trim_end().ends_with(']') {
        return None;
    }
    let array_expr = expr[..bracket].trim();
    if array_expr.is_empty() {
        return None;
    }
    let array_type = resolver.resolve(array_expr, locals, enclosing_internal)?;
    array_type.element_type()
}

fn resolve_var_init_expr(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    existing_imports: &[Arc<str>],
    enclosing_package: Option<&str>,
    index: &GlobalIndex,
) -> Option<TypeName> {
    let expr = expr.trim();
    if let Some(rest) = expr.strip_prefix("new ") {
        // 寻找类型声明的边界：可能是普通构造函数 '('、泛型 '<'，或者是数组的 '['、'{'
        let boundary_idx = rest.find(['(', '<', '[', '{']).unwrap_or(rest.len());
        let type_name = rest[..boundary_idx].trim();

        // 解析基础类型，同时为 primitive 类型做白名单兜底
        let resolved_base: TypeName = match type_name {
            "byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char" => {
                TypeName::new(type_name)
            }
            _ => TypeName::from(crate::completion::import_utils::resolve_simple_to_internal(
                type_name,
                existing_imports,
                enclosing_package,
                index,
            )?),
        };

        let after_type = rest[boundary_idx..].trim_start();

        if after_type.starts_with('[') || after_type.starts_with('{') {
            let brace_idx = after_type.find('{').unwrap_or(after_type.len());
            let dimensions = after_type[..brace_idx].matches('[').count();
            let mut array_ty = resolved_base;
            for _ in 0..dimensions {
                array_ty = array_ty.wrap_array();
            }
            return Some(array_ty);
        }

        return Some(resolved_base);
    }

    let chain = crate::completion::engine::parse_chain_from_expr(expr);
    if !chain.is_empty() {
        return evaluate_chain(
            &chain,
            locals,
            enclosing_internal,
            resolver,
            existing_imports,
            enclosing_package,
            index,
        );
    }

    resolve_array_access_type(
        expr,
        locals,
        enclosing_internal,
        resolver,
        existing_imports,
        enclosing_package,
        index,
    )
}

/// 统一且健壮的调用链类型推导逻辑 (支持连缀方法调用和字段读取)
fn evaluate_chain(
    chain: &[ChainSegment],
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    existing_imports: &[Arc<str>],
    enclosing_package: Option<&str>,
    index: &GlobalIndex,
) -> Option<TypeName> {
    let mut current: Option<TypeName> = None;
    for (i, seg) in chain.iter().enumerate() {
        if i == 0 {
            if seg.arg_count.is_some() {
                let recv_internal = enclosing_internal?;
                let arg_types: Vec<TypeName> = seg
                    .arg_texts
                    .iter()
                    .filter_map(|t| resolver.resolve(t.trim(), locals, enclosing_internal))
                    .collect();
                let arg_types_ref: &[TypeName] = if arg_types.len() == seg.arg_texts.len() {
                    &arg_types
                } else {
                    &[]
                };
                current = resolver.resolve_method_return(
                    recv_internal.as_ref(),
                    &seg.name,
                    seg.arg_count.unwrap_or(-1),
                    arg_types_ref,
                );
            } else {
                current = resolver.resolve(&seg.name, locals, enclosing_internal);
                tracing::debug!(
                    ?current,
                    seg_name = seg.name,
                    "evaluate_chain: i=0 resolver.resolve returned"
                );

                if current.is_none() {
                    if let Some(enclosing) = enclosing_internal {
                        let enclosing_simple = enclosing
                            .rsplit('/')
                            .next()
                            .unwrap_or(enclosing)
                            .rsplit('$')
                            .next()
                            .unwrap_or(enclosing);

                        if seg.name == enclosing_simple {
                            current = Some(TypeName::new(enclosing.as_ref()));
                        }
                    }

                    // 如果还不是当前类，再走 Import 和全局解析
                    if current.is_none() {
                        current = resolve_simple_to_internal(
                            &seg.name,
                            existing_imports,
                            enclosing_package,
                            index,
                        )
                        .map(TypeName::from);

                        tracing::debug!(
                            ?current,
                            seg_name = seg.name,
                            "evaluate_chain: i=0 resolve_simple_to_internal returned"
                        );
                    }
                }
            }
        } else {
            let recv = current.as_ref()?;

            // === 新增：数组下标 segment，如 "[0]" ===
            if seg.arg_count.is_none() && seg.name.starts_with('[') && seg.name.ends_with(']') {
                let dimensions = seg.name.matches('[').count();
                let mut ty = recv.clone();
                for _ in 0..dimensions {
                    ty = ty.element_type()?;
                }
                current = Some(ty);
                continue;
            }

            let recv_str = recv.as_str();
            let recv_full: TypeName = if recv_str.contains('/')
                || index.get_class(recv_str).is_some()
            {
                recv.clone()
            } else {
                resolve_simple_to_internal(recv_str, existing_imports, enclosing_package, index)?
                    .into()
            };

            if seg.arg_count.is_some() {
                let arg_types: Vec<TypeName> = seg
                    .arg_texts
                    .iter()
                    .filter_map(|t| resolver.resolve(t.trim(), locals, enclosing_internal))
                    .collect();
                let arg_types_ref = if arg_types.len() == seg.arg_texts.len() {
                    &arg_types
                } else {
                    &vec![]
                };
                current = resolver.resolve_method_return(
                    recv_full.as_str(),
                    &seg.name,
                    seg.arg_count.unwrap_or(-1),
                    arg_types_ref,
                );
            } else {
                // 字段访问，支持数组字段
                let mut found = None;
                for m in index.mro(recv_full.base()) {
                    if let Some(f) = m.fields.iter().find(|f| f.name.as_ref() == seg.name) {
                        if let Some(ty) = singleton_descriptor_to_type(&f.descriptor) {
                            found = Some(TypeName::new(ty));
                        } else {
                            found = parse_single_type_to_internal(&f.descriptor);
                        }
                        break;
                    }
                }
                current = found;
            }
        }
    }
    current
}

impl Default for CompletionEngine {
    fn default() -> Self {
        Self::new()
    }
}

// 修改后的 dedup 实现了同一名称但不同签名(descriptor)的重载方法的保留
fn dedup(mut candidates: Vec<CompletionCandidate>) -> Vec<CompletionCandidate> {
    candidates.sort_unstable_by(|a, b| {
        a.label
            .cmp(&b.label)
            .then_with(|| a.source.cmp(b.source))
            .then_with(|| {
                a.required_import
                    .is_some()
                    .cmp(&b.required_import.is_some())
            })
            .then_with(|| {
                b.score
                    .partial_cmp(&a.score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    });

    let mut result: Vec<CompletionCandidate> = Vec::with_capacity(candidates.len());
    for c in candidates {
        let duplicate = result
            .iter()
            .rev()
            .take_while(|last| last.label == c.label)
            .any(|last| {
                if std::mem::discriminant(&last.kind) != std::mem::discriminant(&c.kind) {
                    return false;
                }
                match (&last.kind, &c.kind) {
                    (
                        CandidateKind::Method { descriptor: d1, .. },
                        CandidateKind::Method { descriptor: d2, .. },
                    )
                    | (
                        CandidateKind::StaticMethod { descriptor: d1, .. },
                        CandidateKind::StaticMethod { descriptor: d2, .. },
                    )
                    | (
                        CandidateKind::Constructor { descriptor: d1, .. },
                        CandidateKind::Constructor { descriptor: d2, .. },
                    ) => d1 == d2,
                    (CandidateKind::ClassName, CandidateKind::ClassName) => {
                        last.detail == c.detail && last.required_import == c.required_import
                    }
                    _ => true,
                }
            });

        if !duplicate {
            result.push(c);
        }
    }
    result
}

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
            c => {
                if depth == 0 {
                    current.push(c);
                }
            }
        }
    }

    // [修复点]: 循环结束后不要丢弃最后一个字段段落
    let trimmed = current.trim();
    if !trimmed.is_empty() && depth == 0 && !in_method {
        segments.push(ChainSegment::variable(trimmed.to_string()));
    }

    segments
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        completion::LocalVar,
        index::{ClassMetadata, ClassOrigin, MethodSummary},
    };
    use rust_asm::constants::ACC_PUBLIC;
    use std::sync::Arc;

    fn seg_names(expr: &str) -> Vec<(String, Option<i32>)> {
        parse_chain_from_expr(expr)
            .into_iter()
            .map(|s| (s.name, s.arg_count))
            .collect()
    }

    #[test]
    fn test_chain_simple_variable() {
        // [修复点] "list.ge" -> 应当解析为前后两个完整的 variable
        assert_eq!(
            seg_names("list.ge"),
            vec![("list".into(), None), ("ge".into(), None)]
        );
    }

    #[test]
    fn test_chain_method_call() {
        assert_eq!(
            seg_names("list.stream().fi"),
            vec![
                ("list".into(), None),
                ("stream".into(), Some(0)),
                ("fi".into(), None)
            ]
        );
    }

    #[test]
    fn test_chain_multiple_methods() {
        assert_eq!(
            seg_names("a.b().c(x, y).d"),
            vec![
                ("a".into(), None),
                ("b".into(), Some(0)),
                ("c".into(), Some(2)),
                ("d".into(), None)
            ]
        );
    }

    #[test]
    fn test_chain_no_dot() {
        assert_eq!(seg_names("someVar"), vec![("someVar".into(), None)]);
    }

    #[test]
    fn test_chain_nested_parens() {
        assert_eq!(
            seg_names("list.get(map.size()).toStr"),
            vec![
                ("list".into(), None),
                ("get".into(), Some(1)),
                ("toStr".into(), None)
            ]
        );
    }

    fn make_index_with_random_class() -> GlobalIndex {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("org/cubewhy")),
            name: Arc::from("RandomClass"),
            internal_name: Arc::from("org/cubewhy/RandomClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("f"),
                descriptor: Arc::from("()V"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: None,
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            inner_class_of: None,
            generic_signature: None,
            origin: ClassOrigin::Unknown,
        }]);
        idx
    }

    #[test]
    fn test_enrich_context_resolves_simple_name_via_import() {
        let idx = make_index_with_random_class();
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "f".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "f",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: TypeName::new("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".into()],
        );
        engine.enrich_context(&mut ctx, &idx);
        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(
                receiver_type.as_deref(),
                Some("org/cubewhy/RandomClass"),
                "receiver_type should be fully qualified after enrich"
            );
        }
    }

    #[test]
    fn test_enrich_context_resolves_simple_name_via_wildcard_import() {
        let idx = make_index_with_random_class();
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: TypeName::new("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.*".into()],
        );
        engine.enrich_context(&mut ctx, &idx);
        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(receiver_type.as_deref(), Some("org/cubewhy/RandomClass"),);
        }
    }

    #[test]
    fn test_complete_returns_f_method() {
        let mut idx = make_index_with_random_class();
        let engine = CompletionEngine::new();
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "f".to_string(),
                receiver_expr: "cl".to_string(),
            },
            "f",
            vec![LocalVar {
                name: Arc::from("cl"),
                type_internal: TypeName::new("RandomClass"),
                init_expr: None,
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".into()],
        );
        let results = engine.complete(ctx, &mut idx);
        assert!(
            results.iter().any(|c| c.label.as_ref() == "f"),
            "should find method f(): {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_dedup_prefers_no_import_candidate() {
        use crate::completion::candidate::{CandidateKind, CompletionCandidate};
        use std::sync::Arc;
        let with_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        )
        .with_import("org.cubewhy.RandomClass");
        let without_import = CompletionCandidate::new(
            Arc::from("RandomClass"),
            "RandomClass(",
            CandidateKind::Constructor {
                descriptor: Arc::from("()V"),
                defining_class: Arc::from("RandomClass"),
            },
            "constructor",
        );
        let result = dedup(vec![with_import, without_import]);
        assert_eq!(result.len(), 1, "should dedup to one candidate");
        assert!(
            result[0].required_import.is_none(),
            "dedup should prefer the candidate without required_import, got: {:?}",
            result[0].required_import
        );
    }

    #[test]
    fn test_chain_field_access_resolved() {
        use crate::index::{ClassMetadata, ClassOrigin, FieldSummary};
        use rust_asm::constants::{ACC_PUBLIC, ACC_STATIC};
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            ClassMetadata {
                package: Some(Arc::from("java/lang")),
                name: Arc::from("System"),
                internal_name: Arc::from("java/lang/System"),
                super_name: None,
                interfaces: vec![],
                methods: vec![],
                fields: vec![FieldSummary {
                    name: Arc::from("out"),
                    descriptor: Arc::from("Ljava/io/PrintStream;"), // 指向 PrintStream
                    access_flags: ACC_PUBLIC | ACC_STATIC,
                    is_synthetic: false,
                    generic_signature: None,
                }],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
            ClassMetadata {
                package: Some(Arc::from("java/io")),
                name: Arc::from("PrintStream"),
                internal_name: Arc::from("java/io/PrintStream"),
                super_name: None,
                interfaces: vec![],
                methods: vec![],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
        ]);

        let engine = CompletionEngine::new();
        // 模拟用户输入了 System.out.|
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "System.out".to_string(),
            },
            "",
            vec![],
            None,
            None,
            None,
            vec!["java.lang.System".into()], // 确保 System 能够被解析
        );

        engine.enrich_context(&mut ctx, &idx);

        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(
                receiver_type.as_deref(),
                Some("java/io/PrintStream"),
                "System.out 应该被正确链式推导为 java/io/PrintStream"
            );
        } else {
            panic!("Location changed unexpectedly");
        }
    }

    #[test]
    fn test_expected_type_ranks_first_in_constructor_completion() {
        use crate::completion::context::CursorLocation;
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;
        let mut idx = GlobalIndex::new();
        for (pkg, name) in [
            ("org/cubewhy/a", "Main"),
            ("org/cubewhy/a", "Main2"),
            ("org/cubewhy", "RandomClass"),
        ] {
            idx.add_classes(vec![ClassMetadata {
                package: Some(Arc::from(pkg)),
                name: Arc::from(name),
                internal_name: Arc::from(format!("{}/{}", pkg, name)),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("<init>"),
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
            }]);
        }
        let engine = CompletionEngine::new();
        let ctx = CompletionContext::new(
            CursorLocation::ConstructorCall {
                class_prefix: String::new(),
                expected_type: Some("RandomClass".to_string()),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("org/cubewhy/a/Main")),
            Some(Arc::from("org/cubewhy/a")),
            vec!["org.cubewhy.RandomClass".into()],
        );
        let results = engine.complete(ctx, &mut idx);
        assert!(!results.is_empty(), "should have candidates");
        assert_eq!(
            results[0].label.as_ref(),
            "RandomClass",
            "RandomClass should rank first when it matches expected_type, got: {:?}",
            results.iter().map(|c| c.label.as_ref()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_var_method_return_type_resolved() {
        use crate::completion::context::{CursorLocation, LocalVar};
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: None,
            name: Arc::from("NestedClass"),
            internal_name: Arc::from("NestedClass"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("randomFunction"),
                descriptor: Arc::from("(Ljava/lang/String;)LNestedClass;"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: Some(Arc::from("NestedClass")),
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("nc"),
                    type_internal: TypeName::new("NestedClass"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("str"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("nc.randomFunction()".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(str_var.type_internal.as_ref(), "NestedClass");
    }

    #[test]
    fn test_var_overload_resolved_by_long_arg() {
        use crate::completion::context::{CursorLocation, LocalVar};
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
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("nc"),
                    type_internal: TypeName::new("NestedClass"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("str"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("nc.randomFunction(\"a\", 1l)".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(str_var.type_internal.as_ref(), "Main2");
    }

    #[test]
    fn test_var_bare_method_call_resolved() {
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
                name: Arc::from("getString"),
                descriptor: Arc::from("()Ljava/lang/String;"),
                access_flags: ACC_PUBLIC,
                is_synthetic: false,
                generic_signature: None,
                return_type: Some(Arc::from("java/lang/String")),
            }],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "str".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("str"),
                type_internal: TypeName::new("var"),
                init_expr: Some("getString()".to_string()),
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("Main")),
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let str_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "str")
            .unwrap();
        assert_eq!(str_var.type_internal.as_ref(), "java/lang/String");
    }

    #[test]
    fn test_resolve_method_return_walks_mro() {
        use crate::index::{ClassMetadata, ClassOrigin, MethodSummary};
        use rust_asm::constants::ACC_PUBLIC;
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![
            ClassMetadata {
                package: None,
                name: Arc::from("Parent"),
                internal_name: Arc::from("Parent"),
                super_name: None,
                interfaces: vec![],
                methods: vec![MethodSummary {
                    name: Arc::from("getValue"),
                    descriptor: Arc::from("()Ljava/lang/String;"),
                    access_flags: ACC_PUBLIC,
                    is_synthetic: false,
                    generic_signature: None,
                    return_type: Some(Arc::from("java/lang/String")),
                }],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
            ClassMetadata {
                package: None,
                name: Arc::from("Child"),
                internal_name: Arc::from("Child"),
                super_name: Some("Parent".into()),
                interfaces: vec![],
                methods: vec![],
                fields: vec![],
                access_flags: ACC_PUBLIC,
                generic_signature: None,
                inner_class_of: None,
                origin: ClassOrigin::Unknown,
            },
        ]);
        let resolver = TypeResolver::new(&idx);
        let result = resolver.resolve_method_return("Child", "getValue", 0, &[]);
        assert_eq!(result.as_deref(), Some("java/lang/String"));
    }

    #[test]
    fn test_complete_member_after_bare_method_call() {
        let mut idx = GlobalIndex::new();
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
        let engine = CompletionEngine::new();
        let ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "getMain2()".to_string(),
            },
            "",
            vec![],
            Some(Arc::from("Main")),
            Some(Arc::from("Main")),
            None,
            vec![],
        );
        let results = engine.complete(ctx, &mut idx);
        assert!(results.iter().any(|c| c.label.as_ref() == "func"));
    }

    #[test]
    fn test_var_array_element_type_resolved() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/lang")),
            name: Arc::from("String"),
            internal_name: Arc::from("java/lang/String"),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "a".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("args"),
                    type_internal: TypeName::new("String[]"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("a"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("args[0]".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let a_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "a")
            .unwrap();
        assert_eq!(a_var.type_internal.as_ref(), "String");
    }

    #[test]
    fn test_var_primitive_array_element_not_resolved() {
        let idx = GlobalIndex::new();
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "x".to_string(),
            },
            "",
            vec![
                LocalVar {
                    name: Arc::from("nums"),
                    type_internal: TypeName::new("int[]"),
                    init_expr: None,
                },
                LocalVar {
                    name: Arc::from("x"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("nums[0]".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let x_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "x")
            .unwrap();
        assert_ne!(x_var.type_internal.as_ref(), "int[]");
    }

    #[test]
    fn test_enrich_context_array_access_receiver() {
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/lang")),
            name: Arc::from("String"),
            internal_name: Arc::from("java/lang/String"),
            super_name: None,
            interfaces: vec![],
            methods: vec![MethodSummary {
                name: Arc::from("length"),
                descriptor: Arc::from("()I"),
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
        }]);
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "b[0]".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("b"),
                type_internal: TypeName::new("String[]"),
                init_expr: None,
            }],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        if let CursorLocation::MemberAccess { receiver_type, .. } = &ctx.location {
            assert_eq!(receiver_type.as_deref(), Some("java/lang/String"));
        }
    }

    #[test]
    fn test_package_path_becomes_import_location() {
        use crate::completion::engine::CompletionEngine;
        use crate::index::{ClassMetadata, ClassOrigin, GlobalIndex};
        use rust_asm::constants::ACC_PUBLIC;
        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/util")),
            name: Arc::from("ArrayList"),
            internal_name: Arc::from("java/util/ArrayList"),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "ArrayL".to_string(),
                receiver_expr: "java.util".to_string(),
            },
            "ArrayL",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        assert!(matches!(
            &ctx.location,
            CursorLocation::Import { prefix } if prefix == "java.util.ArrayL"
        ));
    }

    #[test]
    fn test_unknown_receiver_stays_member_access() {
        use crate::completion::engine::CompletionEngine;
        use crate::index::GlobalIndex;
        let idx = GlobalIndex::new();
        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "foo".to_string(),
                receiver_expr: "unknownPkg".to_string(),
            },
            "foo",
            vec![],
            None,
            None,
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        assert!(matches!(&ctx.location, CursorLocation::MemberAccess { .. }));
    }

    #[test]
    fn test_var_array_initializer_and_access() {
        use crate::completion::context::LocalVar;
        use crate::index::{ClassMetadata, ClassOrigin};
        use rust_asm::constants::ACC_PUBLIC;

        let mut idx = GlobalIndex::new();
        idx.add_classes(vec![ClassMetadata {
            package: Some(Arc::from("java/lang")),
            name: Arc::from("String"),
            internal_name: Arc::from("java/lang/String"),
            super_name: None,
            interfaces: vec![],
            methods: vec![],
            fields: vec![],
            access_flags: ACC_PUBLIC,
            generic_signature: None,
            inner_class_of: None,
            origin: ClassOrigin::Unknown,
        }]);

        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "strItem".to_string(), // 触发位置
            },
            "",
            vec![
                // var arr = new char[]{};
                LocalVar {
                    name: Arc::from("arr"),
                    type_internal: TypeName::new("char[]"),
                    init_expr: None,
                },
                // var c = arr[0];
                LocalVar {
                    name: Arc::from("c"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("arr[0]".to_string()),
                },
                // var strArr = new String[]{"[1]", "[2]"}; (标准对象数组，带干扰符号)
                LocalVar {
                    name: Arc::from("strArr"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("new String[]{\"[1]\", \"[2]\"}".to_string()),
                },
                // var strItem = strArr[1];
                LocalVar {
                    name: Arc::from("strItem"),
                    type_internal: TypeName::new("var"),
                    init_expr: Some("strArr[1]".to_string()),
                },
            ],
            None,
            None,
            None,
            vec![], // 没传 import，String 可以被兜底或者需要完整包名？
        );

        // 注入默认 java.lang.* import 来确保 String 能正常被 resolve_simple_to_internal 解析
        ctx.existing_imports.push(Arc::from("java.lang.*"));

        engine.enrich_context(&mut ctx, &idx);

        // 校验 c (arr[0]) 被推断为 char
        let c_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "c")
            .unwrap();
        assert_eq!(c_var.type_internal.as_ref(), "char");

        // 校验 strArr (new String[]...) 被推断为 java/lang/String[]
        let str_arr_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "strArr")
            .unwrap();
        assert_eq!(str_arr_var.type_internal.as_ref(), "java/lang/String[]");

        // 校验 strItem (strArr[1]) 被推断为 java/lang/String
        let str_item_var = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "strItem")
            .unwrap();
        assert_eq!(str_item_var.type_internal.as_ref(), "java/lang/String");
    }

    #[test]
    fn test_var_chained_array_access_from_method() {
        // 验证 getArr()[0] 这种通过方法调用拿到数组再取下标的情况
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
                name: Arc::from("getArr"),
                descriptor: Arc::from("()[Ljava/lang/String;"),
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
        }]);

        let engine = CompletionEngine::new();
        let mut ctx = CompletionContext::new(
            CursorLocation::MemberAccess {
                receiver_type: None,
                member_prefix: "".to_string(),
                receiver_expr: "item".to_string(),
            },
            "",
            vec![LocalVar {
                name: Arc::from("item"),
                type_internal: TypeName::new("var"),
                init_expr: Some("getArr()[0]".to_string()),
            }],
            Some(Arc::from("Main")),
            Some(Arc::from("Main")),
            None,
            vec![],
        );
        engine.enrich_context(&mut ctx, &idx);
        let item = ctx
            .local_variables
            .iter()
            .find(|v| v.name.as_ref() == "item")
            .unwrap();
        assert_eq!(item.type_internal.as_ref(), "java/lang/String");
    }

    #[test]
    fn test_dedup_allows_same_label_different_package() {
        let c1 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());
        let c2 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.awt.List".to_string());

        let results = dedup(vec![c1, c2]);
        assert_eq!(
            results.len(),
            2,
            "Should keep both List candidates because FQNs are different"
        );
    }

    #[test]
    fn test_dedup_removes_actual_duplicates() {
        let c1 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());
        let c2 = CompletionCandidate::new(
            Arc::from("List"),
            "List".to_string(),
            CandidateKind::ClassName,
            "test",
        )
        .with_detail("java.util.List".to_string());

        let results = dedup(vec![c1, c2]);
        assert_eq!(results.len(), 1, "Should remove identical class candidates");
    }
}
