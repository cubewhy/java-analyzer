use std::sync::Arc;

use crate::completion::parser::parse_chain_from_expr;
use crate::index::IndexView;
use crate::language::java::type_ctx::SourceTypeCtx;
use crate::semantic::types::type_name::TypeName;
use crate::semantic::types::{
    ChainSegment, TypeResolver, parse_single_type_to_internal, singleton_descriptor_to_type,
};
use crate::semantic::LocalVar;

pub(crate) fn resolve_expression_type(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    type_ctx: &SourceTypeCtx,
    view: &IndexView,
) -> Option<TypeName> {
    if looks_like_array_access(expr) {
        return resolve_array_access_type(expr, locals, enclosing_internal, resolver, type_ctx, view);
    }

    let chain = parse_chain_from_expr(expr);
    if chain.is_empty() {
        return resolver.resolve(expr, locals, enclosing_internal);
    }
    evaluate_chain(&chain, locals, enclosing_internal, resolver, type_ctx, view)
}

pub(crate) fn resolve_var_init_expr(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    type_ctx: &SourceTypeCtx,
    view: &IndexView,
) -> Option<TypeName> {
    let expr = expr.trim();
    if let Some(rest) = expr.strip_prefix("new ") {
        let mut boundary_idx = rest.find(['(', '[', '{']).unwrap_or(rest.len());
        if let Some(gen_start) = rest.find('<')
            && gen_start < boundary_idx
        {
            if let Some(gen_end) = find_matching_angle(rest, gen_start) {
                boundary_idx = gen_end + 1;
            } else {
                return None;
            }
        }
        let type_name = rest[..boundary_idx].trim();
        let resolved_base: TypeName = match type_name {
            "byte" | "short" | "int" | "long" | "float" | "double" | "boolean" | "char" => {
                TypeName::new(type_name)
            }
            _ => type_ctx.resolve_type_name_strict(type_name)?,
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

    resolve_expression_type(expr, locals, enclosing_internal, resolver, type_ctx, view)
}

pub(crate) fn looks_like_array_access(expr: &str) -> bool {
    expr.contains('[') && expr.trim_end().ends_with(']')
}

fn find_matching_angle(s: &str, start: usize) -> Option<usize> {
    let mut depth = 0i32;
    for (i, c) in s.char_indices().skip(start) {
        match c {
            '<' => depth += 1,
            '>' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

pub(crate) fn resolve_array_access_type(
    expr: &str,
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    type_ctx: &SourceTypeCtx,
    view: &IndexView,
) -> Option<TypeName> {
    let bracket = expr.rfind('[')?;
    if !expr.trim_end().ends_with(']') {
        return None;
    }
    let array_expr = expr[..bracket].trim();
    if array_expr.is_empty() {
        return None;
    }
    let array_type =
        resolve_expression_type(array_expr, locals, enclosing_internal, resolver, type_ctx, view)?;
    array_type.element_type()
}

pub(crate) fn evaluate_chain(
    chain: &[ChainSegment],
    locals: &[LocalVar],
    enclosing_internal: Option<&Arc<str>>,
    resolver: &TypeResolver,
    type_ctx: &SourceTypeCtx,
    view: &IndexView,
) -> Option<TypeName> {
    let mut current: Option<TypeName> = None;
    let resolve_qualifier = |q: &str| type_ctx.resolve_type_name_strict(q);
    for (i, seg) in chain.iter().enumerate() {
        let bracket_idx = seg.name.find('[');
        let base_name = if let Some(idx) = bracket_idx {
            &seg.name[..idx]
        } else {
            &seg.name
        };
        let dimensions = seg.name.matches('[').count();

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
                current = resolver.resolve_method_return_with_callsite_and_qualifier_resolver(
                    recv_internal.as_ref(),
                    base_name,
                    seg.arg_count.unwrap_or(-1),
                    arg_types_ref,
                    &seg.arg_texts,
                    locals,
                    enclosing_internal,
                    Some(&resolve_qualifier),
                );
            } else {
                current = resolver.resolve(base_name, locals, enclosing_internal);
                if current.is_none() {
                    if let Some(enclosing) = enclosing_internal {
                        let enclosing_simple = enclosing
                            .rsplit('/')
                            .next()
                            .unwrap_or(enclosing)
                            .rsplit('$')
                            .next()
                            .unwrap_or(enclosing);

                        if base_name == enclosing_simple {
                            current = Some(TypeName::new(enclosing.as_ref()));
                        }
                    }

                    if current.is_none() {
                        current = type_ctx.resolve_type_name_strict(base_name);
                    }
                }
            }
        } else {
            let recv = current.as_ref()?;
            if base_name.is_empty() {
                current = Some(recv.clone());
            } else {
                let recv_full: TypeName = if recv.contains_slash() {
                    recv.clone()
                } else {
                    let mut canonical = type_ctx.resolve_type_name_strict(recv.erased_internal())?;
                    if !recv.args.is_empty() {
                        canonical.args = recv.args.clone();
                    }
                    canonical.array_dims = recv.array_dims;
                    canonical
                };

                if seg.arg_count.is_some() {
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
                    let receiver_internal = recv_full.to_internal_with_generics();
                    current = resolver.resolve_method_return_with_callsite_and_qualifier_resolver(
                        &receiver_internal,
                        base_name,
                        seg.arg_count.unwrap_or(-1),
                        arg_types_ref,
                        &seg.arg_texts,
                        locals,
                        enclosing_internal,
                        Some(&resolve_qualifier),
                    );
                } else {
                    let (methods, fields) =
                        view.collect_inherited_members(recv_full.erased_internal());

                    if let Some(f) = fields.iter().find(|f| f.name.as_ref() == base_name) {
                        if let Some(ty) = singleton_descriptor_to_type(&f.descriptor) {
                            current = Some(TypeName::new(ty));
                        } else {
                            current = parse_single_type_to_internal(&f.descriptor);
                        }
                    } else if methods.iter().any(|m| m.name.as_ref() == base_name) {
                        current = None;
                    } else {
                        current = None;
                    }
                }
            }
        }

        if dimensions > 0
            && let Some(mut ty) = current.take()
        {
            let mut success = true;
            for _ in 0..dimensions {
                if let Some(el) = ty.element_type() {
                    ty = el;
                } else {
                    success = false;
                    break;
                }
            }
            if success {
                current = Some(ty);
            }
        }
    }
    current
}
