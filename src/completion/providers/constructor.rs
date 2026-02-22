use super::super::{
    candidate::{CandidateKind, CompletionCandidate},
    context::{CompletionContext, CursorLocation},
};
use super::CompletionProvider;
use crate::{completion::scorer::AccessFilter, index::GlobalIndex};
use std::sync::Arc;

pub struct ConstructorProvider;

impl CompletionProvider for ConstructorProvider {
    fn name(&self) -> &'static str {
        "constructor"
    }

    fn provide(
        &self,
        ctx: &CompletionContext,
        index: &mut GlobalIndex,
    ) -> Vec<CompletionCandidate> {
        let class_prefix = match &ctx.location {
            CursorLocation::ConstructorCall { class_prefix } => class_prefix.as_str(),
            _ => return vec![],
        };

        index
            .fuzzy_search_classes(class_prefix, 30)
            .into_iter()
            .flat_map(|meta| {
                let fqn = match &meta.package {
                    Some(pkg) => format!("{}.{}", pkg.replace('/', "."), meta.name),
                    None => meta.name.to_string(),
                };

                let filter = AccessFilter::member_completion();

                let constructors: Vec<_> = meta
                    .methods
                    .iter()
                    .filter(|m| {
                        m.name.as_ref() == "<init>"
                            && filter.is_method_accessible(m.access_flags, m.is_synthetic)
                    })
                    .collect();

                if constructors.is_empty() {
                    let candidate = CompletionCandidate::new(
                        Arc::clone(&meta.name),
                        format!("{}()", meta.name),
                        CandidateKind::Constructor {
                            descriptor: Arc::from("()V"),
                            defining_class: Arc::clone(&meta.name),
                        },
                        self.name(),
                    )
                    .with_detail(format!("new {}()", fqn));
                    let candidate = if is_already_imported(&fqn, &ctx.existing_imports) {
                        candidate
                    } else {
                        candidate.with_import(fqn.clone())
                    };
                    // default to no parameter constructor
                    return vec![candidate];
                }

                constructors
                    .iter()
                    .map(|ctor| {
                        let readable_params = descriptor_params_to_readable(&ctor.descriptor);
                        let insert_text = format!("{}(", meta.name);
                        let detail = format!("new {}({})", fqn, readable_params);

                        let candidate = CompletionCandidate::new(
                            Arc::clone(&meta.name),
                            insert_text,
                            CandidateKind::Constructor {
                                descriptor: Arc::clone(&ctor.descriptor),
                                defining_class: Arc::clone(&meta.name),
                            },
                            self.name(),
                        )
                        .with_detail(detail)
                        .with_import(fqn.clone());

                        if is_already_imported(&fqn, &ctx.existing_imports) {
                            candidate
                        } else {
                            candidate.with_import(fqn.clone())
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect()
    }
}

pub fn descriptor_params_to_readable(descriptor: &str) -> String {
    let inner = match descriptor.find('(').zip(descriptor.find(')')) {
        Some((l, r)) => &descriptor[l + 1..r],
        None => return String::new(),
    };
    parse_type_list(inner)
        .into_iter()
        .map(|t| jvm_type_to_readable(&t))
        .collect::<Vec<_>>()
        .join(", ")
}

fn is_already_imported(fqn: &str, existing_imports: &[String]) -> bool {
    existing_imports.iter().any(|imp| {
        imp == fqn
            || (imp.ends_with(".*") && {
                // org.cubewhy.* 匹配 org.cubewhy.RandomClass
                let pkg = &imp[..imp.len() - 2]; // "org.cubewhy"
                fqn.starts_with(pkg)
                    && fqn[pkg.len()..].starts_with('.')
                    && !fqn[pkg.len() + 1..].contains('.')
            })
    })
}

fn parse_type_list(mut s: &str) -> Vec<String> {
    let mut result = Vec::new();
    while !s.is_empty() {
        let (ty, rest) = consume_one_type(s);
        result.push(ty);
        s = rest;
    }
    result
}

fn consume_one_type(s: &str) -> (String, &str) {
    match s.chars().next() {
        Some('L') => {
            // object type, like Ljava/lang/String;
            if let Some(end) = s.find(';') {
                (s[..=end].to_string(), &s[end + 1..])
            } else {
                (s.to_string(), "")
            }
        }
        Some('[') => {
            let (inner, rest) = consume_one_type(&s[1..]);
            (format!("[{}", inner), rest)
        }
        Some(c) => (c.to_string(), &s[1..]),
        None => (String::new(), ""),
    }
}

pub fn jvm_type_to_readable(ty: &str) -> String {
    if let Some(stripped) = ty.strip_prefix('[') {
        return format!("{}[]", jvm_type_to_readable(stripped));
    }
    if ty.starts_with('L') && ty.ends_with(';') {
        let class_path = &ty[1..ty.len() - 1];
        return class_path
            .rsplit('/')
            .next()
            .unwrap_or(class_path)
            .to_string();
    }
    match ty {
        "B" => "byte",
        "C" => "char",
        "D" => "double",
        "F" => "float",
        "I" => "int",
        "J" => "long",
        "S" => "short",
        "Z" => "boolean",
        "V" => "void",
        other => other,
    }
    .to_string()
}
