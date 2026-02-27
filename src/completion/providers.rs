use super::{candidate::CompletionCandidate, context::CompletionContext};
use crate::index::GlobalIndex;

pub mod annotation;
pub mod constructor;
pub mod expression;
pub mod import;
pub mod import_static;
pub mod keyword;
pub mod local_var;
pub mod member;
pub mod name_suggestion;
pub mod override_member;
pub mod package;
pub mod snippet;
pub mod static_import_member;
pub mod static_member;
pub mod this_member;

pub trait CompletionProvider: Send + Sync {
    fn provide(&self, ctx: &CompletionContext, index: &mut GlobalIndex)
    -> Vec<CompletionCandidate>;

    fn name(&self) -> &'static str;
}
