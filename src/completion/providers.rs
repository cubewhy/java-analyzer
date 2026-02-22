use super::{candidate::CompletionCandidate, context::CompletionContext};
use crate::index::GlobalIndex;

pub mod constructor;
pub mod expression;
pub mod import;
pub mod keyword;
pub mod local_var;
pub mod member;
pub mod package;
pub mod static_member;
pub mod this_member;

pub trait CompletionProvider: Send + Sync {
    fn provide(&self, ctx: &CompletionContext, index: &mut GlobalIndex)
    -> Vec<CompletionCandidate>;

    fn name(&self) -> &'static str;
}
