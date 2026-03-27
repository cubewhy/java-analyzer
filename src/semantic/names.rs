use std::sync::Arc;

use crate::semantic::types::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionNameBase {
    Local {
        name: Arc<str>,
    },
    Field {
        owner: Arc<str>,
        name: Arc<str>,
        is_static: bool,
    },
    This,
    Super,
    Value,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameClassification {
    Package {
        internal_name: Arc<str>,
    },
    Type {
        internal_name: Arc<str>,
        ty: TypeName,
    },
    Expression {
        ty: TypeName,
        base: ExpressionNameBase,
    },
}

impl NameClassification {
    pub fn type_internal(&self) -> Option<&str> {
        match self {
            Self::Type { internal_name, .. } => Some(internal_name.as_ref()),
            _ => None,
        }
    }

    pub fn expression_type(&self) -> Option<&TypeName> {
        match self {
            Self::Expression { ty, .. } => Some(ty),
            _ => None,
        }
    }

    pub fn into_qualifier_type(self) -> Option<TypeName> {
        match self {
            Self::Type { ty, .. } | Self::Expression { ty, .. } => Some(ty),
            Self::Package { .. } => None,
        }
    }
}
