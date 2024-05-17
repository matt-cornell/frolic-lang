use frolic_utils::prelude::*;
use miette::Diagnostic;
use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum HirError<'src, S: Span> {
    #[error("unresolved name \"{name}\"")]
    UnresolvedName {
        name: Cow<'src, str>,
        #[label]
        span: S,
    },
    #[error("Global variable defined at local scope")]
    GlobalDefAtLocal {
        #[label("defined here")]
        span: S,
    },
    #[error("ICE: `{kind}` was used which must be done at local scope, but was at global scope")]
    LocalAtGlobal {
        kind: &'static str,
        #[label]
        span: S,
    },
    #[error("ICE: `{kind}` was used which must be done at global scope, but was at local scope")]
    GlobalAtLocal {
        kind: &'static str,
        #[label]
        span: S,
    },
    #[error("ICE: mangled name {name:?} was not found at global scope")]
    MangledGlobalNotFound {
        name: String,
        #[label("defined by this definition")]
        span: S,
    }
}
