use super::*;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum HirError<'src, S: Span> {
    #[error("a global variable can't be declared at in local scope")]
    GlobalInLocal {
        #[label]
        span: S,
    },
    #[error("couldn't find variable {name:?}")]
    UnresolvedVariable {
        name: Cow<'src, str>,
        #[label]
        span: S,
    },
}
