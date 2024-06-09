use super::*;
use miette::Diagnostic;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum ParseASTError<'src, S: Span> {
    #[error("Doc comment is not followed by a declaration")]
    UnboundOuterDoc {
        #[label]
        span: S,
    },
    #[error("Inner doc comment not at start of module")]
    UnboundInnerDoc {
        #[label]
        span: S,
    },
    #[error("Expressions aren't allowed at the top level")]
    InvalidTlExpression {
        #[label]
        span: S,
    },
    #[error("Expected {ex}")]
    ExpectedFound {
        ex: &'static str,
        #[label("found {}", .found.as_ref().map_or("EOF".to_string(), |t| format!("{t:?}")))]
        span: S,
        found: Option<TokenKind<'src>>,
    },
    #[error("Unmatched '{}'", .kind.get_char(!.close))]
    UnmatchedDelimeter {
        kind: Delim,
        close: bool,
        #[label]
        span: S,
        #[label("opened here")]
        start: S,
    },
    #[error("Empty glob group")]
    EmptyGlobGroup {
        #[label]
        span: S,
    },
}
