use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum ParseASTError<'src> {
    #[error("Doc comment is not followed by a declaration")]
    UnboundOuterDoc {
        #[label]
        span: SourceSpan,
    },
    #[error("Inner doc comment not at start of module")]
    UnboundInnerDoc {
        #[label]
        span: SourceSpan,
    },
    #[error("Expressions aren't allowed at the top level")]
    InvalidTlExpression {
        #[label]
        span: SourceSpan,
    },
    #[error("Expected {ex}")]
    ExpectedFound {
        ex: &'static str,
        #[label]
        span: SourceSpan,
        #[label("found {found:?}")]
        found_loc: Option<SourceSpan>,
        found: crate::TokenKind<'src>,
    },
    #[error("Unmatched '{}'", .kind.get_char(!.close))]
    UnmatchedDelimeter {
        kind: crate::Delim,
        close: bool,
        #[label]
        span: SourceSpan,
        #[label("opened here")]
        start: SourceSpan,
    },
}
