use super::*;
use miette::Diagnostic;
use std::fmt::{self, Debug, Display, Formatter};
use thiserror::Error;

/// Helper formatting struct, prints "end of input" if given None and for trees, prints an elipsis
pub struct FormatToken<'a, 'src, S>(&'a Option<TokenKind<'src, S>>);
impl<S: Debug> Display for FormatToken<'_, '_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            None => f.write_str("end of input"),
            Some(TokenKind::Paren(_)) => f.write_str("( ... )"),
            Some(TokenKind::Brace(_)) => f.write_str("{ ... }"),
            Some(TokenKind::Bracket(_)) => f.write_str("[ ... ]"),
            Some(tok) => Debug::fmt(tok, f),
        }
    }
}

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
        #[label("found {}", FormatToken(.found))]
        span: S,
        found: Option<TokenKind<'src, S>>,
    },
    #[error("Empty glob group")]
    EmptyGlobGroup {
        #[label]
        span: S,
    },
}
