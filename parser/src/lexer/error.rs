use super::*;
use miette::Diagnostic;
use std::fmt::{self, Debug, Display, Formatter};
use strum::*;
use thiserror::Error;

struct PrettyByte(u8);
impl Display for PrettyByte {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if (20..=127).contains(&self.0) {
            write!(f, "{}", self.0 as char)
        } else {
            write!(f, "<{:0>2x}>", self.0)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr, Display)]
#[strum(serialize_all = "lowercase")]
pub enum LitKind {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}
impl TryFrom<char> for Delim {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '(' | ')' => Ok(Self::Paren),
            '{' | '}' => Ok(Self::Brace),
            '[' | ']' => Ok(Self::Bracket),
            _ => Err(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum TokenizeError<S: Span> {
    #[error("unexpected char: {found:?}")]
    UnexpectedChar {
        #[label]
        span: S,
        found: char,
    },
    #[error("invalid character {found:?} in {kind} literal")]
    InvalidCharInLit {
        #[label]
        span: S,
        found: char,
        kind: LitKind,
    },
    #[error("invalid UTF-8 byte: 0x{byte:0>2x}")]
    InvalidUTF8 {
        #[label]
        span: S,
        byte: u8,
    },
    #[error("unclosed multiline comment")]
    UnclosedMultiline {
        #[label("opened here")]
        span: S,
        #[label]
        end: usize,
    },
    #[error("unclosed character literal")]
    UnclosedCharLit {
        #[label("started here")]
        span: S,
        #[label]
        end: usize,
    },
    #[error("unclosed string literal")]
    UnclosedStrLit {
        #[label("started here")]
        span: S,
        #[label]
        end: usize,
    },
    #[error("unknown escape code '\\{}'", PrettyByte(*.code))]
    UnknownEscapeCode {
        #[label]
        span: S,
        code: u8,
    },
    #[error("unmatched opening {kind}")]
    UnmatchedOpenDelim {
        #[label("expected here")]
        span: S,
        #[label("opened here")]
        prev: S,
        kind: Delim,
    },
    #[error("unmatched closing {kind}")]
    UnmatchedCloseDelim {
        #[label("expected here")]
        span: S,
        kind: Delim,
    },
    #[error("expected {} brace in unicode escape code", if *.close {"closing"} else {"opening"})]
    ExpectedUnicodeBrace {
        #[label("found {found:?}")]
        span: S,
        found: char,
        close: bool,
    },
    #[error("macro without argument")]
    UnboundMacro { #[label] span: S }
}

impl<S: Span> TokenizeError<S> {
    pub fn map_span<T: Span, F: FnMut(S) -> T>(self, mut f: F) -> TokenizeError<T> {
        use TokenizeError::*;
        match self {
            UnexpectedChar { span, found } => UnexpectedChar {
                span: f(span),
                found,
            },
            InvalidCharInLit { span, found, kind } => InvalidCharInLit {
                span: f(span),
                found,
                kind,
            },
            InvalidUTF8 { span, byte } => InvalidUTF8 {
                span: f(span),
                byte,
            },
            UnclosedMultiline { span, end } => UnclosedMultiline { span: f(span), end },
            UnclosedCharLit { span, end } => UnclosedCharLit { span: f(span), end },
            UnclosedStrLit { span, end } => UnclosedStrLit { span: f(span), end },
            UnknownEscapeCode { span, code } => UnknownEscapeCode {
                span: f(span),
                code,
            },
            UnmatchedOpenDelim { span, prev, kind } => UnmatchedOpenDelim {
                span: f(span),
                prev: f(prev),
                kind,
            },
            UnmatchedCloseDelim { span, kind } => UnmatchedCloseDelim {
                span: f(span),
                kind,
            },
            ExpectedUnicodeBrace { span, found, close } => ExpectedUnicodeBrace {
                span: f(span),
                found,
                close,
            },
            UnboundMacro { span } => UnboundMacro { span: f(span) },
        }
    }
}
