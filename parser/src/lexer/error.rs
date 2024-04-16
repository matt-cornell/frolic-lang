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

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum TokenizeError {
    #[error("unexpected char: {found:?}")]
    UnexpectedChar {
        #[label]
        span: SourceSpan,
        found: char,
    },
    #[error("invalid character {found:?} in {kind} literal")]
    InvalidCharInLit {
        #[label]
        span: SourceSpan,
        found: char,
        kind: LitKind,
    },
    #[error("invalid UTF-8 byte: 0x{byte:0>2x}")]
    InvalidUTF8 {
        #[label]
        span: SourceSpan,
        byte: u8,
    },
    #[error("unclosed multiline comment")]
    UnclosedMultiline {
        #[label("opened here")]
        span: SourceSpan,
        #[label]
        end: usize,
    },
    #[error("unclosed character literal")]
    UnclosedCharLit {
        #[label("started here")]
        span: SourceSpan,
        #[label]
        end: usize,
    },
    #[error("unclosed string literal")]
    UnclosedStrLit {
        #[label("started here")]
        span: SourceSpan,
        #[label]
        end: usize,
    },
    #[error("unknown escape code '\\{}'", PrettyByte(*.code))]
    UnknownEscapeCode {
        #[label]
        span: SourceSpan,
        code: u8,
    },
    #[error("expected {} brace in unicode escape code", if *.close {"closing"} else {"opening"})]
    ExpectedUnicodeBrace {
        close: bool,
        #[label("found {found:?}")]
        span: SourceSpan,
        found: char,
    },
}
