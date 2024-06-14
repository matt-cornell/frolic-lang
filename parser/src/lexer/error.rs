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
        kind: Delim,
        #[label("expected here")]
        span: S,
        #[label("opened here")]
        prev: S,
    },
    #[error("unmatched closing {kind}")]
    UnmatchedCloseDelim {
        kind: Delim,
        #[label("expected here")]
        span: S,
    },
    #[error("expected {} brace in unicode escape code", if *.close {"closing"} else {"opening"})]
    ExpectedUnicodeBrace {
        close: bool,
        #[label("found {found:?}")]
        span: S,
        found: char,
    },
}
