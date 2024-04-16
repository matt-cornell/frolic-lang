use super::*;
use miette::{Diagnostic, SourceCode};
use std::fmt::{self, Debug, Display, Formatter};
use strum::*;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr, Display)]
#[strum(serialize_all = "lowercase")]
pub enum LitKind {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hex = 16,
}

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum TokenizeErrorKind {
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
}

impl TokenizeErrorKind {
    pub fn with_src<F>(self, file: F) -> TokenizeError<F> {
        TokenizeError { file, error: self }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenizeError<F> {
    pub file: F,
    pub error: TokenizeErrorKind,
}
impl<F> Display for TokenizeError<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.error, f)
    }
}
impl<F: Debug> std::error::Error for TokenizeError<F> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.error.source()
    }
}
impl<F: Debug + SourceCode> Diagnostic for TokenizeError<F> {
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.file)
    }
    fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.url()
    }
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.code()
    }
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.help()
    }
    fn severity(&self) -> Option<miette::Severity> {
        self.error.severity()
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.error.labels()
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.error.related()
    }
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.error.diagnostic_source()
    }
}
