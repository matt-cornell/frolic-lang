use super::*;
use std::fmt::Debug;
use thiserror::Error;
use miette::{Diagnostic, LabeledSpan};

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TokenizeError<S> {
    #[error("unexpected char: {found:?}")]
    UnexpectedChar {
        span: S,
        found: char,
    },
    #[error("unexpected EOF")]
    UnexpectedEof {
        span: S,
    },
}

fn single_label<S: Span>(span: S, label: Option<String>) -> Option<Box<dyn Iterator<Item = LabeledSpan>>> {
    Some(Box::new(std::iter::once(LabeledSpan::new_with_span(label, (span.offset(), span.len())))))
}

impl<F: Span + Debug> Diagnostic for TokenizeError<F> {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Self::UnexpectedChar { span, .. } | Self::UnexpectedEof { span } => single_label(*span, None),
        }
    }
    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        match self {
            Self::UnexpectedChar { span, .. } | Self::UnexpectedEof { span } => span.file(),
        }
    }
}
