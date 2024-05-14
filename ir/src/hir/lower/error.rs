use thiserror::Error;
use miette::Diagnostic;
use frolic_utils::prelude::*;

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum HirError<'src, S: Span> {
    #[error("phantom field")]
    Phantom {
        src: &'src [u8],
        #[label]
        label: S,
    }
}
