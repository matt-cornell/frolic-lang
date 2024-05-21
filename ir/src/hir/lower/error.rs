use frolic_utils::span::Span;
use miette::Diagnostic;
use thiserror::Error;

/// Internal errors, shouldn't ever be constructed through normal program usage.
#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum HirIce<S: Span> {
    #[error("ICE: global AST `{kind}` at local scope")]
    GlobalAstAtLocal { kind: &'static str, span: S },
    #[error("ICE: local AST `{kind}` at global scope")]
    LocalAstAtGlobal { kind: &'static str, span: S },
}

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum HirError<S: Span> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Ice(#[from] HirIce<S>),
}

#[derive(Debug, Clone, Copy, PartialEq, Error)]
#[error("Error reporter requested early termination")]
pub struct EarlyReturn;

pub type LowerResult = Result<(), EarlyReturn>;
