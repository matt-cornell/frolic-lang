use frolic_utils::span::Span;
use miette::{Diagnostic, LabeledSpan, SourceCode};
use std::fmt::{self, Debug, Formatter};
use thiserror::Error;

/// Internal errors, shouldn't ever be constructed through normal program usage.
#[derive(Debug, Clone, Copy, PartialEq, Error, Diagnostic)]
pub enum HirIce<'b, S: Span> {
    #[error("ICE: TODO: {message}")]
    Todo {
        message: &'static str,
        #[label]
        span: S,
    },
    #[error("ICE: global AST `{kind}` at local scope")]
    GlobalAstAtLocal {
        kind: &'static str,
        #[label]
        span: S,
    },
    #[error("ICE: local AST `{kind}` at global scope")]
    LocalAstAtGlobal {
        kind: &'static str,
        #[label]
        span: S,
    },
    #[error("ICE: empty variable name")]
    EmptyVarName {
        #[label]
        span: S,
    },
    #[error("ICE: couldn't find global `{name}` in symbol table")]
    CouldntFindInTable {
        name: &'b str,
        #[label]
        span: S,
    },
    // internal error because we should be catching this at parsing.
    #[error("ICE: invalid local name: {name:?}")]
    InvalidLocalName {
        name: &'b str,
        #[label]
        span: S,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Error)]
pub enum HirError<'b, S: Span, F> {
    #[error(transparent)]
    Ice(HirIce<'b, S>),
    #[error("redefinition of `{name}`")]
    DuplicateDefinition {
        name: &'b str,
        span: S,
        prev: PrevDef<S, F>,
    },
    #[error("unbound variable `{name}`")]
    UnboundVariable { name: &'b str, span: S },
}
impl<'b, S: Span, F> From<HirIce<'b, S>> for HirError<'b, S, F> {
    fn from(value: HirIce<'b, S>) -> Self {
        Self::Ice(value)
    }
}
impl<'b, S: Span, F: Debug + SourceCode> Diagnostic for HirError<'b, S, F> {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Self::Ice(i) => i.labels(),
            Self::DuplicateDefinition { span, .. } => Some(Box::new(std::iter::once(
                LabeledSpan::at(*span, "previously defined here"),
            ))),
            Self::UnboundVariable { span, .. } => {
                Some(Box::new(std::iter::once(LabeledSpan::underline(*span))))
            }
        }
    }
    fn related(&self) -> Option<Box<dyn Iterator<Item = &dyn Diagnostic> + '_>> {
        match self {
            Self::DuplicateDefinition { prev, .. } => Some(Box::new(std::iter::once(prev as _))),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Error)]
#[error("previously defined here")]
pub struct PrevDef<S, F> {
    pub file: F,
    pub span: S,
}
impl<S: Debug, F: Debug> Debug for PrevDef<S, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("PrevDef")
            .field("span", &self.span)
            .field("file", &self.file)
            .finish()
    }
}
impl<S: Debug + Span, F: Debug + SourceCode> Diagnostic for PrevDef<S, F> {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.file)
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(LabeledSpan::new_with_span(
            Some("previously defined here".to_string()),
            self.span,
        ))))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Error)]
#[error("Error reporter requested early termination")]
pub struct EarlyReturn;

pub type LowerResult = Result<(), EarlyReturn>;
