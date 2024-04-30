use super::*;
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum HirError {}
