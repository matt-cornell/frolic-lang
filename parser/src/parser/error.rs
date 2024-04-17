use thiserror::Error;
use miette::{Diagnostic, SourceSpan};

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum ParseASTError {
    
}
