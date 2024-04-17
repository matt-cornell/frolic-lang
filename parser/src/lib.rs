use frolic_utils::prelude::*;
use miette::SourceSpan;
use par::dispatch_chunks;
use lexer::tokens::*;

pub mod lexer;
mod par;
pub mod parser;

pub mod prelude {
    pub use crate::lexer::tokenizer::tokenize;
    pub use crate::lexer::tokens::{self, Token, TokenKind};
    pub use crate::parser::{parse_tl, parse_expr};
}
