use frolic_utils::prelude::*;
use lexer::tokens::*;
use miette::SourceSpan;
use par::dispatch_chunks;

pub mod lexer;
mod par;
pub mod parser;

pub mod prelude {
    pub use crate::lexer::tokenizer::tokenize;
    pub use crate::lexer::tokens::{self, Token, TokenKind};
    pub use crate::parser::{parse_expr, parse_tl};
}
