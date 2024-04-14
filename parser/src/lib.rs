use frolic_utils::prelude::*;
use par::dispatch_chunks;

pub mod lexer;
pub mod par;
pub mod parser;

pub mod prelude {
    pub use crate::lexer::tokenizer::tokenize;
    pub use crate::lexer::tokens::{self, Token, TokenKind};
}
