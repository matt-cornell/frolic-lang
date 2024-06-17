#![feature(unsize)]

use frolic_utils::prelude::*;
use lexer::tokens::*;
use std::borrow::Cow;

pub mod lexer;
pub mod macros;
pub mod parser;

pub mod prelude {
    pub use crate::lexer::tokenizer::tokenize;
    pub use crate::lexer::tokens::{self, Token, TokenKind};
    pub use crate::macros::fold_macros;
    pub use crate::parser::traits::*;
    pub use crate::parser::{parse_expr, parse_tl};
}
