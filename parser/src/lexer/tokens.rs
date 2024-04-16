use super::*;
use strum::*;

#[derive(Debug, Clone, Copy, PartialEq, Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Debug, Clone, Copy, PartialEq, Display, EnumString)]
pub enum Keyword {
    Def,
    Let,
    If,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CommentKind {
    Ignore,
    OuterDoc,
    InnerDoc,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'src> {
    Comment(Cow<'src, [u8]>, CommentKind),
    Ident(&'src str),
    Keyword(Keyword),
    Open(Delim),
    Close(Delim),
    Int(i128),
    Float(f64),
    Char(u32),
    String(Cow<'src, [u8]>),
}

impl<'src> TokenKind<'src> {
    /// Convenience method to get either a keyword or ident
    #[inline]
    pub fn from_ident(i: &'src str) -> Self {
        if let Ok(kw) = i.parse() {
            Self::Keyword(kw)
        } else {
            Self::Ident(i)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src, S> {
    pub kind: TokenKind<'src>,
    pub span: S,
}
impl<'src, S> Token<'src, S> {
    pub fn map_span<T, F: FnOnce(S) -> T>(self, op: F) -> Token<'src, T> {
        Token {
            kind: self.kind,
            span: op(self.span),
        }
    }
}
impl<S: Span> Located for Token<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.span
    }
}
