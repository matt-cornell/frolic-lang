use super::*;
use strum::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display, EnumString)]
pub enum Keyword {
    Def,
    Let,
    If,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind<'src> {
    Ident(&'src str),
    Keyword(Keyword),
    Open(Delim),
    Close(Delim),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'src, S> {
    pub kind: TokenKind<'src>,
    pub span: S,
}
impl<S: Span> Located for Token<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.span
    }
}
