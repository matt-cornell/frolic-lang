use super::*;
use std::fmt::{self, Debug, Formatter};
use strum::*;

/// A kind of delimiter.
#[derive(Debug, Clone, Copy, PartialEq, Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum Delim {
    Paren,
    Brace,
    Bracket,
}
impl Delim {
    /// Get the corresponding character. If `is_close`, return the closing character, else the
    /// opening one.
    pub const fn get_char(self, is_close: bool) -> char {
        match (self, is_close) {
            (Self::Paren, false) => '(',
            (Self::Paren, true) => ')',
            (Self::Brace, false) => '{',
            (Self::Brace, true) => '}',
            (Self::Bracket, false) => '[',
            (Self::Bracket, true) => ']',
        }
    }
}

/// A special language keyword.
#[derive(Debug, Clone, Copy, PartialEq, Display, IntoStaticStr, EnumString)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Let,
    Of,
    As,
    If,
    Then,
    Else,
    For,
}
impl Keyword {
    pub fn as_str(self) -> &'static str {
        self.into()
    }
}

/// A kind of comment.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CommentKind {
    /// An ignored comment, becomes a `CommentAST`.
    Ignore,
    /// The "normal" kind of doc comment, to be applied to a definition.
    OuterDoc,
    /// An inner doc comment, to be used on the inside of a module.
    InnerDoc,
}

/// A special character or sequence
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecialChar {
    Semicolon,
    Colon,
    /// This is the `::` sequence
    DoubleColon,
    Backslash,
    Equals,
    Dot,
    /// this is the `->` sequence
    Arrow,
}

/// An operator that could be either prefix or infix depending on position
#[derive(Debug, Clone, Copy, PartialEq, FromRepr)]
#[repr(u8)]
pub enum AmbigOp {
    Plus = b'+',
    Minus = b'-',
    Star = b'*',
    And = b'&',
}
impl AmbigOp {
    /// Get the function name if this is an infix.
    pub fn as_inf_str(self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::And => "&",
        }
    }
    /// Get the function name if this is a prefix-- with a leading `~`.
    pub fn as_pre_str(self) -> &'static str {
        match self {
            Self::Plus => "~+",
            Self::Minus => "~-",
            Self::Star => "~*",
            Self::And => "~&",
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum TokenKind<'src> {
    /// A comment. Will be `Borrowed` if it's a single comment, or `Owned` if the combination of
    /// multiple.
    Comment(Cow<'src, [u8]>, CommentKind),
    /// An identifier-- an XID start character followed by 0 or more XID continues
    Ident(&'src str),
    Keyword(Keyword),
    /// `(`, `[`, or `{`
    Open(Delim),
    /// `)`, `]`, or `}`
    Close(Delim),
    Int(i64),
    Float(f64),
    Char(u32),
    /// A string literal. Will be borrowed if possible, but must be `Owned` if there are escape
    /// sequences.
    String(Cow<'src, [u8]>),
    Special(SpecialChar),
    PreOp(&'src str),
    InfOp(&'src str),
    AmbigOp(AmbigOp),
}
/// Manually implemented so we can use `bstr` for comments and strings.
impl Debug for TokenKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comment(comm, kind) => f
                .debug_tuple("Comment")
                .field(&bstr::BStr::new(comm))
                .field(kind)
                .finish(),
            Self::Ident(id) => f.debug_tuple("Ident").field(id).finish(),
            Self::Keyword(kw) => f.debug_tuple("Keyword").field(kw).finish(),
            Self::Open(delim) => f.debug_tuple("Open").field(delim).finish(),
            Self::Close(delim) => f.debug_tuple("Close").field(delim).finish(),
            Self::Int(val) => f.debug_tuple("Int").field(val).finish(),
            Self::Float(val) => f.debug_tuple("Float").field(val).finish(),
            Self::Char(val) => {
                let mut tup = f.debug_tuple("Char");
                if let Ok(ch) = char::try_from(*val) {
                    tup.field(&ch);
                } else {
                    tup.field(val);
                }
                tup.finish()
            }
            Self::String(val) => f
                .debug_tuple("String")
                .field(&bstr::BStr::new(val))
                .finish(),
            Self::Special(sc) => f.debug_tuple("Special").field(sc).finish(),
            Self::PreOp(op) => f.debug_tuple("PreOp").field(op).finish(),
            Self::InfOp(op) => f.debug_tuple("InfOp").field(op).finish(),
            Self::AmbigOp(ch) => f
                .debug_tuple("AmbigOp")
                .field(&(*ch as u8 as char))
                .finish(),
        }
    }
}

impl<'src> TokenKind<'src> {
    /// An empty comment, ignored everywhere
    pub const EMPTY_COMMENT: Self = Self::Comment(Cow::Borrowed(&[]), CommentKind::Ignore);

    /// Convenience method to get either a keyword or ident
    #[inline]
    pub fn from_ident(i: &'src str) -> Self {
        if let Ok(kw) = i.parse() {
            Self::Keyword(kw)
        } else {
            Self::Ident(i)
        }
    }

    /// Get this as a string for an infix operator. Matches infix operators, ambiguous operators,
    /// and `->`.
    #[inline]
    pub fn inf_op_str(&self) -> Option<&'src str> {
        match self {
            Self::InfOp(op) => Some(op),
            Self::AmbigOp(op) => Some(op.as_inf_str()),
            Self::Special(SpecialChar::Arrow) => Some("->"),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src, S> {
    pub kind: TokenKind<'src>,
    pub span: S,
}
impl<'src, S> Token<'src, S> {
    /// Map the span of a token to create a new one.
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
/// Implement this so that a `Token` can be used directly as a `#[label]` in diagnostics
impl<S: Span> From<Token<'_, S>> for miette::SourceSpan {
    fn from(value: Token<'_, S>) -> Self {
        value.span.into()
    }
}
