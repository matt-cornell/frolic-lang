use super::*;
use derivative::Derivative;
use std::fmt::{self, Debug, Formatter};
use strum::*;

fn bstr_debug<S: AsRef<[u8]>>(bytes: &S, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(bstr::BStr::new(bytes), f)
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
    Namespace,
    Using,
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
    Comma,
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

#[derive(Derivative)]
#[derivative(Debug, Clone, PartialEq)]
pub enum TokenKind<'src, S> {
    /// A comment. Will be `Borrowed` if it's a single comment, or `Owned` if the combination of
    /// multiple.
    Comment(
        #[derivative(Debug(format_with = "bstr_debug"))] Cow<'src, [u8]>,
        CommentKind,
    ),
    /// An identifier-- an XID start character followed by 0 or more XID continues
    Ident(Cow<'src, str>),
    Keyword(Keyword),
    Paren(Vec<Token<'src, S>>),
    Brace(Vec<Token<'src, S>>),
    Bracket(Vec<Token<'src, S>>),
    Int(i64),
    Float(f64),
    Char(u32),
    /// A string literal. Will be borrowed if possible, but must be `Owned` if there are escape
    /// sequences.
    String(#[derivative(Debug(format_with = "bstr_debug"))] Cow<'src, [u8]>),
    Special(SpecialChar),
    LetOp(Cow<'src, str>),
    PreOp(Cow<'src, str>),
    InfOp(Cow<'src, str>),
    AmbigOp(AmbigOp),
    UnboundMacro(Cow<'src, str>),
    BoundMacro(Cow<'src, str>, Box<Token<'src, S>>),
}

impl<'src, S> TokenKind<'src, S> {
    /// An empty comment, ignored everywhere
    pub const EMPTY_COMMENT: Self = Self::Comment(Cow::Borrowed(&[]), CommentKind::Ignore);

    /// Convenience method to get either a keyword or ident
    #[inline]
    pub fn from_ident(i: Cow<'src, str>) -> Self {
        if let Ok(kw) = i.parse() {
            Self::Keyword(kw)
        } else {
            Self::Ident(i)
        }
    }

    /// Get this as a string for an infix operator. Matches infix operators, ambiguous operators,
    /// and `->`.
    #[inline]
    pub fn inf_op_str(&self) -> Option<Cow<'src, str>> {
        match self {
            Self::InfOp(op) => Some(op.clone()),
            Self::AmbigOp(op) => Some(op.as_inf_str().into()),
            Self::Special(SpecialChar::Arrow) => Some("->".into()),
            _ => None,
        }
    }

    pub fn make_owned(&mut self) -> &mut TokenKind<'static, S> {
        match self {
            Self::Paren(ts) | Self::Brace(ts) | Self::Bracket(ts) => ts.iter_mut().for_each(|t| {
                t.kind.make_owned();
            }),
            Self::Comment(b, _) | Self::String(b) => drop(b.to_mut()),
            Self::Ident(s)
            | Self::LetOp(s)
            | Self::PreOp(s)
            | Self::InfOp(s)
            | Self::UnboundMacro(s) => drop(s.to_mut()),
            Self::BoundMacro(s, i) => {
                s.to_mut();
                i.kind.make_owned();
            }
            _ => {}
        }
        unsafe { std::mem::transmute::<&mut Self, &mut TokenKind<'static, S>>(self) }
    }
    pub fn into_static(mut self) -> TokenKind<'static, S> {
        self.make_owned();
        unsafe { std::mem::transmute::<Self, TokenKind<'static, S>>(self) }
    }
    pub fn into_static_boxed(mut self: Box<Self>) -> Box<TokenKind<'static, S>> {
        self.make_owned();
        unsafe { std::mem::transmute::<Box<Self>, Box<TokenKind<'static, S>>>(self) }
    }

    fn map_span_impl<T>(self, f: &mut dyn FnMut(S) -> T) -> TokenKind<'src, T> {
        use TokenKind::*;
        match self {
            Comment(c, k) => Comment(c, k),
            Ident(i) => Ident(i),
            Keyword(kw) => Keyword(kw),
            Paren(ts) => Paren(ts.into_iter().map(|t| t.map_span_impl(f)).collect()),
            Brace(ts) => Brace(ts.into_iter().map(|t| t.map_span_impl(f)).collect()),
            Bracket(ts) => Bracket(ts.into_iter().map(|t| t.map_span_impl(f)).collect()),
            Int(v) => Int(v),
            Float(v) => Float(v),
            Char(v) => Char(v),
            String(v) => String(v),
            Special(s) => Special(s),
            LetOp(o) => LetOp(o),
            PreOp(o) => PreOp(o),
            InfOp(o) => InfOp(o),
            AmbigOp(o) => AmbigOp(o),
            UnboundMacro(n) => UnboundMacro(n),
            BoundMacro(n, t) => BoundMacro(n, Box::new(t.map_span(f))),
        }
    }
    pub fn map_span<T, F: FnMut(S) -> T>(self, mut f: F) -> TokenKind<'src, T> {
        self.map_span_impl(&mut f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src, S> {
    pub kind: TokenKind<'src, S>,
    pub span: S,
}
impl<'src, S> Token<'src, S> {
    /// Take ownership of all data in this token.
    pub fn make_owned(&mut self) -> &mut Token<'static, S> {
        self.kind.make_owned();
        unsafe { std::mem::transmute::<&mut Self, &mut Token<'static, S>>(self) }
    }
    pub fn into_static(mut self) -> Token<'static, S> {
        self.make_owned();
        unsafe { std::mem::transmute::<Self, Token<'static, S>>(self) }
    }
    pub fn into_static_boxed(mut self: Box<Self>) -> Box<Token<'static, S>> {
        self.make_owned();
        unsafe { std::mem::transmute::<Box<Self>, Box<Token<'static, S>>>(self) }
    }

    fn map_span_impl<T>(self, f: &mut dyn FnMut(S) -> T) -> Token<'src, T> {
        Token {
            span: f(self.span),
            kind: self.kind.map_span_impl(f),
        }
    }
    pub fn map_span<T, F: FnMut(S) -> T>(self, mut f: F) -> Token<'src, T> {
        self.map_span_impl(&mut f)
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

impl<'lua, S: 'static> mlua::IntoLua<'lua> for Token<'static, S> {
    fn into_lua(self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        lua.create_any_userdata(self).map(mlua::Value::UserData)
    }
}
impl<'lua, S: Clone + 'static> mlua::FromLua<'lua> for Token<'static, S> {
    fn from_lua(value: mlua::Value<'lua>, _lua: &'lua mlua::Lua) -> mlua::Result<Self> {
        match value {
            mlua::Value::UserData(data) => Ok(data.borrow::<Self>()?.clone()),
            _ => Err(mlua::Error::FromLuaConversionError {
                from: value.type_name(),
                to: "Token",
                message: None,
            }),
        }
    }
}
