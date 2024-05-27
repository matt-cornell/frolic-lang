use super::*;

/// Kind of an integer literal.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IntLitKind {
    #[default]
    Dec = 10,
    Bin = 2,
    Oct = 8,
    Hex = 16,
    Char = 0,
}

/// An integer literal.
#[derive(Debug, Clone, PartialEq)]
pub struct IntLitAST<S> {
    pub loc: S,
    pub val: i64,
    pub kind: IntLitKind,
}
impl<S: Span> Located for IntLitAST<S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// A floating-point literal.
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLitAST<S> {
    pub loc: S,
    pub val: f64,
}
impl<S: Span> Located for FloatLitAST<S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// A string literal.
#[derive(Derivative)]
#[derivative(Debug, Clone, PartialEq)]
pub struct StringLitAST<'src, S> {
    pub loc: S,
    #[derivative(Debug(format_with = "bstr_debug"))]
    pub val: Cow<'src, [u8]>,
}
impl<S: Span> Located for StringLitAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}
