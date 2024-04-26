use super::*;

/// An integer literal.
#[derive(Debug, Clone, PartialEq)]
pub struct IntLitAST<S> {
    pub loc: S,
    pub val: i128,
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

/// A character literal.
#[derive(Debug, Clone, PartialEq)]
pub struct CharLitAST<S> {
    pub loc: S,
    pub val: u32,
}
impl<S: Span> Located for CharLitAST<S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// A string literal.
#[derive(Debug, Clone, PartialEq)]
pub struct StringLitAST<'src, S> {
    pub loc: S,
    pub val: Cow<'src, str>,
}
impl<S: Span> Located for StringLitAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}


