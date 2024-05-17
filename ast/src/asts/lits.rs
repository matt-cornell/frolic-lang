use super::*;

/// An integer literal.
#[derive(Debug, Clone, PartialEq)]
pub struct IntLitAST<S> {
    pub loc: S,
    pub val: i64,
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
