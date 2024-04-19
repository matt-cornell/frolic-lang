use super::*;

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
