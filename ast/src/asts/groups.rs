use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FrolicAST<A, F: Copy> {
    pub file: F,
    pub nodes: Vec<A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParenAST<A: Located> {
    pub inner: A,
    pub loc: A::Span,
}
impl<A: Located> Located for ParenAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}
