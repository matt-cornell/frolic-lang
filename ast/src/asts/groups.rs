use super::*;

/// Some expression wrapped in parentheses, give it a new location.
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
