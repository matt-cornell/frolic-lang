use super::*;

/// A conditional expression, with a form of either `if cond then if_true else if_false` or
/// `if_true if cond else false`.
#[derive(Debug, Clone, PartialEq)]
pub struct IfElseAST<A: Located> {
    pub kw: A::Span,
    pub cond: A,
    pub if_true: A,
    pub if_false: A,
}

impl<A: Located> Located for IfElseAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.kw.merge(self.if_true.loc()).merge(self.if_false.loc())
    }
}
