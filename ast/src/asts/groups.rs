use super::*;
use vec1::smallvec_v1::SmallVec1;

/// Some expression wrapped in parentheses, giving it a new location.
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

/// Some expression wrapped in braces, giving it a new location and scope.
#[derive(Debug, Clone, PartialEq)]
pub struct BraceAST<A: Located> {
    pub inner: A,
    pub loc: A::Span,
}
impl<A: Located> Located for BraceAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// A sequence of nodes, separated by semicolons.
#[derive(Debug, Clone, PartialEq)]
pub struct SeqAST<A> {
    pub nodes: SmallVec1<[A; 2]>,
}
impl<A: Located> Located for SeqAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        let first = self.nodes.first().loc();
        if self.nodes.len() == 1 {
            first
        } else {
            first.merge(self.nodes.last().loc())
        }
    }
}
