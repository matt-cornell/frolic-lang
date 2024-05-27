use super::*;
use vec1::smallvec_v1::SmallVec1;

/// The top-level AST.
#[derive(Debug, Clone, PartialEq)]
pub struct FrolicAST<A, F> {
    pub file: F,
    pub nodes: Vec<A>,
}
/// Note that this requires `SpanConstruct`, we need to create our own span if there are no nodes.
impl<S: SpanConstruct, A: Located<Span = S>, F> Located for FrolicAST<A, F> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        match &self.nodes[..] {
            [] => S::new(0, 0),
            [n] => n.loc(),
            _ => self.nodes[0].loc().merge(self.nodes.last().unwrap().loc()),
        }
    }
}

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
