use super::*;

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
