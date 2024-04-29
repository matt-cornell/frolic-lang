use super::*;

/// Short-circuiting operations *can't* be implemented as function calls because that would require
/// that
#[derive(Debug, Clone, PartialEq)]
pub struct ShortCircuitAST<A: Located> {
    pub oploc: A::Span,
    pub is_or: bool,
    pub lhs: A,
    pub rhs: A,
}
impl<A: Located> Located for ShortCircuitAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.lhs.loc().merge(self.rhs.loc())
    }
}

/// While this could've been implemented as a function, this is right-associative in contrast to
/// the expected left-associativity of the `->` operator. Also because this is so important and
/// shouldn't be overloaded, it's implemented as a special AST node rather than a function.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionTypeAST<A: Located> {
    pub oploc: A::Span,
    pub arg: A,
    pub ret: A,
}
impl<A: Located> Located for FunctionTypeAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.arg.loc().merge(self.ret.loc())
    }
}
