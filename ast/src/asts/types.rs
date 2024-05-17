use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct AscribeAST<A: Located> {
    pub kw: A::Span,
    pub val: A,
    pub ty: A,
}
impl<A: Located> Located for AscribeAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.val.loc().merge(self.kw).merge(self.ty.loc())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastAST<A: Located> {
    pub kw: A::Span,
    pub val: A,
    pub ty: A,
}
impl<A: Located> Located for CastAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.val.loc().merge(self.kw).merge(self.ty.loc())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtendAST<A: Located> {
    pub kw: A::Span,
    pub val: A,
    pub ext: A,
}
impl<A: Located> Located for ExtendAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.val.loc().merge(self.kw).merge(self.ext.loc())
    }
}
