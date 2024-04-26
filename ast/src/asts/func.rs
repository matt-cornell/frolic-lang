use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct CallAST<A> {
    pub func: A,
    pub arg: A,
}
impl<A: Located> Located for CallAST<A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.func.loc().merge(self.arg.loc())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaAST<'src, A: Located> {
    pub bs: A::Span,
    pub arg: Cow<'src, str>,
    pub aloc: A::Span,
    pub argty: Option<A>,
    pub retty: Option<A>,
    pub body: A,
}
impl<A: Located> Located for LambdaAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.bs.merge(self.body.loc())
    }
}
