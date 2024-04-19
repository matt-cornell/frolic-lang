use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'src, A: Located> {
    pub name: Cow<'src, str>,
    pub loc: A::Span,
    pub ty: Option<A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetAST<'src, A: Located> {
    pub kw: A::Span,
    pub name: DottedName<'src, A::Span>,
    pub params: SmallVec<[FnParam<'src, A>; 2]>,
    pub ret: Option<A>,
    pub body: A,
}
impl<A: Located> Located for LetAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.kw.merge(self.body.loc())
    }
}
