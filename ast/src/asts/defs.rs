use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'src, A: Located> {
    pub name: Cow<'src, str>,
    pub loc: A::Span,
    pub ty: A,
    pub default: Option<A>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDefAST<'src, A: ExpressionAST + Located> {
    pub kw: A::Span,
    pub oparen: A::Span,
    pub cparen: A::Span,
    pub name: DottedName<'src, A::Span>,
    pub params: SmallVec<[FnParam<'src, A>; 2]>,
    pub ret: Option<A>,
    pub body: Option<A>,
}
impl<A: ExpressionAST + Located> Located for FnDefAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        if let Some(body) = &self.body {
            self.kw.merge(body.loc())
        } else if let Some(ret) = &self.ret {
            self.kw.merge(ret.loc())
        } else {
            self.kw.merge(self.cparen)
        }
    }
}
impl<A: ExpressionAST + Located> StatementAST for FnDefAST<'_, A> {}
impl<A: ExpressionAST + Located> TopLevelAST for FnDefAST<'_, A> {}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDefAST<'src, A: ExpressionAST + Located> {
    pub kw: A::Span,
    pub name: DottedName<'src, A::Span>,
    pub ty: Option<A>,
    pub val: Option<A>,
}
impl<A: ExpressionAST + Located> Located for VarDefAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        if let Some(val) = &self.val {
            self.kw.merge(val.loc())
        } else if let Some(ty) = &self.ty {
            self.kw.merge(ty.loc())
        } else {
            self.kw.merge(self.name.loc())
        }
    }
}
