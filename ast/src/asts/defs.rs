use super::*;
use crate::dottedname::DottedName;

/// A function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'src, A: Located> {
    pub name: Cow<'src, str>,
    pub loc: A::Span,
    pub ty: Option<A>,
}

/// Either a variable or function definition. A variable is just a nullary function at this point.
#[derive(Derivative, PartialEq)]
#[derivative(Debug, Clone)]
pub struct LetAST<'src, A: Located> {
    pub kw: A::Span,
    #[derivative(Debug(format_with = "bstr_debug"))] 
    pub doc: Cow<'src, [u8]>,
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
