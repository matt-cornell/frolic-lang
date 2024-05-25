use super::*;

impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::CommentAST<'src, S> {
    fn global(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInGlobalContext,
    ) -> LowerResult {
        Ok(())
    }
    fn local(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (const_err(), Ok(()))
    }
}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::ErrorAST<S> {
    fn local(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (const_err(), Ok(()))
    }
}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::NullAST<S> {
    fn local(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (Operand::Const(Constant::Null), Ok(()))
    }
}
impl<'b, 'src: 'b, F: Clone, S: Span> ToHir<'b, F> for asts::VarAST<'src, S> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        loc.lookup(self.loc(), &self.name, self.global.is_some(), glb)
    }
}
