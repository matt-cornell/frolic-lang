use super::*;

impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::IntLitAST<S> {
    fn local(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (Operand::Const(Constant::Int(self.val)), Ok(()))
    }
}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::FloatLitAST<S> {
    fn local(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (Operand::Const(Constant::Float(self.val)), Ok(()))
    }
}
impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::StringLitAST<'src, S> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let val = glb.alloc.alloc_slice_copy(&self.val).into_ref();
        (Operand::Const(Constant::String(val)), Ok(()))
    }
}
