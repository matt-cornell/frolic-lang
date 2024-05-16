use super::*;

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::IntLitAST<S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::Int(self.val)), false)
    }
}

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::FloatLitAST<S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::Float(self.val)), false)
    }
}

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::StringLitAST<'src, S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::String(self.val.clone())), false)
    }
}
