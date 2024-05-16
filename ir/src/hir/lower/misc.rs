use super::*;

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::CommentAST<'src, S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::Error), false)
    }
    fn global(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        _loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        false
    }
}

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::ErrorAST<S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::Error), false)
    }
    fn global(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        _loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        false
    }
}

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::NullAST<S> {
    fn local<'l, 'g: 'l>(
        &self,
        _glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (Operand::Constant(Constant::Null), false)
    }
    fn global(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        _loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        false
    }
}

impl<'src, S: Span, F: Copy> ToHir<'src, F> for asts::VarAST<'src, S> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        if self.global.is_none() {
            if let Some(val) = loc.locals.lookup(&self.name) {
                return (Operand::Instruction(val.clone()), false);
            }
            if let Some(&val) = loc.globals.lookup(&self.name) {
                return (Operand::Global(val), false);
            }
        } else {
            if let Some(&val) = loc.globals.scopes[0].get(&self.name) {
                return (Operand::Global(val), false);
            }
        }
        (Operand::Constant(Constant::Error), (glb.report)(HirError::UnresolvedName {
            name: self.name.clone(),
            span: self.loc(),
        }))
    }
}
