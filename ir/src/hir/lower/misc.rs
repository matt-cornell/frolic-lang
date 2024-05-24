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
        use std::fmt::Write;
        if self.global.is_none() {
            if let Some(&v) = loc.locals.lookup(&*self.name) {
                return (Operand::Inst(v), Ok(()));
            }
            let mut storage = String::new();
            for pre in loc.global_prefixes.iter().rev() {
                let _ = write!(storage, "{pre}.{}", self.name);
                if let Some(&(_, v)) = glb.global_syms.get(&*storage) {
                    return (Operand::Global(v), Ok(()));
                }
            }
        }
        let name = glb.intern_cow(&self.name);
        if let Some(&(_, v)) = glb.global_syms.get(name) {
            return (Operand::Global(v), Ok(()));
        }
        (
            const_err(),
            (glb.report)(HirError::UnboundVariable {
                name,
                span: self.loc(),
            }),
        )
    }
}
