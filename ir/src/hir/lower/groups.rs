use super::*;

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::ParenAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (val, erred) = self.inner.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "()",
                span: self.loc,
                kind: InstKind::Bind(val),
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::BraceAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        loc.locals.push_new_scope();
        let (val, erred) = self.inner.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "{}",
                span: self.loc,
                kind: InstKind::Bind(val),
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        loc.locals.pop_scope();
        (Operand::Inst(Id(inst)), erred)
    }
}

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::SeqAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let mut res = Operand::Const(Constant::Null);
        let mut erred = Ok(());
        // this is guaranteed to execute at least once, but we can't express that easily
        for v in &self.nodes {
            (res, erred) = v.local(glb, loc);
            if erred.is_err() {
                return (res, erred);
            }
        }
        (res, erred)
    }
}
