use super::*;

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::AscribeAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (val, Ok(())) = self.val.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let (ty, erred) = self.ty.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::Ascribe { val, ty },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
impl<'b, F: Copy, A: ToHir<'b, F>> ToHir<'b, F> for asts::CastAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (val, Ok(())) = self.val.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let (ty, erred) = self.ty.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::Cast { val, ty },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::ExtendAST<A> {}
