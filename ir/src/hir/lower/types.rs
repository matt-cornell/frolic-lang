use super::*;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::AscribeAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (val, false) = self.val.local(glb, loc) else {
            return (const_err(), true);
        };

        let (ty, false) = self.ty.local(glb, loc) else {
            return (const_err(), true);
        };

        let inst = Instruction {
            name: "".into(),
            span: self.loc(),
            kind: InstKind::Ascribe { val, ty },
        };

        let id = glb.module.intern_inst(inst);

        loc.push_inst(id);

        (Operand::Instruction(id), false)
    }
}
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::CastAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (val, false) = self.val.local(glb, loc) else {
            return (const_err(), true);
        };

        let (ty, false) = self.ty.local(glb, loc) else {
            return (const_err(), true);
        };

        let inst = Instruction {
            name: "".into(),
            span: self.loc(),
            kind: InstKind::Cast { val, ty },
        };

        let id = glb.module.intern_inst(inst);

        loc.push_inst(id);

        (Operand::Instruction(id), false)
    }
}
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::ExtendAST<A> {}
