use super::*;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::ParenAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (mut op, ret) = self.inner.local(glb, loc);

        let inst = Instruction {
            name: "()".into(),
            span: self.loc(),
            kind: InstKind::Bind(op),
        };
        let i = glb.module.intern_inst(inst);
        loc.insert_blk.insts.push(i);
        op = Operand::Instruction(i);

        (op, ret)
    }
}
