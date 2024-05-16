use super::*;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::FunctionTypeAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (arg, err) = self.arg.local(glb, loc);
        if err {
            return (const_err(), true);
        }
        let (ret, err) = self.ret.local(glb, loc);
        if err {
            return (const_err(), true);
        }

        let inst = Instruction {
            name: "".into(),
            span: self.loc(),
            kind: InstKind::FunctionTy { arg, ret },
        };

        let i = glb.module.intern_inst(inst);

        loc.insert_blk.insts.push(i);

        (Operand::Instruction(i), false)
    }
}

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::ShortCircuitAST<A> {}
