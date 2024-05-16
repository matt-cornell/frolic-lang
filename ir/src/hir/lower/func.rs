use super::*;


// TODO: implement
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::LambdaAST<'src, A> {}

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::CallAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (func, false) = self.func.local(glb, loc) else {
            return (const_err(), true)
        };

        let (arg, false) = self.arg.local(glb, loc) else {
            return (const_err(), true)
        };

        let inst = Instruction {
            name: "".into(),
            span: self.loc(),
            kind: InstKind::Call { func, arg }
        };

        let id = glb.module.intern_inst(inst);

        (Operand::Instruction(id), false)
    }
}