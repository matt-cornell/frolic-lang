use super::*;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::LambdaAST<'src, A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let bloc = self.body.loc();
        loc.scope_name.push(format!("[\\{}-{}]", bloc.offset(), bloc.offset() + bloc.end()).into());
        loc.locals.push_new_scope();
        let gid = glb.module.push_global(Global::new(loc.glb_segs_base("")));
        let erred = loc.with_new_loc(gid, Block::new("entry"), glb.module, |loc| {
            let inst = Instruction {
                name: self.arg.clone(),
                span: self.aloc,
                kind: InstKind::ArgOf { func: gid },
            };
            let i = glb.module.intern_inst(inst);
            loc.push_inst(i);
            loc.locals.insert(self.arg.clone(), i);

            let (ret, erred) = self.body.local(glb, loc);
            loc.block_term().set(Terminator::Return(ret));
            erred
        });
        loc.scope_name.pop();
        loc.locals.pop_scope();
        (Operand::Global(gid), erred)
    }
}

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::CallAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (func, false) = self.func.local(glb, loc) else {
            return (const_err(), true);
        };

        let (arg, false) = self.arg.local(glb, loc) else {
            return (const_err(), true);
        };

        let inst = Instruction {
            name: "".into(),
            span: self.loc(),
            kind: InstKind::Call { func, arg },
        };

        let id = glb.module.intern_inst(inst);

        loc.push_inst(id);

        (Operand::Instruction(id), false)
    }
}
