use super::*;

impl<'b, 'src: 'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LambdaAST<'src, A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        use std::fmt::Write;
        let sloc = self.loc();
        let old = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".[{}-{}]", sloc.offset(), sloc.end());
        loc.locals.push_new_scope();
        let arg = if let Some(ty) = &self.argty {
            let (ty, erred) = ty.local(glb, loc);
            if erred.is_err() {
                loc.locals.pop_scope();
                loc.scope_name.truncate(old);
                return (const_err(), erred);
            }
            ty
        } else {
            Operand::Const(Constant::Unknown)
        };
        let ret = if let Some(ty) = &self.retty {
            let (ty, erred) = ty.local(glb, loc);
            if erred.is_err() {
                loc.locals.pop_scope();
                loc.scope_name.truncate(old);
                return (const_err(), erred);
            }
            ty
        } else {
            Operand::Const(Constant::Unknown)
        };
        let inst = glb
            .alloc
            .alloc(Inst {
                name: glb.alloc.alloc_str(&loc.scope_name).into_ref(),
                span: sloc,
                kind: InstKind::FnType { arg, ret },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        let gid = Id(glb
            .alloc
            .alloc(Global {
                name: glb.alloc.alloc_str(&loc.scope_name).into_ref(),
                is_func: true,
                span: sloc,
                kind: GlobalKind::Local {
                    captures: Id(loc.insert.0.parent(Ordering::Relaxed).unwrap()),
                    ty: Operand::Inst(Id(inst)),
                },
                blocks: LinkedList::NEW,
                link: LinkedListLink::NEW,
            })
            .into_ref());
        glb.module.push_back(gid.0);
        let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
        gid.0.push_back(blk);
        let old_ins = std::mem::replace(&mut loc.insert, Id(blk));

        let name = glb.intern_cow(&self.arg);
        let inst = glb
            .alloc
            .alloc(Inst {
                name,
                span: self.aloc,
                kind: InstKind::Arg,
                link: LinkedListLink::NEW,
            })
            .into_ref();

        blk.push_back(inst);
        loc.locals.insert(name, Id(inst));

        let (ret, erred) = self.body.local(glb, loc);
        blk.term.set(Terminator::Return(ret));

        loc.insert = old_ins;
        loc.locals.pop_scope();
        loc.scope_name.truncate(old);
        (Operand::Global(gid), erred)
    }
}
impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::CallAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (func, Ok(())) = self.func.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let (arg, erred) = self.arg.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::Call { func, arg },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
