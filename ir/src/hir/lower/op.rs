use super::*;

impl<'b, 'src: 'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::PreOpAST<'src, A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let s = match &*self.op {
            "+" => &Cow::Borrowed("~+"),
            "-" => &Cow::Borrowed("~-"),
            "*" => &Cow::Borrowed("~*"),
            "&" => &Cow::Borrowed("~&"),
            _ => &self.op,
        };
        let (func, erred) = loc.lookup(self.oploc, &s, false, glb);
        if erred.is_err() {
            return (const_err(), erred);
        }
        let (val, erred) = self.val.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::Call { func, arg: val },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
impl<'b, 'src: 'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::InfOpAST<'src, A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (func, erred) = loc.lookup(self.oploc, &self.op, false, glb);
        if erred.is_err() {
            return (const_err(), erred);
        }
        let (lhs, erred) = self.lhs.local(glb, loc);
        let span = self.loc();
        let inst1 = glb
            .alloc
            .alloc(Inst {
                name: "",
                span,
                kind: InstKind::Call { func, arg: lhs },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst1);
        let (rhs, erred) = if erred.is_err() {
            (const_err(), erred)
        } else {
            self.rhs.local(glb, loc)
        };
        let inst2 = glb
            .alloc
            .alloc(Inst {
                name: "",
                span,
                kind: InstKind::Call {
                    func: Operand::Inst(Id(inst1)),
                    arg: rhs,
                },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst2);
        (Operand::Inst(Id(inst2)), erred)
    }
}

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::ShortCircuitAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (cond, Ok(())) = self.lhs.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let cond_blk = loc.insert.0;
        let merge = Id(glb.alloc.alloc(Block::new("merge")).into_ref());
        let alt = Id(glb.alloc.alloc(Block::new("if_true")).into_ref());
        alt.0.term.set(Terminator::UncondBr { blk: merge });
        loc.insert = alt;
        let (alt_val, erred) = self.rhs.local(glb, loc);
        cond_blk.term.set(if self.is_or {
            Terminator::CondBr {
                cond,
                if_true: merge,
                if_false: alt,
            }
        } else {
            Terminator::CondBr {
                cond,
                if_true: alt,
                if_false: merge,
            }
        });
        loc.insert = merge;
        let opts = glb
            .alloc
            .alloc([(alt, alt_val), (Id(cond_blk), cond)])
            .into_ref();
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::Phi(opts),
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::FunctionTypeAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (arg, Ok(())) = self.arg.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let (ret, erred) = self.ret.local(glb, loc);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.loc(),
                kind: InstKind::FnType { arg, ret },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst);
        (Operand::Inst(Id(inst)), erred)
    }
}
