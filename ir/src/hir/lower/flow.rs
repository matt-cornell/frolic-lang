use super::*;

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::IfElseAST<A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let (cond, Ok(())) = self.cond.local(glb, loc) else {
            return (const_err(), Err(EarlyReturn));
        };
        let parent = loc.insert.0.parent(Ordering::Relaxed).unwrap();
        let cond_blk = loc.insert.0;
        let merge = Id(glb.alloc.alloc(Block::new("merge")).into_ref());
        let if_true = Id(glb.alloc.alloc(Block::new("if_true")).into_ref());
        parent.push_back(if_true.0);
        if_true.0.term.set(Terminator::UncondBr { blk: merge });
        loc.insert = if_true;
        let (if_true_val, erred) = self.if_true.local(glb, loc);
        if erred.is_err() {
            cond_blk.term.set(Terminator::CondBr {
                cond,
                if_true,
                if_false: merge,
            });
            loc.insert = merge;
            let opts = glb
                .alloc
                .alloc([(if_true, if_true_val), (Id(cond_blk), const_err())])
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
            parent.push_back(merge.0);
            return (Operand::Inst(Id(inst)), erred);
        }
        let if_false = Id(glb.alloc.alloc(Block::new("if_false")).into_ref());
        parent.push_back(if_false.0);
        parent.push_back(merge.0);
        if_false.0.term.set(Terminator::UncondBr { blk: merge });
        loc.insert = if_false;
        let (if_false_val, erred) = self.if_false.local(glb, loc);
        cond_blk.term.set(Terminator::CondBr {
            cond,
            if_true,
            if_false,
        });
        loc.insert = merge;
        let opts = glb
            .alloc
            .alloc([(if_true, if_true_val), (if_false, if_false_val)])
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
