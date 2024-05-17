use super::*;
use smallvec::smallvec;

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

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::ShortCircuitAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (cond, erred) = self.lhs.local(glb, loc);
        if erred {
            return (const_err(), true);
        }

        let f = loc.insert_func;

        let cond_blk = loc.push_swap_blk(&glb.module, Block::new("alt"));

        loc.block_term()
            .set(Terminator::UncondBr(BlockId::invalid()));
        let (alt_val, erred) = self.rhs.local(glb, loc);

        debug_assert_eq!(
            loc.insert_func, f,
            "ICE: modified insert function in the middle of if/else"
        );

        let alt_blk = loc.push_swap_blk(&glb.module, Block::new("merge"));
        let func = &glb.module[f];
        
        loc.lazy_block_id(func[alt_blk].term.map_ref(|term| {
            if let Terminator::UncondBr(br) = term {
                br
            } else {
                unreachable!("ICE: modified return")
            }
        }));
        if self.is_or {
            func[cond_blk].term.set(Terminator::CondBr {
                cond: cond.clone(),
                if_true: BlockId::invalid(),
                if_false: alt_blk,
            });
            loc.lazy_block_id(func[cond_blk].term.map_ref(|term| {
                if let Terminator::CondBr { if_true: br, .. } = term {
                    br
                } else {
                    unreachable!("ICE: modified return")
                }
            }));
        } else {
            func[cond_blk].term.set(Terminator::CondBr {
                cond: cond.clone(),
                if_true: alt_blk,
                if_false: BlockId::invalid(),
            });
            loc.lazy_block_id(func[cond_blk].term.map_ref(|term| {
                if let Terminator::CondBr { if_false: br, .. } = term {
                    br
                } else {
                    unreachable!("ICE: modified return")
                }
            }));

        }
        let ret = if erred {
            const_err()
        } else {
            let inst = Instruction {
                name: "sc_op".into(),
                span: self.loc(),
                kind: InstKind::Phi(smallvec![(alt_blk, alt_val), (cond_blk, cond)]),
            };
            let id = glb.module.intern_inst(inst);
            loc.push_inst(id);
            Operand::Instruction(id)
        };
        (ret, erred)
    }
}
