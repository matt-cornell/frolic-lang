use super::*;
use smallvec::smallvec;

fn ucb_get_blk<'a, 'src: 'a, S>(term: &'a mut Terminator<'src, S>) -> &'a mut BlockId<'src, S> {
    if let Terminator::UncondBr(br) = term {
        br
    } else {
        unreachable!("ICE: modified return")
    }
}

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::IfElseAST<A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        let (cond, erred) = self.cond.local(glb, loc);
        if erred {
            return (Operand::Constant(Constant::Error), true);
        }

        let f = loc.insert_func;

        let cond_blk = loc.push_swap_blk(&glb.module, Block::new("if_true"));

        loc.block_term()
            .set(Terminator::UncondBr(BlockId::invalid()));
        let (if_true_val, erred) = self.if_true.local(glb, loc);

        debug_assert_eq!(
            loc.insert_func, f,
            "ICE: modified insert function in the middle of if/else"
        );

        let if_true = loc.push_swap_blk(&glb.module, Block::new("if_false"));

        loc.block_term()
            .set(Terminator::UncondBr(BlockId::invalid()));
        let (if_false_val, erred) = if erred {
            (const_err(), true)
        } else {
            self.cond.local(glb, loc)
        };

        debug_assert_eq!(
            loc.insert_func, f,
            "ICE: modified insert function in the middle of if/else"
        );

        let if_false = loc.push_swap_blk(&glb.module, Block::new("if_false"));

        let func = &glb.module[f];
        func[cond_blk].term.set(Terminator::CondBr {
            cond,
            if_true,
            if_false,
        });
        loc.lazy_block_id(func[if_true].term.map_ref(ucb_get_blk));
        loc.lazy_block_id(func[if_false].term.map_ref(|term| {
            if let Terminator::UncondBr(br) = term {
                br
            } else {
                unreachable!("ICE: modified return")
            }
        }));

        let ret = if erred {
            const_err()
        } else {
            let inst = Instruction {
                name: "if_else".into(),
                span: self.loc(),
                kind: InstKind::Phi(smallvec![(if_true, if_true_val), (if_false, if_false_val)]),
            };
            let id = glb.module.intern_inst(inst);
            loc.push_inst(id);
            Operand::Instruction(id)
        };
        (ret, erred)
    }
}
