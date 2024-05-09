use super::*;

impl<'src, F, A: ToHir<'src, F>> ToHir<'src, F> for asts::IfElseAST<A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let (cond, ret) = self.cond.to_hir(glb, loc);
        let (false, Some(cond), Some(func)) = (ret, cond, loc.builder.get_func()) else {
            return (None, ret);
        };
        let span = self.loc();
        let if_true_blk = func.append_new_block();
        let if_false_blk = func.append_new_block();
        let merge = func.append_new_block();
        loc.builder.append(Box::new(Value::cond_br(
            &cond,
            &if_true_blk,
            &if_false_blk,
            span,
            "",
        )));
        loc.builder.position_at(&if_true_blk);
        let (if_true, ret) = self.if_true.to_hir(glb, loc);
        loc.builder
            .append(Box::new(Value::uncond_br(&merge, span, "")));
        loc.builder.position_at(&if_false_blk);
        if ret {
            loc.builder
                .append(Box::new(Value::uncond_br(&merge, span, "")));
            loc.builder.position_at(&merge);
            return (None, true);
        }
        let (if_false, ret) = self.if_false.to_hir(glb, loc);
        loc.builder
            .append(Box::new(Value::uncond_br(&merge, span, "")));
        loc.builder.position_at(&merge);
        let val = if let (false, Some(if_true), Some(if_false)) = (ret, if_true, if_false) {
            Some(loc.builder.append(Box::new(Value::phi(
                &if_true_blk,
                &if_true,
                &if_false,
                span,
                "",
            ))))
        } else {
            None
        };
        (val, ret)
    }
}
