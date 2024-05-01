use super::*;

impl<'src, A: ToHir<'src>> ToHir<'src> for asts::ShortCircuitAST<A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let (cond, ret) = self.lhs.to_hir(glb, loc);
        let (false, Some(cond), Some(func)) = (ret, cond, loc.builder.get_func()) else {
            return (None, ret)
        };
        let rhs_blk = func.append_new_block();
        let merge = func.append_new_block();
        if self.is_or {
            loc.builder.append(Box::new(Value::cond_br(&cond, &merge, &rhs_blk, self.loc(), "")));
        } else {
            loc.builder.append(Box::new(Value::cond_br(&cond, &rhs_blk, &merge, self.loc(), "")));
        }
        loc.builder.position_at(&merge);
        let (rhs, ret) = self.rhs.to_hir(glb, loc);
        loc.builder.append(Box::new(Value::uncond_br(&merge, self.loc(), "")));
        (rhs.map(|rhs| loc.builder.append(Box::new(Value::phi(&rhs_blk, &rhs, &cond, self.loc(), "")))), ret)
    }
}

impl<'src, A: ToHir<'src>> ToHir<'src> for asts::FunctionTypeAST<A> {
    fn to_hir(&self, _glb: &GlobalContext<'_, 'src, Self::Span>, _loc: &mut LocalContext<'src, Self::Span>) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        // TODO
        (None, false)
    }
}
