use super::*;

impl<'src, A: ToHir<'src>> ToHir<'src> for asts::LetAST<'src, A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let def = Box::new(Definition::new(Some(self.name.clone())));
        let blk = def.append_new_block();
        let res = loc.push_scope(
            self.name.segs.iter().map(|s| s.0.clone()),
            self.name.global.is_some(),
        );
        let old = loc.builder.get_pos();
        loc.builder.position_at(&blk);
        let ret = self.body.to_hir(glb, loc);
        loc.restore_scope(res);
        if let Some(old) = old {
            loc.builder.position_at(&old);
        } else {
            loc.builder.clear_pos();
        }
        ret
    }
}
