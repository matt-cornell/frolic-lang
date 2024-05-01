use super::*;

impl<'src, A: ToHir<'src>> ToHir<'src> for asts::LetAST<'src, A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        if loc.builder.get_pos().is_none() {
            let def = Box::new(Definition::new(Some(self.name.clone())));
            let blk = def.append_new_block();
            let res = loc.push_scope(
                self.name.segs.iter().map(|s| s.0.clone()),
                self.name.global.is_some(),
            );
            loc.builder.position_at(&blk);
            let (_body, ret) = self.body.to_hir(glb, loc);
            loc.restore_scope(res);
            glb.module.append(def);
            loc.builder.clear_pos();
            (None, ret)
        } else {
            let (_body, ret) = self.body.to_hir(glb, loc);
            (None, ret)
        }
    }
}
