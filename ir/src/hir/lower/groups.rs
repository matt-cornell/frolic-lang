use super::*;

impl<'src, F, A: ToHir<'src, F>> ToHir<'src, F> for asts::ParenAST<A> {
    fn hoist_pass(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        self.inner.hoist_pass(glb, loc)
    }
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let (val, ret) = self.inner.to_hir(glb, loc);
        (
            val.map(|v| {
                loc.builder
                    .append(Value::new_loc(&v, self.loc(), ""))
            }),
            ret,
        )
    }
}
