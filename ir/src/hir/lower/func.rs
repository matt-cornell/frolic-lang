use super::*;

impl<'src, F, A: ToHir<'src, F>> ToHir<'src, F> for asts::CallAST<A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let (func, ret) = self.func.to_hir(glb, loc);
        if ret {
            return (None, true);
        }
        let (arg, ret) = self.arg.to_hir(glb, loc);
        if ret {
            return (None, true);
        }
        if let (Some(func), Some(arg)) = (func, arg) {
            (
                Some(
                    loc.builder
                        .append(Value::call(&func, &arg, self.loc(), "")),
                ),
                false,
            )
        } else {
            (None, false)
        }
    }
}
impl<'src, F, A: ToHir<'src, F>> ToHir<'src, F> for asts::LambdaAST<'src, A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        let span = self.loc();
        let def = Box::new(Definition::new(None));
        let blk = def.append_new_block();
        let res = loc.push_scope(
            [format!(
                "lambda@{}..{}",
                span.offset(),
                span.offset() + span.len()
            )],
            false,
        );
        let old = loc.builder.get_pos();
        loc.builder.position_at(&blk);
        let (_body, ret) = self.body.to_hir(glb, loc);
        loc.restore_scope(res);
        let def = glb.module.append(def);
        let val = if let Some(old) = old {
            loc.builder.position_at(&old);
            Some(
                loc.builder
                    .append(Value::rglobal(&def, span, "lambda")),
            )
        } else {
            loc.builder.clear_pos();
            None
        };
        (val, ret)
    }
}
