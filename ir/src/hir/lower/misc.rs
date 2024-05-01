use super::*;

impl<'src, S: Span> ToHir<'src> for asts::CommentAST<'src, S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        loc.builder.append(Box::new(Value::comment(self.comm.clone(), self.loc, "")));
        (None, false)
    }
}

impl<'src, S: Span> ToHir<'src> for asts::ErrorAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::error(self.loc, "")))), false)
    }
}

impl<'src, S: Span> ToHir<'src> for asts::NullAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::null(self.loc, "")))), false)
    }
}

impl<'src, S: Span> ToHir<'src> for asts::VarAST<'src, S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        _loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        // TODO
        (None, false)
    }
}
