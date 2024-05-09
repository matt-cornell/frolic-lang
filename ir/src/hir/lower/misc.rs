use super::*;

impl<'src, F, S: Span> ToHir<'src, F> for asts::CommentAST<'src, S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        loc.builder
            .append(Value::comment(self.comm.clone(), self.loc, ""));
        (None, false)
    }
}

impl<'src, F, S: Span> ToHir<'src, F> for asts::ErrorAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (
            Some(loc.builder.append(Value::error(self.loc, ""))),
            false,
        )
    }
}

impl<'src, F, S: Span> ToHir<'src, F> for asts::NullAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (
            Some(loc.builder.append(Value::null(self.loc, ""))),
            false,
        )
    }
}

impl<'src, F, S: Span> ToHir<'src, F> for asts::VarAST<'src, S> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        if self.global.is_none() {
            if let Some(sym) = loc.symbols.get(&*self.name) {
                return (Some(sym.value.clone()), false);
            }
        }
        if loc.global_scope.get(&*self.name).is_some() {
            return (
                Some(loc.builder.append(Value::uglobal(
                    format!(".{}", self.name).into(),
                    self.loc,
                    self.name.clone().into_owned(),
                ))),
                false,
            );
        }
        (
            Some(loc.builder.append(Value::error(
                self.loc,
                self.name.clone().into_owned(),
            ))),
            (glb.report)(HirError::UnresolvedVariable {
                name: self.name.clone(),
                span: self.loc,
            }),
        )
    }
}
