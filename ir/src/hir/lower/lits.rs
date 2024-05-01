use super::*;


impl<'src, S: Span> ToHir<'src> for asts::IntLitAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::int(self.val, self.loc, "")))), false)
    }
}
impl<'src, S: Span> ToHir<'src> for asts::FloatLitAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::float(self.val as _, self.loc, "")))), false)
    }
}
impl<'src, S: Span> ToHir<'src> for asts::CharLitAST<S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::int(self.val as _, self.loc, "")))), false)
    }
}
impl<'src, S: Span> ToHir<'src> for asts::StringLitAST<'src, S> {
    fn to_hir(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        (Some(loc.builder.append(Box::new(Value::string(self.val.clone(), self.loc, "")))), false)
    }
}
