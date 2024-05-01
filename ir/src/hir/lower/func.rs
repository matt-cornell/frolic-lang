use super::*;

impl<'src, A: ToHir<'src>> ToHir<'src> for asts::CallAST<A> {
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span>,
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
                        .append(Box::new(Value::call(&func, &arg, self.loc(), ""))),
                ),
                false,
            )
        } else {
            (None, false)
        }
    }
}
