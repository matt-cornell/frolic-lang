use super::*;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::AscribeAST<A> {}
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::CastAST<A> {}
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::ExtendAST<A> {}
