use super::*;

impl<'b, 'src, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LambdaAST<'src, A> {}
impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::CallAST<A> {}
