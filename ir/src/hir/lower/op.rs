use super::*;

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::ShortCircuitAST<A> {}
impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::FunctionTypeAST<A> {}
