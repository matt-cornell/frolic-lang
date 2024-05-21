use super::*;

impl<'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::ParenAST<A> {}
