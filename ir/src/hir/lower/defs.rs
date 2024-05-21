use super::*;

impl<'b, 'src, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LetAST<'src, A> {}
