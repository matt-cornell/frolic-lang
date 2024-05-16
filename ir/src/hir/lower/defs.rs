use super::*;

// TODO: implement
impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::LetAST<'src, A> {}
