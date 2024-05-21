use super::*;

impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::CommentAST<'src, S> {}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::ErrorAST<S> {}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::NullAST<S> {}
impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::VarAST<'src, S> {}
