use super::*;

impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::IntLitAST<S> {}
impl<'b, F: Clone, S: Span> ToHir<'b, F> for asts::FloatLitAST<S> {}
impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::StringLitAST<'src, S> {}
