use super::*;

/// A comment AST. This is kept around so that comments are kept in formatted code and to make it
/// easier to debug the IR.
#[derive(Derivative, PartialEq)]
#[derivative(Debug, Clone)]
pub struct CommentAST<'src, S> {
    #[derivative(Debug(format_with = "bstr_debug"))]
    pub comm: Cow<'src, [u8]>,
    pub loc: S,
}
impl<S: Span> Located for CommentAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// AST node representative of an error. Something failed to parse, but we need to have something
/// here.
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorAST<S> {
    pub loc: S,
}
impl<S: Span> Located for ErrorAST<S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// AST node that evaluates to `()`.
#[derive(Debug, Clone, PartialEq)]
pub struct NullAST<S> {
    pub loc: S,
}
impl<S: Span> Located for NullAST<S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

/// Get a variable.
#[derive(Debug, Clone, PartialEq)]
pub struct VarAST<'src, S> {
    pub name: Cow<'src, str>,
    pub global: Option<S>,
    pub loc: S,
}
impl<S: Span> Located for VarAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.global.map_or(self.loc, |loc| loc.merge(self.loc))
    }
}
