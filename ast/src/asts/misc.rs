use super::*;
use std::fmt::{self, Debug, Formatter};

#[derive(Clone, PartialEq)]
pub struct CommentAST<'src, S> {
    pub comm: Cow<'src, [u8]>,
    pub loc: S,
}
impl<S: Debug> Debug for CommentAST<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("CommentAST")
            .field("comm", &bstr::BStr::new(&self.comm))
            .field("loc", &self.loc)
            .finish()
    }
}
impl<S: Span> Located for CommentAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.loc
    }
}

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
