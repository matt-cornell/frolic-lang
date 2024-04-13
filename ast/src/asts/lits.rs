use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct IntLitAST<'src, S> {
    pub loc: S,
    pub val: i128,
    pub suf: Option<(Cow<'src, str>, S)>,
}
impl<S: Span> Located for IntLitAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        if let Some(suf) = &self.suf {
            self.loc.merge(suf.1)
        } else {
            self.loc
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLitAST<'src, S> {
    pub loc: S,
    pub val: f64,
    pub suf: Option<(Cow<'src, str>, S)>,
}
impl<S: Span> Located for FloatLitAST<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        if let Some(suf) = &self.suf {
            self.loc.merge(suf.1)
        } else {
            self.loc
        }
    }
}
