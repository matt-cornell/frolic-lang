use super::*;

#[derive(Derivative, Clone, PartialEq)]
#[derivative(Debug)]
pub struct NamespaceAST<'src, A: Located> {
    /// The span covering the keyword.
    pub kw: A::Span,
    /// The span covering the body of this namespace.
    pub body: A::Span,
    /// Documentation for this namespace.
    #[derivative(Debug(format_with = "bstr_debug"))] 
    pub doc: Cow<'src, [u8]>,
    /// The name of this namespace.
    pub name: DottedName<'src, A::Span>,
    /// Defintions in this namespace.
    pub nodes: Vec<A>
}

impl<A: Located> Located for NamespaceAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.kw.merge(self.body)
    }
}
