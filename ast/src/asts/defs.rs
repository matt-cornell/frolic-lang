use super::*;
use crate::dottedname::DottedName;

/// A function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct FnParam<'src, A: Located> {
    pub name: Cow<'src, str>,
    pub loc: A::Span,
    pub ty: Option<A>,
}

/// Either a variable or function definition. A variable is just a nullary function at this point.
/// Unlike `LetOpAST`, this merely binds a value to a name, and doesn't do any control flow.
#[derive(Derivative, Clone, PartialEq)]
#[derivative(Debug)]
pub struct LetAST<'src, A: Located> {
    pub kw: A::Span,
    #[derivative(Debug(format_with = "bstr_debug"))]
    pub doc: Cow<'src, [u8]>,
    pub name: DottedName<'src, A::Span>,
    pub params: SmallVec<[FnParam<'src, A>; 2]>,
    pub ret: Option<A>,
    pub body: A,
}
impl<A: Located> Located for LetAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.kw.merge(self.body.loc())
    }
}

/// Here, `let` is being used as a control flow operator. Because of this, it needs to know the
/// continuation.
#[derive(Debug, Clone, PartialEq)]
pub struct LetOpAST<'src, A: Located> {
    /// Location of the keyword.
    pub kw: A::Span,
    /// The name of the operator. Should always start with `let`.
    pub op: Cow<'src, str>,
    /// Name of the variable.
    pub name: Cow<'src, str>,
    /// Location of the name.
    pub nloc: A::Span,
    /// Value of this binding, goes on the RHS of the `=`.
    pub body: A,
    /// Continuation of this binding, what would come after the `;`.
    pub cont: A,
}
impl<A: Located> Located for LetOpAST<'_, A> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        self.kw.merge(self.body.loc())
    }
}
