use super::*;
use crate::dottedname::DottedName;
use std::fmt::{self, Debug, Formatter};
use derivative::Derivative;

pub mod defs;
pub mod flow;
pub mod func;
pub mod groups;
pub mod lits;
pub mod misc;
pub mod op;
pub mod scope;
pub mod types;

fn bstr_debug<S: AsRef<[u8]>>(bytes: &S, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(bstr::BStr::new(bytes), f)
}

/// The top-level AST.
#[derive(Debug, Clone, PartialEq)]
pub struct FrolicAST<'src, A: Located, F> {
    pub file: F,
    pub name: Option<DottedName<'src, A::Span>>,
    pub nodes: Vec<A>,
}
/// Note that this requires `SpanConstruct`, we need to create our own span if there are no nodes.
impl<S: SpanConstruct, A: Located<Span = S>, F> Located for FrolicAST<'_, A, F> {
    type Span = A::Span;

    fn loc(&self) -> Self::Span {
        match &self.nodes[..] {
            [] => S::new(0, 0),
            [n] => n.loc(),
            _ => self.nodes[0].loc().merge(self.nodes.last().unwrap().loc()),
        }
    }
}
