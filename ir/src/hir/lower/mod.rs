use super::lang::*;
use frolic_utils::prelude::*;

mod error;
pub use error::HirError;

pub struct GlobalContext<'a, 'src, S: Span> {
    pub module: Module<'src, S>,
    #[cfg(feature = "rayon")]
    pub report: &'a (dyn Fn(HirError<'src, S>) -> bool + Sync),
    #[cfg(not(feature = "rayon"))]
    pub report: &'a dyn Fn(HirError<'src, S>) -> bool
}

pub struct LocalContext<'src, S> {
    pub insert: Option<(GlobalId<'src, S>, Block<'src, S>)>,
}

pub trait ToHir<'src>: Located {
    fn predef_global(&self, _glb: &GlobalContext<'_, 'src, Self::Span>) -> bool { false }
    fn to_hir(&self, glb: &GlobalContext<'_, 'src, Self::Span>, loc: &mut LocalContext<'src, Self::Span>) -> bool;
}
