#![feature(ptr_metadata)]

pub mod common;
pub mod hir;
pub mod mir;

#[doc(inline)]
pub use prelude::*;

pub mod prelude {
    pub use crate::hir::lower::ToHir;
    pub use crate::hir::Builder as HirBuilder;
    pub use crate::hir::Definition as HirDefinition;
    pub use crate::hir::Module as HirModule;
    pub use crate::hir::Value as HirValue;
}
