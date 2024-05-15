pub mod common;
pub mod hir;
pub mod mir;

pub mod prelude {
    pub use crate::hir::lang::Module as HirModule;
    pub use crate::hir::lower::{ToHir, lower_to_hir, HirError};
}
