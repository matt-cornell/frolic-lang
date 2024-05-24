#![feature(array_windows)]

pub mod common;
pub mod hir;
pub mod mir;

pub mod prelude {
    pub use crate::hir::lang::Module as HirModule;
    #[cfg(feature = "rayon")]
    pub use crate::hir::lower::multi_threaded;
    pub use crate::hir::lower::single_threaded;
    pub use crate::hir::lower::{
        alloc_from_bump, lower_to_hir, lower_to_ret_module, BumpAlloc, ToHir,
    };
}
