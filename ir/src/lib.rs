#![feature(array_windows)]
use atomic_ref::AtomicRef;
use std::sync::atomic::Ordering;
use std::fmt::{self, Debug, Formatter, Write};

pub mod common;
pub mod hir;
pub mod mir;

fn fmt_ref<T>(r: &AtomicRef<T>, f: &mut Formatter) -> fmt::Result {
    if let Some(ptr) = r.load(Ordering::Relaxed) {
        write!(f, "{ptr:p}")
    } else {
        f.write_str("null")
    }
}

pub mod prelude {
    pub use crate::hir::lang::Module as HirModule;
    #[cfg(feature = "rayon")]
    pub use crate::hir::lower::multi_threaded;
    pub use crate::hir::lower::single_threaded;
    pub use crate::hir::lower::{
        alloc_from_bump, lower_to_hir, lower_to_ret_module, BumpAlloc, ToHir,
    };
}
