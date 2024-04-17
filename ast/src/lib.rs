#![feature(unsize, coerce_unsized)]

pub mod asts;
pub mod traits;

use frolic_utils::prelude::*;
use smallvec::SmallVec;
use std::borrow::Cow;
use traits::*;

pub mod prelude {
    pub use crate::traits::*;
    pub mod asts {
        pub use crate::asts::defs::*;
        pub use crate::asts::groups::*;
        pub use crate::asts::lits::*;
    }
}
