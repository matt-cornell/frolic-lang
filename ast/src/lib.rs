pub mod asts;

use frolic_utils::prelude::*;
use smallvec::SmallVec;
use std::borrow::Cow;

pub mod prelude {
    pub mod asts {
        pub use crate::asts::defs::*;
        pub use crate::asts::func::*;
        pub use crate::asts::groups::*;
        pub use crate::asts::lits::*;
        pub use crate::asts::misc::*;
        pub use crate::asts::op::*;
    }
}
