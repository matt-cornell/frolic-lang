pub mod asts;
pub mod dottedname;

use frolic_utils::prelude::*;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;

pub mod prelude {
    pub use crate::dottedname::DottedName;
    pub mod asts {
        pub use crate::asts::defs::*;
        pub use crate::asts::flow::*;
        pub use crate::asts::func::*;
        pub use crate::asts::groups::*;
        pub use crate::asts::lits::*;
        pub use crate::asts::misc::*;
        pub use crate::asts::op::*;
    }
}
