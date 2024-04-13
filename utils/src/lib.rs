pub mod dottedname;
pub mod span;

pub mod prelude {
    pub use crate::dottedname::DottedName;
    pub use crate::span::{Span, Located};
}
