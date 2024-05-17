use super::*;
use std::fmt::{self, Debug, Formatter};
use derivative::Derivative;

pub mod defs;
pub mod flow;
pub mod func;
pub mod groups;
pub mod lits;
pub mod misc;
pub mod op;
pub mod types;

fn bstr_debug<S: AsRef<[u8]>>(bytes: &S, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(bstr::BStr::new(bytes), f)
}
