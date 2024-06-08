use super::*;
use strum::*;

/// Intrinsics for values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Intrinsic {
    Type,
    NamespaceType,
}

pub mod list;
pub mod symbols;
