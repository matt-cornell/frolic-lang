use super::*;
use strum::*;

/// Intrinsics for values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum Intrinsic {
    Type,
    NamespaceType,
}

pub mod list;
pub mod symbols;
