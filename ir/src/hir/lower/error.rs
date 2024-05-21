use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum HirError<S> {
    Phantom(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Error)]
#[error("Error reporter requested early termination")]
pub struct EarlyReturn;

pub type LowerResult = Result<(), EarlyReturn>;
