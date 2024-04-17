use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FrolicAST<A, F: Copy> {
    pub file: F,
    pub nodes: Vec<A>,
}
