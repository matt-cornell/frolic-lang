use crate::common::lang;
use std::borrow::Cow;
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq)]
pub enum InstKind<'src, S> {
    Call {
        func: Operand<'src, S>,
        arg: Operand<'src, S>,
    },
    ArgOf {
        func: GlobalId<'src, S>,
    },
    FunctionTy {
        arg: Operand<'src, S>,
        ret: Operand<'src, S>,
    },
    Bind(Operand<'src, S>),
    Phi(SmallVec<[(BlockId<'src, S>, Operand<'src, S>); 2]>)
}

#[derive(Debug, Default)]
pub enum Terminator<'src, S> {
    #[default]
    Unreachable,
    Return(Operand<'src, S>),
    UncondBr(BlockId<'src, S>),
    CondBr {
        cond: Operand<'src, S>,
        if_true: BlockId<'src, S>,
        if_false: BlockId<'src, S>,
    },
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Constant<'src> {
    #[default]
    Error,
    Null,
    Int(i64),
    Float(f64),
    String(Cow<'src, [u8]>),
}

pub struct Hir;
impl<'src, S> lang::Language<'src, S> for Hir {
    type Constant = Constant<'src>;
    type InstKind = InstKind<'src, S>;
    type Terminator = Terminator<'src, S>;
}

pub type GlobalId<'src, S> = lang::GlobalId<'src, S, Hir>;
pub type BlockId<'src, S> = lang::BlockId<'src, S, Hir>;
pub type InstId<'src, S> = lang::InstId<'src, S, Hir>;
pub type ModuleId<'src, S> = lang::ModuleId<'src, S, Hir>;
pub type UniversalGlobalId<'src, S, F> = lang::UniversalGlobalId<'src, S, F, Hir>;

pub type Module<'src, S> = lang::Module<'src, S, Hir>;
pub type Global<'src, S> = lang::Global<'src, S, Hir>;
pub type Block<'src, S> = lang::Block<'src, S, Hir>;
pub type Instruction<'src, S> = lang::Instruction<'src, S, Hir>;
pub type Operand<'src, S> = lang::Operand<'src, S, Hir>;
