use crate::common::{disp, lang};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::fmt::{self, Formatter};

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
    Cast {
        val: Operand<'src, S>,
        ty: Operand<'src, S>,
    },
    Ascribe {
        val: Operand<'src, S>,
        ty: Operand<'src, S>,
    },
    Bind(Operand<'src, S>),
    Phi(SmallVec<[(BlockId<'src, S>, Operand<'src, S>); 2]>),
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

impl<'src, S> lang::IsReturn<'src, S, Hir> for Terminator<'src, S> {
    #[inline]
    fn is_return(&self) -> Option<Operand<'src, S>> {
        if let Self::Return(op) = self {
            Some(op.clone())
        } else {
            None
        }
    }
}

impl<'a, 'src, S> disp::DispWithContext<(&'a Module<'src, S>, &'a Global<'src, S>)>
    for InstKind<'src, S>
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S>, &'a Global<'src, S>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Call { func, arg } => write!(
                f,
                "call {} {}",
                disp::WithContext {
                    value: func,
                    context,
                },
                disp::WithContext {
                    value: arg,
                    context,
                }
            ),
            Self::ArgOf { func } => write!(
                f,
                "argof {}",
                disp::WithContext {
                    value: func,
                    context: context.0
                }
            ),
            Self::FunctionTy { arg, ret } => write!(
                f,
                "fn {} -> {}",
                disp::WithContext {
                    value: arg,
                    context
                },
                disp::WithContext {
                    value: ret,
                    context
                }
            ),
            Self::Cast { val, ty } => write!(
                f,
                "cast {} to {}",
                disp::WithContext {
                    value: val,
                    context,
                },
                disp::WithContext {
                    value: ty,
                    context,
                }
            ),
            Self::Ascribe { val, ty } => write!(
                f,
                "asc {} to {}",
                disp::WithContext {
                    value: val,
                    context,
                },
                disp::WithContext {
                    value: ty,
                    context,
                }
            ),
            Self::Bind(op) => write!(
                f,
                "bind {}",
                disp::WithContext {
                    value: op,
                    context
                }
            ),
            Self::Phi(args) => {
                f.write_str("phi")?;
                args.iter()
                    .try_fold(true, |first, (blk, val)| {
                        if !first {
                            f.write_str(",")?;
                        }
                        write!(
                            f,
                            " {} => {}",
                            disp::WithContext {
                                value: blk,
                                context
                            },
                            disp::WithContext {
                                value: val,
                                context
                            }
                        ).map(|_| false)
                    })
                    .map(|_| ())
            }
        }
    }
}

impl<'a, 'src, S> disp::DispWithContext<(&'a Module<'src, S>, &'a Global<'src, S>)>
    for Terminator<'src, S>
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S>, &'a Global<'src, S>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Unreachable => f.write_str("unreachable"),
            Self::Return(ret) => write!(f, "return {}", disp::WithContext { value: ret, context }),
            Self::UncondBr(blk) => write!(f, "goto {}", disp::WithContext { value: blk, context }),
            Self::CondBr { cond, if_true, if_false } => write!(f, "if {} then {} else {}",
                disp::WithContext { value: cond, context },
                disp::WithContext { value: if_true, context },
                disp::WithContext { value: if_false, context },
            ),
        }
    }
}

impl<'src, T: Copy> disp::DispWithContext<T> for Constant<'src> {
    fn fmt(&self, _context: T, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => f.write_str("error"),
            Self::Null => f.write_str("null"),
            Self::Int(v) => write!(f, "int {v}"),
            Self::Float(v) => write!(f, "float {v}"),
            Self::String(v) => write!(f, "str {:?}", bstr::BStr::new(v)),
        }
    }
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
