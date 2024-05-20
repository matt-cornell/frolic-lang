use crate::common::list::{LinkedList, LinkedListLink, LinkedListParent, LinkedListElem};
use derivative::Derivative;
use derive_more::*;
use frolic_utils::synccell::SyncCell;
use std::ops::{Deref, DerefMut};
use orx_concurrent_vec::ConcurrentVec as CVec;

fn ptr_opt<T>(val: &Option<&T>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if let Some(ptr) = val {
        write!(f, "{ptr:p}")
    } else {
        f.write_str("null")
    }
}

/// Wrapper around a pointer for better intent and impls of `Debug` and `Eq`
#[repr(transparent)]
#[derive(Derivative)]
#[derivative(Debug(bound=""), Clone(bound = ""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""), Hash(bound=""))]
pub struct Id<'b, T: 'b>(
    #[derivative(Debug(format_with = "std::fmt::Pointer::fmt"), PartialEq(compare_with="std::ptr::eq"), Hash(hash_with="std::ptr::hash"))]
    pub &'b T
);
pub type ModuleId<'b, S> = Id<'b, Module<'b, S>>;
pub type GlobalId<'b, S> = Id<'b, Global<'b, S>>;
pub type DefId<'b, S> = Id<'b, Definition<'b, S>>;
pub type BlockId<'b, S> = Id<'b, Block<'b, S>>;
pub type InstId<'b, S> = Id<'b, Inst<'b, S>>;

pub struct Module<'b, S> {
    pub name: &'b str,
    pub globals: LinkedList<'b, Global<'b, S>>,
}
impl<'b, S> LinkedListParent<'b> for Module<'b, S> {
    type Elem = Global<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<'b, Global<'b, S>> {
        &self.globals
    }
}

/// Something defined at global scope.
#[derive(Debug)]
pub enum Global<'b, S> {
    Namespace(Namespace<'b, S>),
    Definition(Definition<'b, S>),
    Overload(Overload<'b, S>),
}
impl<'b, S> Deref for Global<'b, S> {
    type Target = GlobalCommon<'b, S>;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Namespace(n) => &n,
            Self::Definition(d) => &d,
            Self::Overload(o) => &o,
        }
    }
}
impl<S> DerefMut for Global<'_, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Namespace(n) => &mut **n,
            Self::Definition(d) => &mut **d,
            Self::Overload(o) => &mut **o,
        }
    }
}
impl<'b, S> LinkedListElem<'b> for Global<'b, S> {
    type Parent = Module<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

/// Things common to all global items. Accessible through `Deref`s for `Global` and its variants. 
#[derive(Derivative)]
#[derivative(Debug(bound=""), Clone(bound=""), PartialEq(bound=""))]
pub struct GlobalCommon<'b, S> {
    pub name: Option<&'b str>,
    pub link: LinkedListLink<'b, Global<'b, S>>,
}

#[derive(Derivative, Deref, DerefMut)]
#[derivative(Debug(bound=""), Clone(bound=""), PartialEq(bound=""))]
pub struct Namespace<'b, S> {
    pub common: GlobalCommon<'b, S>,
}

#[derive(Derivative, PartialEq, Deref, DerefMut)]
#[derivative(Debug)]
pub struct Definition<'b, S> {
    #[deref]
    #[deref_mut]
    pub common: GlobalCommon<'b, S>,
    #[derivative(Debug(format_with = "ptr_opt"))]
    pub captures: Option<&'b Definition<'b, S>>,
    pub is_func: bool,
    pub blocks: LinkedList<'b, Block<'b, S>>,
}
impl<'b, S> LinkedListParent<'b> for Definition<'b, S> {
    type Elem = Block<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<Block<'b, S>> {
        &self.blocks
    }
}

#[derive(Debug, Deref, DerefMut)]
pub struct Overload<'b, S> {
    #[deref]
    #[deref_mut]
    pub common: GlobalCommon<'b, S>,
    pub variants: CVec<DefId<'b, S>>, 
}
impl<S> PartialEq for Overload<'_, S> {
    fn eq(&self, other: &Self) -> bool {
        self.common == other.common && self.variants.iter().eq(other.variants.iter())
    }
}

#[derive(Debug, PartialEq)]
pub struct Block<'b, S> {
    pub name: &'b str,
    pub term: SyncCell<Terminator<'b, S>>,
    pub insts: LinkedList<'b, Inst<'b, S>>,
    pub link: LinkedListLink<'b, Self>,
}
impl<'b, S> LinkedListParent<'b> for Block<'b, S> {
    type Elem = Inst<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<'b, Inst<'b, S>> {
        &self.insts
    }
}
impl<'b, S> LinkedListElem<'b> for Block<'b, S> {
    type Parent = Definition<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Inst<'b, S> {
    pub name: &'b str,
    pub kind: InstKind<'b, S>,
    pub span: S,
    pub link: LinkedListLink<'b, Self>,
}
impl<'b, S> LinkedListElem<'b> for Inst<'b, S> {
    type Parent = Block<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constant<'b> {
    Unknown,
    Error,
    Null,
    Int(i64),
    Float(f64),
    String(&'b [u8])
}

#[derive(Derivative, Default)]
#[derivative(Debug(bound=""), Clone(bound=""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""), Hash(bound=""))]
pub enum Terminator<'b, S> {
    #[default]
    Unreachable,
    Return(Operand<'b, S>),
    CondBr {
        cond: Operand<'b, S>,
        if_true: BlockId<'b, S>,
        if_false: BlockId<'b, S>,
    },
    UncondBr { blk: BlockId<'b, S> },
}

#[derive(Derivative)]
#[derivative(Debug(bound=""), Clone(bound=""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""), Hash(bound=""))]
pub enum Operand<'b, S> {
    Const(Constant<'b>),
    Inst(InstId<'b, S>),
    Global(GlobalId<'b, S>),
}

/// A kind of instruction.
#[derive(Derivative)]
#[derivative(Debug(bound=""), Clone(bound=""), Copy(bound=""), PartialEq(bound=""), Eq(bound=""), Hash(bound=""))]
pub enum InstKind<'b, S> {
    /// Call `func` with `arg`.
    Call {
        func: Operand<'b, S>,
        arg: Operand<'b, S>
    },
    /// Transparent, but gives a new name and span.
    Bind(Operand<'b, S>),
    /// Cast a value to a given type.
    Cast {
        val: Operand<'b, S>,
        ty: Operand<'b, S>,
    },
    /// Assert that a value has a given type.
    Ascribe {
        val: Operand<'b, S>,
        ty: Operand<'b, S>,
    },
    /// Get value based on the predecessor.
    Phi(&'b [(BlockId<'b, S>, Operand<'b, S>)]>),
}
