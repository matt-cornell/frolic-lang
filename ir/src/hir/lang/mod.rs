use super::*;
use crate::common::list::{LinkedList, LinkedListElem, LinkedListLink, LinkedListParent};
use crate::common::Intrinsic;
use bump_scope::NoDrop;
use derivative::Derivative;
use derive_more::From;
use frolic_utils::synccell::SyncCell;

mod disp;

fn debug_bstr<T: AsRef<[u8]>>(val: &T, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(bstr::BStr::new(val), f)
}

/// Wrapper around a pointer for better intent and impls of `Debug` and `Eq`
#[repr(transparent)]
#[derive(Derivative, From)]
#[derivative(
    Debug(bound = ""),
    PartialEq(bound = ""),
    Eq(bound = ""),
    Hash(bound = "")
)]
pub struct Id<'b, T>(
    #[derivative(
        Debug(format_with = "std::fmt::Pointer::fmt"),
        PartialEq(compare_with = "std::ptr::eq"),
        Hash(hash_with = "std::ptr::hash")
    )]
    pub &'b T,
);
impl<T> Clone for Id<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Id<'_, T> {}
pub type ModuleId<'b, S> = Id<'b, Module<'b, S>>;
pub type GlobalId<'b, S> = Id<'b, Global<'b, S>>;
pub type BlockId<'b, S> = Id<'b, Block<'b, S>>;
pub type InstId<'b, S> = Id<'b, Inst<'b, S>>;

#[derive(Debug)]
pub struct Module<'b, S> {
    pub name: &'b str,
    pub globals: LinkedList<'b, Global<'b, S>>,
}
impl<'b, S> Module<'b, S> {
    pub fn new(name: &'b str) -> Self {
        Self {
            name,
            globals: LinkedList::default(),
        }
    }
}
impl<'b, S> NoDrop for Module<'b, S> {}
impl<'b, S> LinkedListParent<'b> for Module<'b, S> {
    type Elem = Global<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<'b, Global<'b, S>> {
        &self.globals
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum GlobalKind<'b, S> {
    Local {
        ty: Operand<'b, S>,
        captures: GlobalId<'b, S>,
    },
    Global(#[derivative(Debug(format_with = "fmt_ref"))] AtomicRef<'b, Global<'b, S>>),
    Intrinsic(Intrinsic),
}
impl<'b, S> GlobalKind<'b, S> {
    pub const NAMESPACE: Self = Self::Intrinsic(Intrinsic::NamespaceType);
    pub fn get_ty(&self) -> Operand<'b, S> {
        match self {
            Self::Global(r) => {
                if let Some(ty) = r.load(Ordering::Relaxed) {
                    Operand::Global(Id(ty))
                } else {
                    Operand::Const(Constant::Unknown)
                }
            }
            &Self::Local { ty, .. } => ty,
            &Self::Intrinsic(i) => Operand::Const(Constant::Intrinsic(i)),
        }
    }
}
impl<S> NoDrop for GlobalKind<'_, S> {}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Global<'b, S> {
    pub name: &'b str,
    pub is_func: bool,
    #[derivative(Debug(format_with = "debug_bstr"))]
    pub docs: &'b [u8],
    pub span: S,
    pub kind: GlobalKind<'b, S>,
    pub blocks: LinkedList<'b, Block<'b, S>>,
    pub link: LinkedListLink<'b, Self>,
}
impl<'b, S> Global<'b, S> {
    /// For the case of a global that only returns a value, use this shortcut.
    pub fn as_alias(&self) -> Option<Operand<'b, S>> {
        let mut it = self.blocks.iter();
        let Block { insts, term, .. } = it.next()?;
        it.next().is_none().then_some(())?;
        insts.iter().next().is_none().then_some(())?;
        if let Terminator::Return(op) = term.get() {
            Some(op)
        } else {
            None
        }
    }
}
impl<'b, S> NoDrop for Global<'b, S> {}
impl<'b, S> LinkedListParent<'b> for Global<'b, S> {
    type Elem = Block<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<Block<'b, S>> {
        &self.blocks
    }
}
impl<'b, S> LinkedListElem<'b> for Global<'b, S> {
    type Parent = Module<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

#[derive(Debug, PartialEq)]
pub struct Block<'b, S> {
    pub name: &'b str,
    pub term: SyncCell<Terminator<'b, S>>,
    pub insts: LinkedList<'b, Inst<'b, S>>,
    pub link: LinkedListLink<'b, Self>,
}
impl<'b, S> Block<'b, S> {
    pub fn new(name: &'b str) -> Self {
        Self {
            name,
            term: SyncCell::new(Terminator::Unreachable),
            insts: LinkedList::NEW,
            link: LinkedListLink::NEW,
        }
    }
    pub fn returning(name: &'b str, ret: Operand<'b, S>) -> Self {
        Self {
            name,
            term: SyncCell::new(Terminator::Return(ret)),
            insts: LinkedList::NEW,
            link: LinkedListLink::NEW,
        }
    }
}
impl<'b, S> NoDrop for Block<'b, S> {}
impl<'b, S> LinkedListParent<'b> for Block<'b, S> {
    type Elem = Inst<'b, S>;

    fn get_list(&'b self) -> &'b LinkedList<'b, Inst<'b, S>> {
        &self.insts
    }
}
impl<'b, S> LinkedListElem<'b> for Block<'b, S> {
    type Parent = Global<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Inst<'b, S> {
    pub name: &'b str,
    pub kind: InstKind<'b, S>,
    pub span: S,
    pub link: LinkedListLink<'b, Self>,
}
impl<'b, S: NoDrop> NoDrop for Inst<'b, S> {}
impl<'b, S> LinkedListElem<'b> for Inst<'b, S> {
    type Parent = Block<'b, S>;

    fn get_link(&'b self) -> &'b LinkedListLink<'b, Self> {
        &self.link
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constant<'b> {
    Unknown,
    Error,
    Null,
    Intrinsic(Intrinsic),
    Int(i64),
    Float(f64),
    String(&'b [u8]),
    Namespace(&'b str),
}

#[derive(Derivative, Default)]
#[derivative(Debug(bound = ""), PartialEq(bound = ""))]
pub enum Terminator<'b, S> {
    #[default]
    Unreachable,
    Return(Operand<'b, S>),
    CondBr {
        cond: Operand<'b, S>,
        if_true: BlockId<'b, S>,
        if_false: BlockId<'b, S>,
    },
    UncondBr {
        blk: BlockId<'b, S>,
    },
}
impl<S> Clone for Terminator<'_, S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S> Copy for Terminator<'_, S> {}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), PartialEq(bound = ""))]
pub enum Operand<'b, S> {
    Const(Constant<'b>),
    Inst(InstId<'b, S>),
    Global(GlobalId<'b, S>),
}
impl<S> Clone for Operand<'_, S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S> Copy for Operand<'_, S> {}

/// A kind of instruction.
#[derive(Derivative)]
#[derivative(Debug(bound = ""), PartialEq(bound = ""))]
pub enum InstKind<'b, S> {
    /// Call `func` with `arg`.
    Call {
        func: Operand<'b, S>,
        arg: Operand<'b, S>,
    },
    /// Create an arrow.
    FnType {
        arg: Operand<'b, S>,
        ret: Operand<'b, S>,
    },
    /// Transparent, but gives a new name and span.
    Bind(Operand<'b, S>),
    /// Argument of the current function
    Arg,
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
    /// A type with a name. Also a base type for it to act like.
    NamedTy {
        name: &'b str,
        base: Operand<'b, S>,
        decays: bool,
    },
    /// Get value based on the predecessor.
    Phi(&'b [(BlockId<'b, S>, Operand<'b, S>)]),
}
impl<S> Clone for InstKind<'_, S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S> Copy for InstKind<'_, S> {}
