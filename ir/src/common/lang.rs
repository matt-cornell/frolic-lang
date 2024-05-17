use derivative::Derivative;
use frolic_utils::synccell::SyncCell;
use orx_concurrent_vec::ConcurrentVec as ConVec;
use std::borrow::Cow;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Index;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::fmt::{self, Debug, Formatter};

fn bstr_debug<S: AsRef<[u8]>>(bytes: &S, f: &mut Formatter<'_>) -> fmt::Result {
    Debug::fmt(bstr::BStr::new(bytes), f)
}

pub mod markers {
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Global;
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Block;
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Inst;
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Module;
}

pub trait Language<'src, S>: Sized {
    type Constant;
    type InstKind;
    type Terminator: IsReturn<'src, S, Self>;
}

pub trait IsReturn<'src, S, L: Language<'src, S>> {
    fn is_return(&self) -> Option<Operand<'src, S, L>> {
        None
    }
}

pub type GlobalId<'src, S, L> = Id<PhantomData<(&'src L, markers::Global, S)>>;
pub type BlockId<'src, S, L> = Id<PhantomData<(&'src L, markers::Block, S)>>;
pub type InstId<'src, S, L> = Id<PhantomData<(&'src L, markers::Inst, S)>>;
pub type ModuleId<'src, S, L> = Id<PhantomData<(&'src L, markers::Module, S)>>;

#[derive(Derivative)]
#[derivative(
    Debug(bound = "F: Debug, S: Debug"),
    Clone(bound = "F: Clone, S: Clone"),
    Copy(bound = "F: Copy, S: Copy"),
    PartialEq(bound = "F: PartialEq, S: PartialEq"),
    Eq(bound = "F: PartialEq, S: PartialEq"),
    Hash(bound = "F: Hash, S: Hash")
)]
pub struct UniversalGlobalId<'src, S, F, L> {
    pub id: GlobalId<'src, S, L>,
    pub module: ModuleId<'src, S, L>,
    pub file: F,
    pub span: S,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id<M> {
    pub index: usize,
    pub marker: M,
}
impl<M> Id<M> {
    pub const fn with_marker(index: usize, marker: M) -> Self {
        Self { index, marker }
    }
}
impl<M: Default> Id<M> {
    pub fn invalid() -> Self {
        Self::new(usize::MAX)
    }
    pub fn new(index: usize) -> Self {
        Self {
            index,
            marker: M::default(),
        }
    }
}

static MODULE_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Module<'src, S, L: Language<'src, S>> {
    pub name: String,
    id: usize,
    #[derivative(Debug = "ignore")]
    pub(crate) glbs: ConVec<Global<'src, S, L>>,
    #[derivative(Debug = "ignore")]
    insts: ConVec<Instruction<'src, S, L>>,
}
impl<'src, S, L: Language<'src, S>> Module<'src, S, L> {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            id: MODULE_ID.fetch_add(1, Ordering::Relaxed),
            glbs: ConVec::new(),
            insts: ConVec::new(),
        }
    }
    /// Returns a unique `usize` identifying this module.
    pub fn id(&self) -> ModuleId<'src, S, L> {
        ModuleId::new(self.id)
    }
    pub fn push_global(&self, glb: Global<'src, S, L>) -> GlobalId<'src, S, L> {
        GlobalId::new(self.glbs.push(glb))
    }
    pub fn push_new_global(&self) -> GlobalId<'src, S, L>
    where
        L: Default,
    {
        self.push_global(Default::default())
    }
    pub fn intern_inst(&self, inst: Instruction<'src, S, L>) -> InstId<'src, S, L> {
        InstId::new(self.insts.push(inst))
    }
    pub fn get_global(&self, id: GlobalId<'src, S, L>) -> Option<&Global<'src, S, L>> {
        self.glbs.get(id.index)
    }
    pub fn get_inst(&self, id: InstId<'src, S, L>) -> Option<&Instruction<'src, S, L>> {
        self.insts.get(id.index)
    }
    pub fn globals(&self) -> impl Iterator<Item = &Global<'src, S, L>> {
        self.glbs.iter()
    }
}
impl<'src, S, L: Language<'src, S>> Default for Module<'src, S, L> {
    fn default() -> Self {
        Self::new(String::new())
    }
}
impl<'src, S, L: Language<'src, S>> Index<GlobalId<'src, S, L>> for Module<'src, S, L> {
    type Output = Global<'src, S, L>;

    fn index(&self, index: GlobalId<'src, S, L>) -> &Self::Output {
        self.glbs.get(index.index).unwrap()
    }
}
impl<'src, S, L: Language<'src, S>> Index<InstId<'src, S, L>> for Module<'src, S, L> {
    type Output = Instruction<'src, S, L>;

    fn index(&self, index: InstId<'src, S, L>) -> &Self::Output {
        self.insts.get(index.index).unwrap()
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "L::Constant: Debug"),
    Clone(bound = "L::Constant: Clone"),
    PartialEq(bound = "L::Constant: PartialEq")
)]
pub enum Operand<'src, S, L: Language<'src, S>> {
    Constant(L::Constant),
    Global(GlobalId<'src, S, L>),
    Instruction(InstId<'src, S, L>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""))]
pub struct Global<'src, S, L: Language<'src, S>> {
    pub name: String,
    #[derivative(Debug(format_with = "bstr_debug"))]
    pub doc: Cow<'src, [u8]>,
    #[derivative(Debug = "ignore")]
    pub blocks: ConVec<Block<'src, S, L>>,
}
impl<'src, S, L: Language<'src, S>> Global<'src, S, L> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            doc: b"".into(),
            blocks: ConVec::new(),
        }
    }
    pub fn with_doc(name: String, doc: impl Into<Cow<'src, [u8]>>) -> Self {
        Self {
            name,
            doc: doc.into(),
            blocks: ConVec::new(),
        }
    }
    pub fn push_blk(&self, block: Block<'src, S, L>) -> BlockId<'src, S, L> {
        BlockId::new(self.blocks.push(block))
    }
    pub fn get_blk(&self, id: BlockId<'src, S, L>) -> Option<&Block<'src, S, L>> {
        self.blocks.get(id.index)
    }
    pub fn blocks(&self) -> impl Iterator<Item = &Block<'src, S, L>> {
        self.blocks.iter()
    }
    pub fn as_alias(&self) -> Option<Operand<'src, S, L>> {
        (self.blocks.len() == 1).then(|| {
            let b = self.blocks.get(0).unwrap();
            (b.insts.is_empty()).then(|| b.term.with(|t| t.is_return())).flatten()
        }).flatten()
    }
}
impl<'src, S, L: Language<'src, S>> Index<BlockId<'src, S, L>> for Global<'src, S, L> {
    type Output = Block<'src, S, L>;

    fn index(&self, index: BlockId<'src, S, L>) -> &Self::Output {
        self.get_blk(index).unwrap()
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "L::Terminator: Debug"),
    Default(bound = "L::Terminator: Default")
)]
pub struct Block<'src, S, L: Language<'src, S>> {
    pub name: Cow<'src, str>,
    pub insts: Vec<InstId<'src, S, L>>,
    pub term: SyncCell<L::Terminator>,
}
impl<'src, S, L: Language<'src, S>> Block<'src, S, L> {
    pub fn new(name: impl Into<Cow<'src, str>>) -> Self
    where
        L::Terminator: Default,
    {
        Self {
            name: name.into(),
            insts: vec![],
            term: Default::default(),
        }
    }
    pub fn with_term(name: impl Into<Cow<'src, str>>, term: L::Terminator) -> Self {
        Self {
            name: name.into(),
            insts: vec![],
            term: SyncCell::new(term),
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "S: Debug, L::InstKind: Debug"),
    Clone(bound = "S: Clone, L::InstKind: Clone"),
    PartialEq(bound = "S: PartialEq, L::InstKind: PartialEq")
)]
pub struct Instruction<'src, S, L: Language<'src, S>> {
    pub name: Cow<'src, str>,
    pub span: S,
    pub kind: L::InstKind,
}
