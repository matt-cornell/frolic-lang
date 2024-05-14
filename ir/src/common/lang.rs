use derivative::Derivative;
use orx_concurrent_vec::ConcurrentVec as ConVec;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::Index;

pub mod markers {
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Global;
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Block;
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Inst;
}

pub trait Language<'src, S> {
    type Constant;
    type InstKind;
    type Terminator;
}

pub type GlobalId<'src, S, L> = Id<PhantomData<(&'src L, markers::Global, S)>>;
pub type BlockId<'src, S, L> = Id<PhantomData<(&'src L, markers::Block, S)>>;
pub type InstId<'src, S, L> = Id<PhantomData<(&'src L, markers::Inst, S)>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id<M> {
    pub index: usize,
    pub marker: M,
}
impl<M> Id<M> {
    pub fn with_marker(index: usize, marker: M) -> Self {
        Self { index, marker }
    }
}
impl<M: Default> Id<M> {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            marker: M::default(),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug, Default(bound = ""))]
pub struct Module<'src, S, L: Language<'src, S>> {
    pub name: String,
    #[derivative(Debug = "ignore")]
    glbs: ConVec<Global<'src, S, L>>,
    #[derivative(Debug = "ignore")]
    insts: ConVec<Instruction<'src, S, L>>,
}
impl<'src, S, L: Language<'src, S>> Module<'src, S, L> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            glbs: ConVec::new(),
            insts: ConVec::new(),
        }
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
    #[derivative(Debug = "ignore")]
    blocks: ConVec<Block<'src, S, L>>,
}
impl<'src, S, L: Language<'src, S>> Global<'src, S, L> {
    pub fn new(name: String) -> Self {
        Self {
            name,
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
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "L::Terminator: Debug"),
    Clone(bound = "L::Terminator: Clone"),
    Default(bound = "L::Terminator: Default")
)]
pub struct Block<'src, S, L: Language<'src, S>> {
    pub insts: Vec<InstId<'src, S, L>>,
    pub term: L::Terminator,
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "S: Debug, L::InstKind: Debug"),
    Clone(bound = "S: Clone, L::InstKind: Clone"),
    PartialEq(bound = "S: PartialEq, L::InstKind: PartialEq")
)]
pub struct Instruction<'src, S, L: Language<'src, S>> {
    pub span: S,
    pub kind: L::InstKind,
}
