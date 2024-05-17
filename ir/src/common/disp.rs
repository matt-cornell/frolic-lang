use super::*;
use frolic_utils::synccell::*;
use std::fmt::{self, Display, Formatter};

pub struct WithContext<C, V> {
    pub context: C,
    pub value: V,
}

impl<C, V: DispWithContext<C>> Display for WithContext<C, V>
where
    C: Copy,
{
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.value.fmt(self.context, f)
    }
}

#[impl_tools::autoimpl(for<T: trait> &T)]
pub trait DispWithContext<C> {
    fn fmt(&self, context: C, f: &mut Formatter<'_>) -> fmt::Result;
}

impl<C, V: DispWithContext<C>> DispWithContext<C> for SyncCell<V> {
    fn fmt(&self, context: C, f: &mut Formatter<'_>) -> fmt::Result {
        self.with(|v| v.fmt(context, f))
    }
}
impl<C, V: DispWithContext<C>> DispWithContext<C> for SyncRef<'_, V> {
    fn fmt(&self, context: C, f: &mut Formatter<'_>) -> fmt::Result {
        self.with(|v| v.fmt(context, f))
    }
}

impl<'a, 'src, S, L: Language<'src, S>> DispWithContext<&'a Module<'src, S, L>>
    for GlobalId<'src, S, L>
{
    fn fmt(&self, context: &'a Module<'src, S, L>, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(glb) = context.get_global(*self) {
            write!(f, "@{{{}:{:0>4x}}}", glb.name, self.index)
        } else {
            f.write_str("@!ERROR")
        }
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(&'a Module<'src, S, L>, GlobalId<'src, S, L>)> for &'a Global<'src, S, L>
where
    L::InstKind: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
    L::Constant: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
    L::Terminator: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S, L>, GlobalId<'src, S, L>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        write!(
            f,
            "let {} ",
            WithContext {
                value: context.1,
                context: context.0,
            }
        )?;
        if let Some(op) = self.as_alias() {
            writeln!(f, "= {}", WithContext {
                value: op,
                context: (context.0, *self),
            })
        } else {
            writeln!(f, "{{")?;
            for (n, blk) in self.blocks.iter().enumerate() {
                write!(
                    f,
                    "{}",
                    WithContext {
                        value: blk,
                        context: (context.0, *self, BlockId::new(n)),
                    }
                )?;
            }
            writeln!(f, "}}")
        }
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)> for BlockId<'src, S, L>
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S, L>, &'a Global<'src, S, L>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        if let Some(blk) = context.1.get_blk(*self) {
            write!(f, "${{{}:{:0>4x}}}", blk.name, self.index)
        } else {
            f.write_str("$!ERROR")
        }
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(
        &'a Module<'src, S, L>,
        &'a Global<'src, S, L>,
        BlockId<'src, S, L>,
    )> for Block<'src, S, L>
where
    L::InstKind: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
    L::Terminator: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
{
    fn fmt(
        &self,
        context: (
            &'a Module<'src, S, L>,
            &'a Global<'src, S, L>,
            BlockId<'src, S, L>,
        ),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        writeln!(
            f,
            "  {}:",
            WithContext {
                value: context.2,
                context: (context.0, context.1),
            }
        )?;
        for &id in self.insts.iter() {
            if let Some(value) = context.0.get_inst(id) {
                writeln!(
                    f,
                    "    {}",
                    WithContext {
                        value,
                        context: (context.0, context.1, id)
                    }
                )?;
            } else {
                writeln!(f, "    %!ERROR")?;
            }
        }
        writeln!(
            f,
            "    {}",
            WithContext {
                value: &self.term,
                context: (context.0, context.1),
            }
        )
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)> for InstId<'src, S, L>
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S, L>, &'a Global<'src, S, L>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        if let Some(inst) = context.0.get_inst(*self) {
            write!(f, "%{{{}:{:0>4x}}}", inst.name, self.index)
        } else {
            f.write_str("%!ERROR")
        }
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(
        &'a Module<'src, S, L>,
        &'a Global<'src, S, L>,
        InstId<'src, S, L>,
    )> for Instruction<'src, S, L>
where
    L::InstKind: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
{
    fn fmt(
        &self,
        context: (
            &'a Module<'src, S, L>,
            &'a Global<'src, S, L>,
            InstId<'src, S, L>,
        ),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        let id = context.2;
        let context = (context.0, context.1);
        write!(
            f,
            "{} = {}",
            WithContext { value: id, context },
            WithContext {
                value: &self.kind,
                context
            }
        )
    }
}

impl<'a, 'src, S, L: Language<'src, S>>
    DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)> for Operand<'src, S, L>
where
    L::Constant: DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
{
    fn fmt(
        &self,
        context: (&'a Module<'src, S, L>, &'a Global<'src, S, L>),
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Constant(c) => DispWithContext::fmt(c, context, f),
            Self::Instruction(i) => DispWithContext::fmt(i, context, f),
            Self::Global(g) => DispWithContext::fmt(g, context.0, f),
        }
    }
}

impl<'src, S, L: Language<'src, S>> Display for Module<'src, S, L>
where
    L::InstKind: for<'a> DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
    L::Constant: for<'a> DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
    L::Terminator: for<'a> DispWithContext<(&'a Module<'src, S, L>, &'a Global<'src, S, L>)>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "# module {:?}", self.name)?;
        for (n, glb) in self.glbs.iter().enumerate() {
            writeln!(f)?;
            write!(
                f,
                "{}",
                WithContext {
                    value: glb,
                    context: (self, GlobalId::new(n)),
                }
            )?;
        }
        Ok(())
    }
}
