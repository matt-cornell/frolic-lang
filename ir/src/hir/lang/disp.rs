use super::*;
use std::fmt::{self, Display, Formatter};

impl<S> Display for Id<'_, Module<'_, S>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "!{{{:?}:{:0>6x}}}",
            self.0.name,
            self.0 as *const _ as usize & 0xffffff
        )
    }
}
impl<S> Display for Id<'_, Global<'_, S>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "@{{{}:{:0>6x}}}",
            self.0.name,
            self.0 as *const _ as usize & 0xffffff
        )
    }
}
impl<S> Display for Id<'_, Block<'_, S>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "${{{}:{:0>6x}}}",
            self.0.name,
            self.0 as *const _ as usize & 0xffffff
        )
    }
}
impl<S> Display for Id<'_, Inst<'_, S>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "%{{{}:{:0>6x}}}",
            self.0.name,
            self.0 as *const _ as usize & 0xffffff
        )
    }
}

impl Display for Constant<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => f.write_str("error"),
            Self::Null => f.write_str("null"),
            Self::Unknown => f.write_str("unknown"),
            Self::Int(v) => write!(f, "int {v}"),
            Self::Float(v) => write!(f, "float {v}"),
            Self::String(v) => write!(f, "str {:?}", bstr::BStr::new(v)),
            Self::Namespace(n) => write!(f, "namespace {n}"),
        }
    }
}
impl<S> Display for Operand<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(v) => Display::fmt(v, f),
            Self::Inst(v) => Display::fmt(v, f),
            Self::Global(v) => Display::fmt(v, f),
        }
    }
}
impl<S> Display for Terminator<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unreachable => f.write_str("unreachable"),
            Self::Return(v) => write!(f, "return {v}"),
            Self::CondBr {
                cond,
                if_true,
                if_false,
            } => write!(f, "if {cond} then {if_true} else {if_false}"),
            Self::UncondBr { blk } => write!(f, "goto {blk}"),
        }
    }
}
impl<S> Display for InstKind<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Call { func, arg } => write!(f, "call {func} {arg}"),
            Self::FnType { arg, ret } => write!(f, "fnty {arg} -> {ret}"),
            Self::Arg => f.write_str("arg"),
            Self::Bind(val) => write!(f, "bind {val}"),
            Self::Cast { val, ty } => write!(f, "cast {val} to {ty}"),
            Self::Ascribe { val, ty } => write!(f, "asc {val} to {ty}"),
            Self::Phi(vars) => {
                f.write_str("phi")?;
                let mut it = vars.iter().peekable();
                while let Some((blk, val)) = it.next() {
                    write!(f, " {blk} => {val}")?;
                    if it.peek().is_some() {
                        f.write_str(",")?;
                    }
                }
                Ok(())
            }
        }
    }
}
impl<S> Display for Inst<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", Id(self), self.kind)
    }
}
impl<S> Display for Block<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", Id(self))?;
        for i in self.insts.iter() {
            writeln!(f, "  {i}")?;
        }
        writeln!(f, "  {}", self.term.get())
    }
}
impl<S> Display for Global<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use std::fmt::Write;
        if let Some(cap) = self.captures {
            writeln!(f, "# captures {}", Id(cap))?;
        }
        if let Some(op) = self.as_alias() {
            writeln!(f, "let {} = {op};", Id(self))
        } else {
            writeln!(f, "let {} {{", Id(self))?;
            let mut ind = indenter::indented(f).with_str("  ");
            for blk in self.blocks.iter() {
                write!(ind, "{blk}")?;
            }
            write!(f, "}}")
        }
    }
}
impl<S> Display for Module<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "# module {}", self.name)?;
        for glb in self.globals.iter() {
            writeln!(f)?;
            writeln!(f, "{glb}")?;
        }
        Ok(())
    }
}
