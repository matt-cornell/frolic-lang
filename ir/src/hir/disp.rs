use super::*;
use std::fmt::Display;

const PTR_WIDTH: usize = 6;
const PTR_MASK: usize = (1 << PTR_WIDTH * 4) - 1;

impl<S: Debug> Debug for Value<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Value")
            .field("addr", &(self as *const Self))
            .field("name", &self.name)
            .field("span", &self.span)
            .field("inner", &self.inner)
            .field("parent", &self.parent.load(Ordering::Relaxed))
            .field("prev_val", &self.prev_val.load(Ordering::Relaxed))
            .field("next_val", &self.next_val.load(Ordering::Relaxed))
            .field("refs", &self.refs.load(Ordering::Relaxed))
            .field("dropped", &self.dropped.load(Ordering::Relaxed))
            .finish()
    }
}
impl<S> Display for Value<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "%{{{}:{:0>PTR_WIDTH$x}}}",
            "", // TODO: fix segfault
            self as *const Self as usize & PTR_MASK
        )?;
        if !f.alternate() {
            f.write_str(" = ")?;
            if self.dropped.load(Ordering::Relaxed) {
                f.write_str("!DROPPED")
            } else {
                unsafe {
                    match &self.inner {
                        ValueInner::Null => f.write_str("null"),
                        ValueInner::Error => f.write_str("error"),
                        ValueInner::Int(val) => write!(f, "{val}"),
                        ValueInner::Float(val) => write!(f, "{val}"),
                        ValueInner::String(val) => write!(f, "{:?}", bstr::BStr::new(&**val)),
                        ValueInner::Comment(val) => write!(f, "## {:?}", bstr::BStr::new(&**val)),
                        ValueInner::NewLoc(val) => write!(f, "wrap {}", **val),
                        ValueInner::Call(func, arg) => write!(f, "call {:#} {:#}", **func, **arg),
                        ValueInner::UnresolvedGlobal(func) => write!(f, "global {func}"),
                        ValueInner::ResolvedGlobal(func) => write!(f, "global {:#}", **func),
                        ValueInner::FunctionArg(func) => write!(f, "argof {:#}", **func),
                        ValueInner::CondBr {
                            cond,
                            if_true,
                            if_false,
                        } => write!(
                            f,
                            "if {:#} then {:#} else {:#}",
                            **cond, **if_true, **if_false
                        ),
                        ValueInner::UncondBr(blk) => write!(f, "goto {:#}", **blk),
                        ValueInner::Phi {
                            pred,
                            value,
                            default,
                        } => write!(f, "phi {:#} => {:#} else {:#}", **pred, **value, **default),
                    }
                }
            }
        } else {
            Ok(())
        }
    }
}

impl<S> Debug for Block<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Block")
            .field("addr", &(self as *const Self))
            .field("parent", &self.parent.load(Ordering::Relaxed))
            .field("prev_blk", &self.prev_blk.load(Ordering::Relaxed))
            .field("next_blk", &self.next_blk.load(Ordering::Relaxed))
            .field("first_val", &self.first_val.load(Ordering::Relaxed))
            .field("last_val", &self.last_val.load(Ordering::Relaxed))
            .finish()
    }
}
impl<S> Display for Block<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "${{{:0>PTR_WIDTH$x}}}",
            self as *const _ as usize & PTR_MASK
        )?;
        if !f.alternate() {
            f.write_str(":\n")?;
            let mut p = self.first_val.load(Ordering::Relaxed);
            while let Some(r) = unsafe { p.as_ref() } {
                p = r.next_val.load(Ordering::Relaxed);
                writeln!(f, "    {r}")?;
            }
        }
        Ok(())
    }
}

impl<S: Debug> Debug for Definition<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Definition")
            .field("addr", &(self as *const Self))
            .field("name", &self.name)
            .field("parent", &self.parent.load(Ordering::Relaxed))
            .field("prev_def", &self.prev_def.load(Ordering::Relaxed))
            .field("next_def", &self.next_def.load(Ordering::Relaxed))
            .field("first_blk", &self.first_blk.load(Ordering::Relaxed))
            .field("last_blk", &self.last_blk.load(Ordering::Relaxed))
            .finish()
    }
}
impl<S> Display for Definition<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(
                f,
                "@{{{name}:{:0>PTR_WIDTH$x}}}",
                self as *const _ as usize & PTR_MASK
            )?;
        } else {
            write!(
                f,
                "@{{{:0>PTR_WIDTH$x}}}",
                self as *const _ as usize & PTR_MASK
            )?;
        }
        if !f.alternate() {
            f.write_str(" {\n")?;
            let mut p = self.first_blk.load(Ordering::Relaxed);
            while let Some(r) = unsafe { p.as_ref() } {
                p = r.next_blk.load(Ordering::Relaxed);
                write!(f, "  {r}")?;
            }
            f.write_str("}\n")?;
        }
        Ok(())
    }
}

impl<S> Debug for Module<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Module")
            .field("addr", &(self as *const Self))
            .field("first_def", &self.first_def.load(Ordering::Relaxed))
            .field("last_def", &self.last_def.load(Ordering::Relaxed))
            .finish()
    }
}
impl<S> Display for Module<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut p = self.first_def.load(Ordering::Relaxed);
        while let Some(r) = unsafe { p.as_ref() } {
            p = r.next_def.load(Ordering::Relaxed);
            write!(f, "{r}")?;
        }
        Ok(())
    }
}
