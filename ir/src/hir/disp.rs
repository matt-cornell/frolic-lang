use super::*;
use std::fmt::Display;

const PTR_WIDTH: usize = 6;
const PTR_MASK: usize = (1 << PTR_WIDTH * 4) - 1;

impl<S> Display for Value<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{{{}:{:0>PTR_WIDTH$x}}}", self.name, self as *const _ as usize & PTR_MASK)?;
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
                        ValueInner::Function(func) => write!(f, "global {:#}", **func),
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
impl<S> Display for Block<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "${{{:0>PTR_WIDTH$x}}}", self as *const _ as usize & PTR_MASK)?;
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
impl<S> Display for Definition<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "@{{{name}:{:0>PTR_WIDTH$x}}}", self as *const _ as usize & PTR_MASK)?;
        } else {
            write!(f, "@{{{:0>PTR_WIDTH$x}}}", self as *const _ as usize & PTR_MASK)?;
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
