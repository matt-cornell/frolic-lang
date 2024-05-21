use atomic_ref::AtomicRef;
use std::fmt::{self, Debug, Formatter};
use std::sync::atomic::Ordering;

fn fmt_ref<T>(r: &AtomicRef<T>, f: &mut Formatter) -> fmt::Result {
    if let Some(ptr) = r.load(Ordering::Relaxed) {
        write!(f, "{ptr:p}")
    } else {
        f.write_str("null")
    }
}

pub mod list;
pub mod symbols;
