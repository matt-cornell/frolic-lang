use std::marker::Unsize;
use std::ops::{CoerceUnsized, Deref};
use std::fmt::Debug;

pub trait AllImplTrait<T: ?Sized> {
    type First: Unsize<T> + ?Sized;
    type Rest: AllImplTrait<T>;
}

impl<T: Unsize<T> + ?Sized> AllImplTrait<T> for () {
    type First = T;
    type Rest = ();
}

macro_rules! impl_trait {
    ($first:tt, $($rest:tt,)+) => {
        impl<T: Unsize<T> + ?Sized, $first, $($rest),*> AllImplTrait<T> for ($first, $($rest),*) where $first: Unsize<T>, $($rest: Unsize<T>),* {
            type First = $first;
            type Rest = ($($rest,)*);
        }
        impl_trait!($($rest,)+);
    };
    ($last:tt,) => {
        impl<T: Unsize<T> + ?Sized, $last: Unsize<T>> AllImplTrait<T> for ($last,) {
            type First = $last;
            type Rest = ();
        }
    }
}
impl_trait!(A, B, C, D, E, F, G,);

pub trait AstDefs {
    type AstTrait<'a>: Unsize<Self::AstTrait<'a>> + ?Sized + 'a;
    type Box<T: ?Sized>: Deref<Target = T>;

    fn make_box<'a, T: Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> impl CoerceUnsized<Self::Box<Self::AstTrait<'a>>>;
}

#[macro_export]
macro_rules! trait_alias {
    ($pub:vis trait $name:ident = $($traits:tt)*) => {
        $pub trait $name: $($traits)* {}
        impl<T: $($traits)*> $name for T {}
    }
}

pub struct DebugAsts;
impl AstDefs for DebugAsts {
    type AstTrait<'a> = dyn Debug + 'a;
    type Box<T: ?Sized> = Box<T>;

    fn make_box<'a, T: Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> impl CoerceUnsized<Self::Box<Self::AstTrait<'a>>> {
        Box::new(val)
    }
}
