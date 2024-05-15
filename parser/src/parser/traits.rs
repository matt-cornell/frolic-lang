use super::*;
use std::fmt::Debug;
use std::ops::Deref;

pub trait AstDefs: 'static {
    type AstTrait<'a>: Unsize<Self::AstTrait<'a>> + ?Sized + 'a;
    type AstBox<'a>: Deref<Target = Self::AstTrait<'a>>;

    fn make_box<'a, T: Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a>;
}

/// Create an alias for a trait.
///
/// ## Usage
/// ```
/// trait_alias!(trait MyTrait = Trait1 + Trait2);
/// ```
/// Generics are supported, but bounds need to be specified with a `where` clause.
///
/// The generated code for this is just:
/// ```
/// trait MyTrait: Trait1 + Trait2 {}
/// impl<T: Trait1 + Trait2> MyTrait for T {}
/// ```
#[macro_export]
macro_rules! trait_alias {
    ($pub:vis trait $name:ident = $($traits:tt)*) => {
        $pub trait $name: $($traits)* {}
        impl<T: $($traits)*> $name for T {}
    };
    ($pub:vis trait $name:ident<$($lts:lifetime),*> = $($traits:tt)* $(where $($res:tt)*)?) => {
        $pub trait $name<$($lts,)*>: $($traits)* $(where $($res)*)? {}
        impl<$($lts,)* T: $($traits)*> $name<$($lts)*> for T $(where $($res)*)? {}
    };
    ($pub:vis trait $name:ident<$($lts:lifetime,)* $($gens:ident),* $(,)?> = $($traits:tt)* $(where $($res:tt)*)?) => {
        $pub trait $name<$($lts,)* $($gens,)*>: $($traits)* $(where $($res)*)? {}
        impl<$($gens,)* T: $($traits)*> $name<$($gens)*> for T $(where $($res)*)? {}
    };
}

/// Convenience macro to create a struct and implement `AstDefs` for it.
///
/// ## Usage
/// ```
/// def_box_asts!(struct MyAsts = 'src -> dyn MyTrait<'src>);
/// ```
/// Generics are supported, but bounds must be specified with a `where` clause.
///
/// Because the generated type can be generic, a simple unit struct isn't always possible, so it
/// will generate one with a `PhantomData` field. Lifetimes and generics are used in the return
/// type of a function, so the generated type is `'static` no matter what.
///
/// A type is generated with a `new()` method (`const`) and an impl of `AstDefs`.
#[macro_export]
macro_rules! def_box_asts {
    ($pub:vis struct $name:ident = $lt:lifetime -> $traits:ty) => {
        $pub struct $name;
        impl $name {
            pub const fn new() -> Self {
                Self
            }
        }
        impl $crate::prelude::AstDefs for $name {
            type AstTrait<$lt> = $traits;
            type AstBox<$lt> = Box<$traits>;
            fn make_box<'a, T: std::marker::Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a> {
                Box::new(val) as _
            }
        }
    };
    ($pub:vis struct $name:ident<$($lts:lifetime),+> = $lt:lifetime -> $traits:ty $(where $($res:tt)*)?) => {
        $pub struct $name<$($lts,)*>(std::marker::PhantomData<fn() -> ($(&$gens (),)*)>) $(where $($res)*)?;
        impl<$($lts,)*> $name<$($lts,)*> $(where $($res)*)? {
            pub const fn new() -> Self {
                Self(std::marker::PhantomData)
            }
        }
        impl<$($lts,)*> $crate::prelude::AstDefs for $name<$($lts,)*> $(where $($res)*)? {
            type AstTrait<$lt> = $($traits)*;
            type AstBox<$lt> = Box<$($traits)*>;
            fn make_box<'a, T: std::marker::Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a> {
                Box::new(val) as _
            }
        }
    };
    ($pub:vis struct $name:ident<$($lts:lifetime,)* $($gens:ident),* $(,)?> = $lt:lifetime -> $traits:ty $(where $($res:tt)*)?) => {
        $pub struct $name<$($lts,)* $($gens,)*>(std::marker::PhantomData<fn() -> ($(&$lts (),)* $($gens,)*)>) $(where $($res)*)?;
        impl<$($lts,)* $($gens,)*> $name<$($lts,)* $($gens,)*> $(where $($res)*)? {
            pub const fn new() -> Self {
                Self(std::marker::PhantomData)
            }
        }
        impl<$($lts,)* $($gens,)*> $crate::prelude::AstDefs for $name<$($lts,)* $($gens,)*> $(where $($res)*)? {
            type AstTrait<$lt> = $traits;
            type AstBox<$lt> = Box<$traits>;
            fn make_box<'a, T: std::marker::Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a> {
                Box::new(val) as _
            }
        }
    };
}

trait_alias!(pub trait LocDebug<Span> = Debug + Located<Span = Span>);
def_box_asts!(pub struct DebugAsts<S> = 'a -> dyn LocDebug<S> + 'a where S: Span + 'static);

/*
// def_box_asts! generates this
pub struct DebugAsts<S>(PhantomData<S>);
impl<S> DebugAsts<S> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}
impl<S: Span + 'static> AstDefs for DebugAsts<S> {
    type AstTrait<'a> = dyn LocDebug<S> + 'a;
    type AstBox<'a> = Box<dyn LocDebug<S> + 'a>;

    fn make_box<'a, T: Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a> {
        Box::new(val) as _
    }
}
*/
