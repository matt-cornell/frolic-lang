use super::*;
use std::fmt::Debug;
use std::ops::Deref;

pub trait AstDefs {
    type AstTrait<'a>: Unsize<Self::AstTrait<'a>> + ?Sized + 'a where Self: 'a;
    type AstBox<'a>: Deref<Target = Self::AstTrait<'a>> where Self: 'a;

    fn make_box<'a, T: Unsize<Self::AstTrait<'a>> + 'a>(val: T) -> Self::AstBox<'a> where Self: 'a;
}

#[macro_export]
macro_rules! trait_alias {
    ($pub:vis trait $name:ident = $($traits:tt)*) => {
        $pub trait $name: $($traits)* {}
        impl<T: $($traits)*> $name for T {}
    };
    ($pub:vis trait $name:ident<$($gens:ident),* $(,)?> = $($traits:tt)*) => {
        $pub trait $name<$($gens)*>: $($traits)* {}
        impl<$($gens,)* T: $($traits)*> $name<$($gens)*> for T {}
    };
    ($pub:vis trait $name:ident<$($gens:ident),* $(,)?> = $($traits:tt)* where $($res:tt),* $(,)?) => {
        $pub trait $name<$($gens)*>: $($traits)* where $($res,)* {}
        impl<$($gens,)* T: $($traits)*> $name<$($gens)*> for T where $($res,)* {}
    };
}

trait_alias!(pub trait LocDebug<Span> = Debug + Located<Span = Span>);

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
