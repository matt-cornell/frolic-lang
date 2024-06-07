#![allow(clippy::type_complexity)]

use super::*;
use super::lang::*;
use crate::common::Intrinsic;
use crate::common::list::*;
use crate::common::symbols::Scopes;
use bump_scope::allocator_api2::alloc::{Allocator, Global as AGlobal};
use bump_scope::*;
use derivative::Derivative;
use derive_more::{Deref, DerefMut};
use frolic_ast::prelude::*;
use frolic_utils::prelude::*;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::atomic::Ordering;
use smallvec::{smallvec, SmallVec};

#[cfg(feature = "rayon")]
use thread_local::ThreadLocal;

mod error;
pub use error::*;

mod defs;
mod flow;
mod func;
mod groups;
mod lits;
mod misc;
mod op;
mod types;

const fn const_err<'b, S>() -> Operand<'b, S> {
    Operand::Const(Constant::Error)
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct GlobalPreContext<'g, 'b, S: Span, F, A: Allocator + Clone = AGlobal> {
    #[derivative(Debug = "ignore")]
    pub report: &'g dyn Fn(HirError<'b, S, F>) -> LowerResult,
    pub alloc: &'g BumpScope<'b, A>,
    pub module: &'b Module<'b, S>,
    pub global_syms: &'g mut HashMap<&'b str, (F, GlobalId<'b, S>)>,
    pub file: F,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct GlobalContext<'g, 'b, S: Span, F, A: Allocator + Clone = AGlobal> {
    #[derivative(Debug = "ignore")]
    pub report: &'g dyn Fn(HirError<'b, S, F>) -> LowerResult,
    pub alloc: &'g BumpScope<'b, A>,
    pub module: &'b Module<'b, S>,
    pub global_syms: &'g HashMap<&'b str, (F, GlobalId<'b, S>)>,
    pub file: F,
}
impl<'g, 'b, S: Span, F, A: Allocator + Clone> GlobalContext<'g, 'b, S, F, A> {
    #[allow(clippy::ptr_arg)]
    pub fn intern_cow<'src: 'b>(&self, cow: &Cow<'src, str>) -> &'b str {
        match cow {
            Cow::Borrowed(s) => s,
            Cow::Owned(s) => self.alloc.alloc_str(s).into_ref(),
        }
    }
}

#[cfg(feature = "rayon")]
#[derive(Derivative)]
#[derivative(Debug)]
pub struct SyncGlobalContext<'g, 'b, S: Span, F, A: Allocator + Clone + Sync = AGlobal> {
    #[derivative(Debug = "ignore")]
    pub report: &'g (dyn Fn(HirError<'b, S, F>) -> LowerResult + Send + Sync),
    pub alloc_tl: ThreadLocal<BumpPoolGuard<'b, A, 1, true, true>>,
    pub alloc_pool: &'b BumpPool<A>,
    pub module: &'b Module<'b, S>,
    pub global_syms: &'g HashMap<&'b str, (F, GlobalId<'b, S>)>,
    pub file: F,
}
#[cfg(feature = "rayon")]
impl<'g, 'b, S: Span, F, A: Allocator + Clone + Sync> SyncGlobalContext<'g, 'b, S, F, A> {
    pub fn alloc(&self) -> &BumpPoolGuard<'b, A, 1, true, true> {
        self.alloc_tl.get_or(|| self.alloc_pool.get())
    }
    pub fn make_unsync(&self) -> GlobalContext<'_, 'b, S, F, A>
    where
        F: Clone,
    {
        GlobalContext {
            report: self.report,
            alloc: &**self.alloc(),
            module: self.module,
            global_syms: self.global_syms,
            file: self.file.clone(),
        }
    }
}
#[cfg(feature = "rayon")]
impl<S: Span, F: Clone, A: Allocator + Clone + Sync> Clone for SyncGlobalContext<'_, '_, S, F, A> {
    fn clone(&self) -> Self {
        Self {
            alloc_tl: ThreadLocal::new(),
            file: self.file.clone(),
            ..*self
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct LocalInGlobalContext<'b> {
    pub scope_name: String,
    pub global_prefixes: SmallVec<[&'b str; 1]>,
}
impl<'b> LocalInGlobalContext<'b> {
    /// Create and use a temporary local context from this one.
    pub fn in_local<S, R, F: FnOnce(&mut LocalInLocalContext<'b, S>) -> R>(
        &mut self,
        insert: BlockId<'b, S>,
        f: F,
    ) -> R {
        let ctx = std::mem::take(self);
        let mut this = LocalInLocalContext {
            ctx,
            insert,
            locals: Scopes::new(),
        };
        let ret = f(&mut this);
        *self = this.ctx;
        ret
    }
}

#[derive(Derivative, Deref, DerefMut)]
#[derivative(Debug(bound = ""))]
pub struct LocalInLocalContext<'b, S> {
    #[deref]
    #[deref_mut]
    pub ctx: LocalInGlobalContext<'b>,
    pub locals: Scopes<&'b str, InstId<'b, S>>,
    pub insert: BlockId<'b, S>,
}

/// Wrapper around `std::any::type_name` that gives a shorter output.
fn pretty_name<T: ?Sized>() -> &'static str {
    let mut raw = std::any::type_name::<T>();
    // trim templates
    if let Some(idx) = raw.find('<') {
        raw = &raw[..idx];
    }
    // trim module
    if let Some(idx) = raw.rfind("::") {
        raw = &raw[(idx + 2)..];
    }
    raw
}

#[allow(unused_variables)]
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait ToHir<'b, F: Clone>: Located {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        println!("bt: {}", std::backtrace::Backtrace::capture());
        (
            const_err(),
            (glb.report)(
                HirIce::GlobalAstAtLocal {
                    kind: pretty_name::<Self>(),
                    span: self.loc(),
                }
                .into(),
            ),
        )
    }

    fn predef_global(
        &self,
        glb: &mut GlobalPreContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        Ok(())
    }
    fn global(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        (glb.report)(
            HirIce::LocalAstAtGlobal {
                kind: pretty_name::<Self>(),
                span: self.loc(),
            }
            .into(),
        )
    }
    #[cfg(feature = "rayon")]
    fn global_sync(
        &self,
        glb: &SyncGlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        self.global(&glb.make_unsync(), loc)
    }
}

pub mod single_threaded {
    use super::*;
    use std::cell::UnsafeCell;

    /// Single-threaded lowering to HIR.
    pub fn lower_to_hir<
        'b,
        F: Clone,
        A: ToHir<'b, F>,
        E: ErrorReporter<SourcedError<F, HirError<'b, A::Span, F>>>,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: impl Into<&'b BumpScope<'b>>,
        module: &'b Module<'b, A::Span>,
        errs: E,
        global_syms: Option<&mut HashMap<&'b str, (F, GlobalId<'b, A::Span>)>>,
        starting_scope: String,
    ) -> LowerResult {
        let file = ast.file.clone();
        let errs = UnsafeCell::new(errs);
        let mut default = Default::default();
        let global_syms = global_syms.unwrap_or(&mut default);
        let report = &|err: HirError<'b, _, F>| {
            // SAFETY: this doesn't let the reference escape and we already know it's !Sync
            let rep = unsafe { &mut *errs.get() };
            let erred = rep.report(SourcedError {
                file: file.clone(),
                error: err,
            });
            (!erred).then_some(()).ok_or(EarlyReturn)
        };
        let alloc = alloc.into();
        let mut loc = LocalInGlobalContext {
            global_prefixes: smallvec![alloc.alloc_str(&starting_scope).into_ref()],
            scope_name: starting_scope,
        };
        {
            let mut glb = GlobalPreContext {
                report,
                alloc,
                module,
                global_syms,
                file: ast.file.clone(),
            };
            ast.nodes
                .iter()
                .try_for_each(|a| a.predef_global(&mut glb, &mut loc))?;
        }
        {
            let glb = GlobalContext {
                report,
                alloc,
                module,
                global_syms,
                file: ast.file.clone(),
            };
            ast.nodes.iter().try_for_each(|a| a.global(&glb, &mut loc))
        }
    }

    /// Lower to a new module, returning it.
    pub fn lower_to_ret_module<
        'b,
        F: Clone + Send + Sync,
        A: ToHir<'b, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<'b, A::Span, F>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: impl Into<&'b BumpScope<'b>>,
        errs: E,
        global_syms: Option<&mut HashMap<&'b str, (F, GlobalId<'b, A::Span>)>>,
        mod_name: impl std::fmt::Display,
        starting_scope: String,
    ) -> &'b Module<'b, A::Span>
    where
        A::Span: Sync + Span,
    {
        let alloc = alloc.into();
        let mod_name = alloc.alloc_fmt(format_args!("{mod_name}")).into_ref();
        let module = alloc.alloc(Module::new(mod_name)).into_ref();
        let _ = lower_to_hir(ast, alloc, module, errs, global_syms, starting_scope);
        module
    }

    /// The type of allocator necessary. For single-threaded, we only need a `Bump`.
    pub type BumpAlloc = Bump;

    /// Make the bump allocator usable for allocation. Useful for combined single/multithreaded use.
    /// For a single-threaded `Bump`, this is a no-op.
    pub fn alloc_from_bump(bump: &Bump) -> &Bump {
        bump
    }
}

/// Multi-threaded lowering.
#[cfg(feature = "rayon")]
pub mod multi_threaded {
    use super::*;
    use rayon::prelude::*;

    /// Lower the code to HIR using multiple threads.
    pub fn lower_to_hir<
        'b,
        F: Clone + Send + Sync,
        A: ToHir<'b, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<'b, A::Span, F>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: &'b BumpPool,
        module: &'b Module<'b, A::Span>,
        errs: E,
        global_syms: Option<&mut HashMap<&'b str, (F, GlobalId<'b, A::Span>)>>,
        starting_scope: String,
    ) -> LowerResult
    where
        A::Span: Sync,
    {
        let file = ast.file.clone();
        let mut default = Default::default();
        let global_syms = global_syms.unwrap_or(&mut default);
        let report = &|err: HirError<'b, _, F>| {
            let mut rep = errs;
            let erred = rep.report(SourcedError {
                file: file.clone(),
                error: err,
            });
            (!erred).then_some(()).ok_or(EarlyReturn)
        };
        let local_alloc = alloc.get();
        let mut loc = LocalInGlobalContext {
            global_prefixes: smallvec![local_alloc.alloc_str(&starting_scope).into_ref()],
            scope_name: starting_scope,
        };
        {
            let mut glb = GlobalPreContext {
                report,
                alloc: &local_alloc,
                module,
                global_syms,
                file: ast.file.clone(),
            };
            ast.nodes
                .iter()
                .try_for_each(|a| a.predef_global(&mut glb, &mut loc))?;
        }
        std::mem::drop(local_alloc); // return this memory asap for the heavy processing
        {
            let glb = SyncGlobalContext {
                report,
                alloc_tl: ThreadLocal::new(),
                alloc_pool: alloc,
                module,
                global_syms,
                file: ast.file.clone(),
            };
            ast.nodes
                .par_iter()
                .try_for_each_init(|| loc.clone(), |loc, a| a.global_sync(&glb, loc))
        }
    }

    /// Lower to a new module, returning it.
    pub fn lower_to_ret_module<
        'b,
        F: Clone + Send + Sync,
        A: ToHir<'b, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<'b, A::Span, F>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: &'b BumpPool,
        errs: E,
        global_syms: Option<&mut HashMap<&'b str, (F, GlobalId<'b, A::Span>)>>,
        mod_name: impl std::fmt::Display,
        starting_scope: String,
    ) -> &'b Module<'b, A::Span>
    where
        A::Span: Sync + Span,
    {
        let alloc_ = alloc.get();
        let mod_name = alloc_.alloc_fmt(format_args!("{mod_name}")).into_ref();
        let module = alloc_.alloc(Module::new(mod_name)).into_ref();
        let _ = lower_to_hir(ast, alloc, module, errs, global_syms, starting_scope);
        module
    }

    /// The type of bump allocator necessary. For multithreaded, we need a pool.
    pub type BumpAlloc = BumpPool;

    /// Make the bump allocator usable for allocation. Useful for combined single/multithreaded use.
    pub fn alloc_from_bump(bump: &BumpAlloc) -> BumpPoolGuard<'_, AGlobal, 1, true, true> {
        bump.get()
    }
}

#[cfg(feature = "rayon")]
pub use multi_threaded::*;

#[cfg(not(feature = "rayon"))]
pub use single_threaded::*;
