use super::lang::*;
use crate::common::symbols::Scopes;
use bump_scope::allocator_api2::alloc::{Allocator, Global as AGlobal};
use bump_scope::*;
use derivative::Derivative;
use derive_more::{Deref, DerefMut};
use frolic_ast::prelude::*;
use frolic_utils::prelude::*;
use std::borrow::Cow;

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

#[derive(Derivative)]
#[derivative(Debug, Clone(bound = "F: Clone"), Copy(bound = "F: Copy"))]
pub struct GlobalContext<'g, 'b, S: Span, F, A: Allocator + Clone = AGlobal> {
    #[derivative(Debug = "ignore")]
    pub report: &'g dyn Fn(HirError<S>) -> LowerResult,
    pub alloc: &'g BumpScope<'b, A>,
    pub module: &'b Module<'b, S>,
    pub file: F,
}
impl<'g, 'b, S: Span, F, A: Allocator + Clone> GlobalContext<'g, 'b, S, F, A> {
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
    pub report: &'g (dyn Fn(HirError<S>) -> LowerResult + Send + Sync),
    pub alloc_tl: ThreadLocal<BumpPoolGuard<'b, A, 1, true, true>>,
    pub alloc_pool: &'b BumpPool<A>,
    pub module: &'b Module<'b, S>,
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

#[derive(Derivative)]
#[derivative(Debug(bound = ""), Default(bound = ""), Clone(bound = ""))]
pub struct LocalInGlobalContext<'b, S> {
    pub globals: Scopes<&'b str, GlobalId<'b, S>>,
    pub scope_name: String,
}
impl<'b, S> LocalInGlobalContext<'b, S> {
    /// Create and use a temporary local context from this one.
    pub fn in_local<R, F: FnOnce(&mut LocalInLocalContext<'b, S>) -> R>(
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
    pub ctx: LocalInGlobalContext<'b, S>,
    pub locals: Scopes<&'b str, Operand<'b, S>>,
    pub insert: BlockId<'b, S>,
}

#[allow(unused_variables)]
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait ToHir<'b, F: Clone>: Located {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        (
            Operand::Const(Constant::Error),
            (glb.report)(
                HirIce::GlobalAstAtLocal {
                    kind: std::any::type_name::<Self>(),
                    span: self.loc(),
                }
                .into(),
            ),
        )
    }

    fn predef_global(&self, glb: &GlobalContext<'_, 'b, Self::Span, F>) -> LowerResult {
        Ok(())
    }
    fn global(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b, Self::Span>,
    ) -> LowerResult {
        (glb.report)(
            HirIce::LocalAstAtGlobal {
                kind: std::any::type_name::<Self>(),
                span: self.loc(),
            }
            .into(),
        )
    }
    #[cfg(feature = "rayon")]
    fn global_sync(
        &self,
        glb: &SyncGlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b, Self::Span>,
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
        E: ErrorReporter<SourcedError<F, HirError<A::Span>>>,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: impl Into<&'b BumpScope<'b>>,
        module: &'b Module<'b, A::Span>,
        errs: E,
        starting_scope: String,
    ) -> LowerResult {
        let file = ast.file.clone();
        let errs = UnsafeCell::new(errs);
        let glb = GlobalContext {
            report: &move |err| {
                // SAFETY: this doesn't let the reference escape and we already know it's !Sync
                let rep = unsafe { &mut *errs.get() };
                let erred = rep.report(SourcedError {
                    file: file.clone(),
                    error: err,
                });
                (!erred).then_some(()).ok_or(EarlyReturn)
            },
            alloc: alloc.into(),
            module,
            file: ast.file.clone(),
        };
        let mut loc = LocalInGlobalContext {
            scope_name: starting_scope,
            globals: Scopes::new(),
        };
        ast.nodes.iter().try_for_each(|a| a.predef_global(&glb))?;
        ast.nodes.iter().try_for_each(|a| a.global(&glb, &mut loc))
    }

    /// Lower to a new module, returning it.
    pub fn lower_to_ret_module<
        'b,
        F: Clone + Send + Sync,
        A: ToHir<'b, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<A::Span>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: &'b BumpPool,
        errs: E,
        mod_name: impl std::fmt::Display,
        starting_scope: String,
    ) -> &'b Module<'b, A::Span>
    where
        A::Span: Sync + Span,
    {
        let alloc_ = alloc.get();
        let mod_name = alloc_.alloc_fmt(format_args!("{mod_name}")).into_ref();
        let module = alloc_.alloc(Module::new(mod_name)).into_ref();
        lower_to_hir(ast, alloc, module, errs, starting_scope);
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
        E: ErrorReporter<SourcedError<F, HirError<A::Span>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: &'b BumpPool,
        module: &'b Module<'b, A::Span>,
        errs: E,
        starting_scope: String,
    ) where
        A::Span: Sync,
    {
        let file = ast.file.clone();
        let glb = SyncGlobalContext {
            report: &move |err| {
                let mut rep = errs;
                let erred = rep.report(SourcedError {
                    file: file.clone(),
                    error: err,
                });
                (!erred).then_some(()).ok_or(EarlyReturn)
            },
            alloc_tl: ThreadLocal::new(),
            alloc_pool: alloc,
            module,
            file: ast.file.clone(),
        };
        let loc = LocalInGlobalContext {
            scope_name: starting_scope,
            globals: Scopes::new(),
        };
        let unsync = glb.make_unsync();
        let _ = ast.nodes.iter().try_for_each(|a| a.predef_global(&unsync));
        let _ = ast
            .nodes
            .par_iter()
            .try_for_each_init(|| loc.clone(), |loc, a| a.global_sync(&glb, loc));
    }

    /// Lower to a new module, returning it.
    pub fn lower_to_ret_module<
        'b,
        F: Clone + Send + Sync,
        A: ToHir<'b, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<A::Span>>> + Copy + Send + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        alloc: &'b BumpPool,
        errs: E,
        mod_name: impl std::fmt::Display,
        starting_scope: String,
    ) -> &'b Module<'b, A::Span>
    where
        A::Span: Sync + Span,
    {
        let alloc_ = alloc.get();
        let mod_name = alloc_.alloc_fmt(format_args!("{mod_name}")).into_ref();
        let module = alloc_.alloc(Module::new(mod_name)).into_ref();
        lower_to_hir(ast, alloc, module, errs, starting_scope);
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
