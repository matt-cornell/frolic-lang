//! ## About HIR lowering and contexts
//! The `ToHir` trait takes both a global and a local context. The global context contains things
//! relevant to the entire program, such as definitions and the global symbol table, while the
//! local context contains things only relevant to child nodes, like the scoped symbol table and of
//! course the name of the scope.

use super::lang::*;
use crate::common::symbols::*;
use derivative::Derivative;
use frolic_ast::prelude::*;
use frolic_utils::prelude::*;
use std::borrow::Cow;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use thread_local::ThreadLocal;

mod error;
pub use error::HirError;

const fn const_err<'a, S: Span>() -> Operand<'a, S> {
    Operand::Constant(Constant::Error)
}

mod defs;
mod flow;
mod func;
mod groups;
mod lits;
mod misc;
mod op;

/// Global context for HIR lowering.
#[derive(Derivative)]
#[derivative(Debug)]
#[repr(C)]
pub struct GlobalContext<'a, 'src, S: Span, F: Copy> {
    pub module: &'a Module<'src, S>,
    /// Symbol table. Note that this is behind an immutable reference during the actual lowering.
    pub symbols: &'a mut HashMap<String, UniversalGlobalId<'src, S, F>>,
    #[derivative(Debug = "ignore")]
    pub report: &'a dyn Fn(HirError<'src, S>) -> bool,
    pub file: F,
}

/// Context for HIR lowering, with the added contraint that the reporter is `Sync`.
#[cfg(feature = "rayon")]
#[derive(Derivative)]
#[derivative(Debug)]
#[repr(C)]
pub struct SyncGlobalContext<'a, 'src, S: Span, F: Copy> {
    pub module: &'a Module<'src, S>,
    /// Symbol table. Note that this is behind an immutable reference during the actual lowering.
    pub symbols: &'a mut HashMap<String, UniversalGlobalId<'src, S, F>>,
    #[derivative(Debug = "ignore")]
    pub report: &'a (dyn Fn(HirError<'src, S>) -> bool + Sync),
    pub file: F,
}
impl<'a, 'src, S: Span, F: Copy> SyncGlobalContext<'a, 'src, S, F> {
    pub fn as_unsync(&self) -> &GlobalContext<'a, 'src, S, F> {
        // SAFETY: reference compatible
        unsafe { std::mem::transmute(self) }
    }
    pub fn as_unsync_mut(&mut self) -> &mut GlobalContext<'a, 'src, S, F> {
        // SAFETY: reference compatible
        unsafe { std::mem::transmute(self) }
    }
}

#[derive(Debug, PartialEq)]
enum ScopeRestoreInner<'src> {
    Truncate(usize),
    Replace(Vec<Cow<'src, str>>),
}

#[derive(Debug, PartialEq)]
pub struct ScopeRestore<'src> {
    inner: ScopeRestoreInner<'src>
}

/// Local context for HIR lowering.
#[derive(Debug, Clone)]
pub struct LocalInGlobalContext<'src, S> {
    pub globals: Scopes<Cow<'src, str>, GlobalId<'src, S>>,
    pub scope_name: Vec<Cow<'src, str>>,
}
impl<'src, S> LocalInGlobalContext<'src, S> {
    pub fn new() -> Self {
        Self {
            globals: Scopes::new_single(),
            scope_name: Vec::new(),
        }
    }
    /// Take a name, and return an iterator over the segments of its global spec relative to the local position.
    pub fn glb_segs<'a>(
        &'a self,
        name: &'a DottedName<'src, S>,
    ) -> impl Iterator<Item = &'a Cow<'src, str>> {
        name.global
            .is_none()
            .then(|| self.scope_name.iter())
            .into_iter()
            .flatten()
            .chain(name.segs.iter().map(|s| &s.0))
    }
    /// Globally format a string.
    pub fn glb_format(&self, name: &DottedName<'src, S>) -> String {
        self.glb_segs(name).fold(String::new(), |mut out, s| {
            use std::fmt::Write;
            let _ = write!(out, ".{s}");
            out
        })
    }
    pub fn push_name(&mut self, name: &DottedName<'src, S>) -> ScopeRestore<'src> {
        let it = name.segs.iter().map(|n| n.0.clone());
        if name.global.is_some() {
            ScopeRestore {
                inner: ScopeRestoreInner::Replace(
                    std::mem::replace(&mut self.scope_name, it.collect())
                )
            }
        } else {
            let old_len = self.scope_name.len();
            self.scope_name.extend(it);
            ScopeRestore { inner: ScopeRestoreInner::Truncate(old_len) }
        }
    }
    pub fn restore_name(&mut self, res: ScopeRestore<'src>) {
        match res.inner {
            ScopeRestoreInner::Replace(rep) => {
                if rep.capacity() > self.scope_name.capacity() {
                    self.scope_name = rep;
                } else {
                    self.scope_name.clear();
                    self.scope_name.extend(rep);
                }
            }
            ScopeRestoreInner::Truncate(len) => self.scope_name.truncate(len),
        }
    }
    #[inline]
    pub fn with_pushed_name<R, F: FnOnce(&mut Self) -> R>(&mut self, name: &DottedName<'src, S>, f: F) -> R {
        let res = self.push_name(name);
        let ret = f(self);
        self.restore_name(res);
        ret
    }
}
impl<S> Default for LocalInGlobalContext<'_, S> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct LocalInLocalContext<'a, 'src: 'a, S> {
    insert_func: GlobalId<'src, S>,
    insert_blk: Block<'src, S>,
    resolve_blks: Vec<SyncRef<'a, BlockId<'src, S>>>,
    blks_base: usize,
    pub globals: Scopes<Cow<'src, str>, GlobalId<'src, S>>,
    pub locals: Scopes<Cow<'src, str>, InstId<'src, S>>,
    pub scope_name: Vec<Cow<'src, str>>,
}
impl<'a, 'src: 'a, S> LocalInLocalContext<'a, 'src, S> {
    pub fn new(
        insert_func: GlobalId<'src, S>,
        insert_blk: Block<'src, S>,
        from: LocalInGlobalContext<'src, S>,
    ) -> Self {
        Self {
            insert_func,
            insert_blk,
            resolve_blks: Vec::new(),
            blks_base: 0,
            globals: from.globals,
            locals: Scopes::new_single(),
            scope_name: from.scope_name,
        }
    }
    pub fn ins_func(&self) -> GlobalId<'src, S> {
        self.insert_func
    }
    pub fn goto_pushed_in(&mut self, id: GlobalId<'src, S>, block: Block<'src, S>, module: &Module<'src, S>) {
        let old = std::mem::replace(&mut self.insert_blk, block);
        let old_func = std::mem::replace(&mut self.insert_func, id);
        let blk_id = module[old_func].push_blk(old);
        self.resolve_blks.drain(self.blks_base..).for_each(|b| b.set(blk_id));
    }
    pub fn block_term(&self) -> &SyncCell<Terminator<'src, S>> {
        &self.insert_blk.term
    }
    pub fn lazy_block_id(&mut self, blk: SyncRef<'a, BlockId<'src, S>>) {
        self.resolve_blks.push(blk);
    }
    pub fn to_global(self, module: &Module<'src, S>) -> LocalInGlobalContext<'src, S> {
        let id = module[self.insert_func].push_blk(self.insert_blk);
        self.resolve_blks.into_iter().for_each(|b| b.set(id));
        LocalInGlobalContext {
            globals: self.globals,
            scope_name: self.scope_name,
        }
    }
    pub fn push_swap_blk(
        &mut self,
        module: &Module<'src, S>,
        blk: Block<'src, S>,
    ) -> BlockId<'src, S> {
        let old = std::mem::replace(&mut self.insert_blk, blk);
        let id = module[self.insert_func].push_blk(old);
        self.resolve_blks.drain(self.blks_base..).for_each(|b| b.set(id));
        id
    }
    pub fn with_new_loc<R, F: FnOnce(&mut Self) -> R>(&mut self, func: GlobalId<'src, S>, blk: Block<'src, S>, module: &Module<'src, S>, f: F) -> R {
        let old_start = std::mem::replace(&mut self.blks_base, self.resolve_blks.len());
        let old_blk = std::mem::replace(&mut self.insert_blk, blk);
        let old_func = std::mem::replace(&mut self.insert_func, func);

        let ret = f(self);

        self.blks_base = old_start;
        self.insert_func = old_func;
        let new_blk = std::mem::replace(&mut self.insert_blk, old_blk);
        let bid = module[func].push_blk(new_blk);
        self.resolve_blks.drain(old_start..).for_each(|b| b.set(bid));

        ret
    }

    pub fn push_inst(&mut self, id: InstId<'src, S>) {
        self.insert_blk.insts.push(id);
    }

    /// Take a name, and return an iterator over the segments of its global spec relative to the local position.
    pub fn glb_segs<'b>(
        &'b self,
        name: &'b DottedName<'src, S>,
    ) -> impl Iterator<Item = &'b Cow<'src, str>> {
        name.global
            .is_none()
            .then(|| self.scope_name.iter())
            .into_iter()
            .flatten()
            .chain(name.segs.iter().map(|s| &s.0))
    }
    /// Globally format a string.
    pub fn glb_format(&self, name: &DottedName<'src, S>) -> String {
        self.glb_segs(name).fold(String::new(), |mut out, s| {
            use std::fmt::Write;
            let _ = write!(out, ".{s}");
            out
        })
    }
    pub fn glb_segs_base(&self, suffix: impl std::fmt::Display) -> String {
        use std::fmt::Write;
        let mut out = String::new();
        self.scope_name.iter().for_each(|s| {
            let _ = write!(out, ".{s}");
        });
        let _ = write!(out, "{suffix}");
        out
    }
    pub fn push_name(&mut self, name: &DottedName<'src, S>) -> ScopeRestore<'src> {
        let it = name.segs.iter().map(|n| n.0.clone());
        if name.global.is_some() {
            ScopeRestore {
                inner: ScopeRestoreInner::Replace(
                    std::mem::replace(&mut self.scope_name, it.collect())
                )
            }
        } else {
            let old_len = self.scope_name.len();
            self.scope_name.extend(it);
            ScopeRestore { inner: ScopeRestoreInner::Truncate(old_len) }
        }
    }
    pub fn restore_name(&mut self, res: ScopeRestore<'src>) {
        match res.inner {
            ScopeRestoreInner::Replace(rep) => {
                if rep.capacity() > self.scope_name.capacity() {
                    self.scope_name = rep;
                } else {
                    self.scope_name.clear();
                    self.scope_name.extend(rep);
                }
            }
            ScopeRestoreInner::Truncate(len) => self.scope_name.truncate(len),
        }
    }
    #[inline]
    pub fn with_pushed_name<R, F: FnOnce(&mut Self) -> R>(&mut self, name: &DottedName<'src, S>, f: F) -> R {
        let res = self.push_name(name);
        let ret = f(self);
        self.restore_name(res);
        ret
    }
}

/// Trait for lowering AST nodes.
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait ToHir<'src, F: Copy>: Located {
    /// Predefine globals. This is the only time a mutable handle to the global context is passed.
    /// Returns `true` if the reporter says there was a critical failure.
    fn predef_global(&self, _glb: &mut GlobalContext<'_, 'src, Self::Span, F>, _loc: &mut LocalInGlobalContext<'src, Self::Span>) -> bool {
        false
    }

    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        _loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        (
            const_err(),
            (glb.report)(HirError::GlobalAtLocal {
                kind: std::any::type_name::<Self>(),
                span: self.loc(),
            }),
        )
    }

    /// Lower to HIR. Returns `true` as the second value if the reporter says there was a critical failure.
    /// If this AST node shouldn't produce a value, `Operand::Constant(Constant::Error)` should be
    /// returned.
    fn global(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        _loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        (glb.report)(HirError::LocalAtGlobal {
            kind: std::any::type_name::<Self>(),
            span: self.loc(),
        })
    }

    /// Alternate version of `to_hir` that takes a `SyncGlobalContext`. This is only really
    /// necessary for AST nodes that want to dispatch to their children in parallel.
    #[cfg(feature = "rayon")]
    fn global_sync(
        &self,
        glb: &SyncGlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        self.global(glb.as_unsync(), loc)
    }
}

/// Single-threaded HIR generation, works with or without `rayon`.
pub mod single_thread {
    use super::*;
    pub fn lower_to_hir<
        'src,
        F: Copy,
        A: ToHir<'src, F>,
        E: ErrorReporter<SourcedError<F, HirError<'src, A::Span>>> + Copy,
    >(
        ast: &asts::FrolicAST<A, F>,
        errs: E,
        module: &Module<'src, A::Span>,
        symbols: Option<&mut HashMap<String, UniversalGlobalId<'src, A::Span, F>>>,
    ) {
        let file = ast.file;
        let report = &move |error| {
            let mut errs = errs;
            errs.report(SourcedError { file, error })
        };
        let mut syms = HashMap::default();
        let mut glb: GlobalContext<'_, 'src, A::Span, F> = GlobalContext {
            module,
            file,
            symbols: symbols.unwrap_or(&mut syms),
            report,
        };
        let mut loc = LocalInGlobalContext::new();
        let _ = ast.nodes.iter().any(|n| n.predef_global(&mut glb, &mut loc))
            || ast.nodes.iter().any(|n| n.global(&glb, &mut loc));
    }
}

/// Multi-threaded HIR generation.
#[cfg(feature = "rayon")]
pub mod multi_thread {
    use super::*;
    use rayon::prelude::*;
    pub fn lower_to_hir<
        'src,
        F: Copy + Sync,
        A: ToHir<'src, F> + Send + Sync,
        E: ErrorReporter<SourcedError<F, HirError<'src, A::Span>>> + Copy + Sync,
    >(
        ast: &asts::FrolicAST<A, F>,
        errs: E,
        module: &Module<'src, A::Span>,
        symbols: Option<&mut HashMap<String, UniversalGlobalId<'src, A::Span, F>>>,
    ) where
        A::Span: Send + Sync,
    {
        let file = ast.file;
        let report = &move |error| {
            let mut errs = errs;
            errs.report(SourcedError { file, error })
        };
        let mut syms = HashMap::default();
        let mut glb = SyncGlobalContext {
            module,
            file,
            symbols: symbols.unwrap_or(&mut syms),
            report,
        };
        let mut loc = LocalInGlobalContext::new();
        let erred = ast.nodes.iter().any(|n| n.predef_global(glb.as_unsync_mut(), &mut loc));
        // SAFETY: only one node at a time within a thread can be using the local context, with no
        // way to store it. Thread-local, so only one thread can be using a given context.
        if !erred {
            let tl = ThreadLocal::new();
            let _ = ast.nodes.par_iter().with_min_len(32).any(|n| {
                n.global_sync(&glb, unsafe { &mut *tl.get_or(|| UnsafeCell::new(loc.clone())).get() })
            });
        }
    }
}

#[cfg(feature = "rayon")]
pub use multi_thread::lower_to_hir;
#[cfg(not(feature = "rayon"))]
pub use single_thread::lower_to_hir;
