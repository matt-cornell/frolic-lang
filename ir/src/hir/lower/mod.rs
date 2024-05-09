use super::*;
use crate::common::symbols::*;
use error::HirError;
use frolic_ast::prelude::*;
use parking_lot::RwLock;
use std::rc::Rc;
use std::sync::Arc;

mod defs;
mod flow;
mod func;
mod groups;
mod lits;
mod misc;
mod op;

pub struct GlobalSymbols<'src, S, F> {
    pub backing: boxcar::Vec<Symbol<S, F, MonoOwned<Definition<'src, S>>>>,
    pub map: RwLock<ahash::HashMap<String, usize>>,
}
impl<S, F> Default for GlobalSymbols<'_, S, F> {
    fn default() -> Self {
        Self {
            backing: boxcar::Vec::new(),
            map: RwLock::default(),
        }
    }
}

/// Global context passed to HIR generation. This is what's mutated (internally).
pub struct GlobalContext<'a, 'src, S: Span, F> {
    pub file: F,
    pub symbols: Arc<GlobalSymbols<'src, S, F>>,
    pub module: Box<Module<'src, S>>,
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    #[cfg(not(feature = "rayon"))]
    pub report: &'a dyn Fn(HirError<'src, S>) -> bool,
    #[cfg(feature = "rayon")]
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    pub report: &'a (dyn Fn(HirError<'src, S>) -> bool + Send + Sync),
}

#[derive(Debug, Clone, PartialEq)]
enum RestoreInner<'src> {
    Index(usize),
    Path(Vec<Cow<'src, str>>),
}

/// A scope to restore to.
#[derive(Debug, Clone, PartialEq)]
pub struct RestorePoint<'src> {
    inner: RestoreInner<'src>,
}

/// Local context. This stores local qualities like scope.
pub struct LocalContext<'src, S> {
    pub scope: Vec<Cow<'src, str>>,
    pub builder: Builder<'src, S>,
    pub symbols: LocalMap<'src, Symbol<S, (), Owned<Value<'src, S>>>>,
    pub global_scope: LocalMap<'src, usize>,
}
impl<'src, S> LocalContext<'src, S> {
    pub fn new() -> Self {
        Self {
            scope: Vec::new(),
            builder: Builder::new(),
            symbols: LocalMap::default(),
            global_scope: LocalMap::single(),
        }
    }
    pub fn push_scope<I: IntoIterator<Item = impl Into<Cow<'src, str>>>>(
        &mut self,
        scope: I,
        global: bool,
    ) -> RestorePoint<'src> {
        if global {
            let old =
                std::mem::replace(&mut self.scope, scope.into_iter().map(Into::into).collect());
            RestorePoint {
                inner: RestoreInner::Path(old),
            }
        } else {
            let idx = self.scope.len();
            self.scope.extend(scope.into_iter().map(Into::into));
            RestorePoint {
                inner: RestoreInner::Index(idx),
            }
        }
    }
    pub fn restore_scope(&mut self, saved: RestorePoint<'src>) {
        match saved.inner {
            RestoreInner::Index(idx) => self.scope.truncate(idx),
            RestoreInner::Path(path) => self.scope = path,
        }
    }
    pub fn globalize_name<T>(&self, name: &DottedName<'_, T>) -> String {
        if name.global.is_some() {
            name.to_string()
        } else {
            self.scope
                .iter()
                .map(|s| format!(".{s}"))
                .chain(
                    name.segs
                        .iter()
                        .take(if self.builder.get_pos().is_some() {
                            1
                        } else {
                            usize::MAX
                        })
                        .map(|(s, _)| format!(".{s}")),
                )
                .collect()
        }
    }
}
impl<S> Default for LocalContext<'_, S> {
    fn default() -> Self {
        Self::new()
    }
}

/// An AST node that can be lowered to HIR.
pub trait ToHir<'src, F>: Located {
    /// Definition hoisting pass override, defaults to doing nothing.
    fn hoist_pass(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span, F>,
        _loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        false
    }
    /// Lower self, modifying the module and builder as necessary. Return true if we should exit
    /// according to the handler.
    /// All state changes must be made to the global context, the local must be in the same state
    /// as it was to start.
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool);
}
impl<'src, F, A: ToHir<'src, F> + ?Sized> ToHir<'src, F> for Box<A> {
    fn hoist_pass(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        A::hoist_pass(self, glb, loc)
    }

    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        A::to_hir(self, glb, loc)
    }
}
impl<'src, F, A: ToHir<'src, F> + ?Sized> ToHir<'src, F> for Rc<A> {
    fn hoist_pass(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        A::hoist_pass(self, glb, loc)
    }

    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        A::to_hir(self, glb, loc)
    }
}
impl<'src, F, A: ToHir<'src, F> + ?Sized> ToHir<'src, F> for Arc<A> {
    fn hoist_pass(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        A::hoist_pass(self, glb, loc)
    }

    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        A::to_hir(self, glb, loc)
    }
}
/// Lower a top-level AST. For an expression, use `ToHir` directly.
#[cfg(not(feature = "rayon"))]
pub fn lower_ast<
    'src,
    F: Copy,
    A: ToHir<'src, F> + 'src,
    E: ErrorReporter<SourcedError<F, HirError<'src, A::Span>>> + Copy,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Box<Module<'src, A::Span>>>,
    symbols: Option<Arc<GlobalSymbols<'src, A::Span, F>>>,
    errs: E,
) -> Box<Module<'src, A::Span>> {
    let module = module.unwrap_or_default();
    let symbols = symbols.unwrap_or_default();
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
        file: ast.file,
        symbols,
        module,
        report: &report,
    };
    let mut local = LocalContext::new();
    let _ = ast.nodes.iter().any(|a| a.hoist_pass(&global, &mut local));
    let _ = ast.nodes.iter().any(|a| a.to_hir(&global, &mut local).1);
    global.module
}

/// Lower a top-level AST. For an expression, use `ToHir` directly.
#[cfg(feature = "rayon")]
pub fn lower_ast<
    'src,
    F: Copy + Send + Sync,
    A: ToHir<'src, F> + Send + Sync + 'src,
    E: ErrorReporter<SourcedError<F, HirError<'src, A::Span>>> + Copy + Sync,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Box<Module<'src, A::Span>>>,
    symbols: Option<Arc<GlobalSymbols<'src, A::Span, F>>>,
    errs: E,
) -> Box<Module<'src, A::Span>>
where
    A::Span: Send + Sync,
{
    use rayon::prelude::*;
    use std::cell::RefCell;
    let module = module.unwrap_or_default();
    let symbols = symbols.unwrap_or_default();
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
        file: ast.file,
        symbols,
        module,
        report: &report,
    };
    let tl_local = thread_local::ThreadLocal::new();
    let _ = ast.nodes.par_iter().any(|a| {
        a.hoist_pass(
            &global,
            &mut *tl_local.get_or(|| RefCell::default()).borrow_mut(),
        )
    });
    let _ = ast.nodes.par_iter().any(|a| {
        a.to_hir(
            &global,
            &mut *tl_local.get_or(|| RefCell::default()).borrow_mut(),
        )
        .1
    });
    global.module
}
