use super::*;
use error::HirError;
use frolic_ast::prelude::*;

mod defs;
mod flow;
mod func;
mod groups;
mod lits;
mod misc;
mod op;

/// Global context passed to HIR generation. This is what's mutated (internally).
pub struct GlobalContext<'a, 'src, S> {
    pub module: Module<'src, S>,
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    #[cfg(not(feature = "rayon"))]
    pub report: &'a dyn Fn(HirError) -> bool,
    #[cfg(feature = "rayon")]
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    pub report: &'a (dyn Fn(HirError) -> bool + Send + Sync),
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
#[derive(Debug, Clone)]
pub struct LocalContext<'src, S> {
    pub scope: Vec<Cow<'src, str>>,
    pub builder: Builder<'src, S>,
}
impl<'src, S> LocalContext<'src, S> {
    pub const fn new() -> Self {
        Self {
            scope: Vec::new(),
            builder: Builder::new(),
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
}

/// An AST node that can be lowered to HIR.
pub trait ToHir<'src>: Located {
    /// Definition hoisting pass override, defaults to doing nothing.
    fn hoist_pass(
        &self,
        _glb: &GlobalContext<'_, 'src, Self::Span>,
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
        glb: &GlobalContext<'_, 'src, Self::Span>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool);
}

/// Lower a top-level AST. For an expression, use `ToHir` directly.
#[cfg(not(feature = "rayon"))]
pub fn lower_ast<
    'src,
    A: ToHir<'src> + 'src,
    F: Copy,
    E: ErrorReporter<SourcedError<F, HirError>> + Copy,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Module<'src, A::Span>>,
    errs: E,
) -> Module<'src, A::Span> {
    let module = module.unwrap_or(Module::new());
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
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
    S: Span + Sync,
    A: ToHir<'src> + Send + Sync + 'src,
    F: Copy + Sync,
    E: ErrorReporter<SourcedError<F, HirError>> + Copy + Sync,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Module<'src, A::Span>>,
    errs: E,
) -> Module<'src, A::Span> {
    use rayon::prelude::*;
    use std::cell::RefCell;
    let module = module.unwrap_or(Module::new());
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
        module,
        report: &report,
    };
    let local = LocalContext::new();
    let tl_local = thread_local::ThreadLocal::new();
    let _ = ast.nodes.par_iter().any(|a| {
        a.hoist_pass(
            &global,
            &mut *tl_local.get_or(|| RefCell::new(local.clone())).borrow_mut(),
        )
    });
    let _ = ast.nodes.par_iter().any(|a| {
        a.to_hir(
            &global,
            &mut *tl_local.get_or(|| RefCell::new(local.clone())).borrow_mut(),
        )
        .1
    });
    global.module
}
