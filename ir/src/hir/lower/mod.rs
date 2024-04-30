use super::*;
use error::HirError;
use frolic_ast::prelude::*;

/// Global context passed to HIR generation. This is what's mutated (internally).
pub struct GlobalContext<'a, 'src, S> {
    pub module: Module<'src, S>,
    pub builder: Builder<'src, S>,
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    #[cfg(not(feature = "rayon"))]
    pub report: &'a dyn Fn(HirError) -> bool,
    #[cfg(feature = "rayon")]
    /// Note that this should probably internally use a `SourcedError`. In addition, a cell or
    /// mutex may be used.
    pub report: &'a (dyn Fn(HirError) -> bool + Send + Sync),
}

/// Store the scope as a linked list to avoid allocations.
#[derive(Debug, Clone, Copy)]
pub struct ScopeSeg<'a> {
    frag: &'a str,
    prev: Option<&'a Self>,
}

/// Local context. This stores local qualities like scope.
#[derive(Debug, Clone)]
pub struct LocalContext<'a, 'src, S> {
    scope: ScopeSeg<'a>,
    scope_len: usize,
    phantom: std::marker::PhantomData<&'src S>,
}
impl<'a, 'src, S> LocalContext<'a, 'src, S> {
    pub fn new(scope: &'a str) -> Self {
        Self {
            scope: ScopeSeg {
                frag: scope,
                prev: None,
            },
            scope_len: scope.len(),
            phantom: std::marker::PhantomData,
        }
    }
    /// Create a new local context with an appended scope.
    pub fn with_new_scope<'b>(&'b self, scope: &'b str) -> LocalContext<'b, 'src, S> {
        LocalContext {
            scope: ScopeSeg {
                frag: scope,
                prev: Some(&self.scope),
            },
            scope_len: self.scope_len + scope.len() + 1,
            phantom: std::marker::PhantomData,
        }
    }
    /// Convert the scope into a string.
    pub fn scope_to_str(&self) -> String {
        let mut out = vec![0u8; self.scope_len];
        let mut offset = self.scope_len;
        let mut seg = Some(&self.scope);
        while let Some(&ScopeSeg { frag, prev }) = seg {
            seg = prev;
            let old = offset;
            offset -= frag.len();
            out[offset..old].copy_from_slice(frag.as_bytes());
            offset -= 1;
            out[offset] = b'.';
        }
        unsafe { String::from_utf8_unchecked(out) }
    }
}

/// An AST node that can be lowered to HIR.
pub trait ToHir<'src, S>: Located {
    /// Definition hoisting pass override, defaults to doing nothing.
    fn hoist_pass(
        &self,
        _glb: &GlobalContext<'_, 'src, S>,
        _loc: &mut LocalContext<'_, 'src, S>,
    ) -> bool {
        false
    }
    /// Lower self, modifying the module and builder as necessary. Return true if we should exit
    /// according to the handler.
    /// All state changes must be made to the global context, the local must be in the same state
    /// as it was to start.
    fn to_hir(&self, glb: &GlobalContext<'_, 'src, S>, loc: &mut LocalContext<'_, 'src, S>)
        -> bool;
}

/// Lower a top-level AST. For an expression, use `ToHir` directly.
#[cfg(not(feature = "rayon"))]
pub fn lower_ast<
    'src,
    S: Span,
    A: ToHir<'src, S, Span = S> + 'src,
    F: Copy,
    E: ErrorReporter<SourcedError<F, HirError>> + Copy,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Module<'src, S>>,
    errs: E,
    scope: &str,
) -> Module<'src, S> {
    let module = module.unwrap_or(Module::new());
    let builder = Builder::new();
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
        module,
        builder,
        report: &report,
    };
    let mut local = LocalContext::new(scope);
    let _ = ast.nodes.iter().any(|a| a.hoist_pass(&global, &mut local));
    let _ = ast.nodes.iter().any(|a| a.to_hir(&global, &mut local));
    global.module
}

/// Lower a top-level AST. For an expression, use `ToHir` directly.
#[cfg(feature = "rayon")]
pub fn lower_ast<
    'src,
    S: Span + Sync,
    A: ToHir<'src, S, Span = S> + Send + Sync + 'src,
    F: Copy + Sync,
    E: ErrorReporter<SourcedError<F, HirError>> + Copy + Sync,
>(
    ast: &asts::FrolicAST<A, F>,
    module: Option<Module<'src, S>>,
    errs: E,
    scope: &str,
) -> Module<'src, S> {
    use rayon::prelude::*;
    use std::cell::RefCell;
    let module = module.unwrap_or(Module::new());
    let builder = Builder::new();
    let report = |error| {
        let mut errs = errs;
        errs.report(SourcedError {
            file: ast.file,
            error,
        })
    };
    let global = GlobalContext {
        module,
        builder,
        report: &report,
    };
    let local = LocalContext::new(scope);
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
    });
    global.module
}
