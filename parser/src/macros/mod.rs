use super::lexer::error::*;
use super::parser::error::FormatToken;
use super::prelude::*;
use super::*;
use miette::Diagnostic;
use os_str_bytes::OsStrBytes;
use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

/// All macro-based spans are going to have an overridden location in the final output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroSpan<S> {
    /// A token that came from inside a macro.
    Token(S),
    /// A token that was passed in and reused.
    Macro(S),
}
impl<S> MacroSpan<S> {
    pub fn inner(self) -> S {
        match self {
            MacroSpan::Token(v) | MacroSpan::Macro(v) => v,
        }
    }
}
impl<S: Into<miette::SourceSpan>> From<MacroSpan<S>> for miette::SourceSpan {
    fn from(value: MacroSpan<S>) -> Self {
        value.inner().into()
    }
}
impl<S: Span> Span for MacroSpan<S> {
    fn merge(self, other: Self) -> Self {
        use MacroSpan::*;
        match (self, other) {
            (Token(a), Token(b)) => Token(a.merge(b)),
            (Macro(a), Macro(b)) => Macro(a.merge(b)),
            (Token(a), _) | (_, Token(a)) => Token(a),
        }
    }
    fn offset(self) -> usize {
        self.inner().offset()
    }
    fn len(self) -> usize {
        self.inner().len()
    }
}
impl<S: SpanConstruct> SpanConstruct for MacroSpan<S> {
    fn new(offset: usize, len: usize) -> Self {
        Self::Macro(S::new(offset, len))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroSource<F> {
    String(String),
    Token(F),
}

#[derive(Debug, Clone, PartialEq, Error, Diagnostic)]
pub enum MacroError<S: Span> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    TokenizeError(TokenizeError<S>),
    #[error("Expected an include path, found {}", FormatToken(Some(&.0.kind)))]
    IncludeExpectedPath(#[label] Token<'static, S>),
    #[error("Unknown macro `{name}`")]
    UnknownMacro {
        #[label]
        span: S,
        name: String,
    },
    #[error("invalid path given: {:?}", bstr::BStr::new(.path))]
    InvalidPathGiven {
        #[label]
        span: S,
        path: Vec<u8>,
    },
    #[error("Error from Lua: {1}")]
    Lua(#[label] S, String),
}

#[derive(Debug, Clone, Copy)]
pub struct ExpandContext<'src, 'a> {
    /// Source code used to parse this
    frolic_source: &'src [u8],
    /// Macro lookup table
    lookup: &'a HashMap<Cow<'src, str>, mlua::Function<'a>>,
    /// Expand `lazy!` macros. Should be true for non-internal calls.
    expand_lazy: bool,
}

mod private {
    pub trait Sealed {}
    impl<S> Sealed for super::Source<S> {}
    impl<S> Sealed for super::MacroSpan<S> {}
}

/// Maybe it's a MacroSpan. Maybe it's not.
pub trait MaybeWrapped: private::Sealed {
    type ToWrap;
    fn wrap(self) -> MacroSpan<Self::ToWrap>;
}

/// It's not a MacroSpan.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
struct Source<S>(S);
impl<S: Into<miette::SourceSpan>> From<Source<S>> for miette::SourceSpan {
    fn from(value: Source<S>) -> Self {
        value.0.into()
    }
}
impl<S: Span> Span for Source<S> {
    fn merge(self, other: Self) -> Self {
        Self(self.0.merge(other.0))
    }
    fn offset(self) -> usize {
        self.0.offset()
    }
    fn len(self) -> usize {
        self.0.len()
    }
}
impl<S: SpanConstruct> SpanConstruct for Source<S> {
    fn new(offset: usize, len: usize) -> Self {
        Self(S::new(offset, len))
    }
}
impl<S> MaybeWrapped for Source<S> {
    type ToWrap = S;
    fn wrap(self) -> MacroSpan<Self::ToWrap> {
        MacroSpan::Token(self.0)
    }
}

impl<S> MaybeWrapped for MacroSpan<S> {
    type ToWrap = S;
    fn wrap(self) -> MacroSpan<Self::ToWrap> {
        self
    }
}

fn expand_single_token<'src, S: Span + MaybeWrapped>(
    tok: &mut Token<'src, S>,
    ctx: &ExpandContext<'src, '_>,
) -> mlua::Result<()>
where
    S::ToWrap: Span + Send + Sync,
{
    let TokenKind::BoundMacro(..) = tok.kind else {
        return Ok(());
    };
    let TokenKind::BoundMacro(name, mut arg) =
        std::mem::replace(&mut tok.kind, TokenKind::Brace(Vec::new()))
    else {
        unreachable!()
    };
    let span = tok.span;
    match &*name {
        "lazy" => {
            if ctx.expand_lazy {
                let arg = *arg;
                tok.kind = arg.kind;
                tok.span = span.merge(arg.span);
            } else {
                tok.kind = TokenKind::BoundMacro(Cow::Borrowed("lazy"), arg);
            }
            return Ok(());
        }
        "unlazy" => {
            *tok = *arg;
            return expand_single_token(
                tok,
                &ExpandContext {
                    expand_lazy: true,
                    ..*ctx
                },
            );
        }
        "include" => {
            let mut arg = *arg;
            expand_single_token(&mut arg, ctx)?;
            let TokenKind::String(path) = arg.kind else {
                return Err(mlua::Error::external(MacroError::IncludeExpectedPath(
                    arg.into_static().map_span(MaybeWrapped::wrap),
                )));
            };
            let path = if let Some(path) = Path::from_io_bytes(&path) {
                path
            } else {
                return Err(mlua::Error::external(MacroError::InvalidPathGiven {
                    span: span.wrap(),
                    path: path.to_vec(),
                }));
            };
            let src = std::fs::read(path)?; // TODO: relative resolution
            let mut rep = OnceReporter::<SourcedError<(), TokenizeError<DummySpan>>>::new();
            let toks = tokenize(&src, (), &mut rep);
            rep.as_result().map_err(|SourcedError { file, error }| {
                mlua::Error::external(SourcedError {
                    file,
                    error: MacroError::TokenizeError(error.map_span(|_| span.wrap())),
                })
            })?;
            tok.kind = TokenKind::Brace(toks).into_static().map_span(|_| span);
        }
        "include_str" => {
            let mut arg = *arg;
            expand_single_token(&mut arg, ctx)?;
            let TokenKind::String(path) = arg.kind else {
                return Err(mlua::Error::external(MacroError::IncludeExpectedPath(
                    arg.into_static().map_span(MaybeWrapped::wrap),
                )));
            };
            let path = if let Some(path) = Path::from_io_bytes(&path) {
                path
            } else {
                return Err(mlua::Error::external(MacroError::InvalidPathGiven {
                    span: span.wrap(),
                    path: path.to_vec(),
                }));
            };
            let src = std::fs::read(path)?; // TODO: relative resolution
            tok.kind = TokenKind::String(src.into());
        }
        n => {
            expand_single_token(
                &mut arg,
                &ExpandContext {
                    expand_lazy: false,
                    ..*ctx
                },
            )?;
            let Some(f) = ctx.lookup.get(n) else {
                return Err(mlua::Error::external(MacroError::UnknownMacro {
                    name: name.into_owned(),
                    span: span.wrap(),
                }));
            };
            arg.map_span(MaybeWrapped::wrap);
            todo!()
        }
    }
    Ok(())
}

pub fn fold_macros_inner<'src, F: Copy, S: Span + MaybeWrapped>(
    input: &mut [Token<'src, S>],
    file: F,
    ctx: &ExpandContext<'src, '_>,
    errs: &mut dyn ErrorReporter<SourcedError<MacroSource<F>, MacroError<MacroSpan<S::ToWrap>>>>,
) -> bool
where
    S::ToWrap: Span + Send + Sync,
{
    let mut stack = vec![(input, 0)];
    while let Some((slice, idx)) = stack.last_mut() {
        let Some(tok) = slice.get_mut(*idx) else {
            stack.pop();
            continue;
        };
        *idx += 1;
        let res = expand_single_token(tok, ctx);
        if let Err(err) = res {
            if let mlua::Error::ExternalError(err) = &err {
                if let Some(err) = err.downcast_ref::<MacroError<MacroSpan<S::ToWrap>>>() {
                    let erred = errs.report(SourcedError {
                        file: MacroSource::Token(file),
                        error: err.clone(),
                    });
                    if erred {
                        return true;
                    } else {
                        continue;
                    }
                }
            }
            let erred = errs.report(SourcedError {
                file: MacroSource::Token(file),
                error: MacroError::Lua(tok.span.wrap(), err.to_string()),
            });
            if erred {
                return true;
            }
        }
    }
    false
}

pub fn fold_macros<
    'src,
    F: Copy,
    S: Span + Send + Sync,
    E: ErrorReporter<SourcedError<MacroSource<F>, MacroError<MacroSpan<S>>>>,
>(
    input: &mut [Token<'src, S>],
    file: F,
    frolic_source: &'src impl AsRef<[u8]>,
    lookup: Option<&HashMap<Cow<'src, str>, mlua::Function>>,
    mut errs: E,
) {
    let default = HashMap::new();
    let lookup = lookup.unwrap_or(&default);
    let frolic_source = frolic_source.as_ref();
    fold_macros_inner(
        unsafe {
            std::mem::transmute::<&mut [Token<'src, S>], &mut [Token<'src, Source<S>>]>(input)
        },
        file,
        &ExpandContext {
            frolic_source,
            lookup,
            expand_lazy: true,
        },
        &mut errs,
    );
}
