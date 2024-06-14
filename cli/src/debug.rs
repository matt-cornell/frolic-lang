use super::*;

#[derive(Debug, Clone, Subcommand)]
pub enum FrolicDebug {
    /// Lex the input code, printing out the tokens.
    Lex(FrolicDebugLex),
    /// Parse the input code, printing out the AST.
    Parse(FrolicDebugParse),
    /// Parse the input code and lower it to HIR.
    Hir(FrolicDebugHir),
}
impl Runnable for FrolicDebug {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        stdin: I,
        stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        match self {
            Self::Lex(cmd) => cmd.run(stdin, stdout, stderr),
            Self::Parse(cmd) => cmd.run(stdin, stdout, stderr),
            Self::Hir(cmd) => cmd.run(stdin, stdout, stderr),
        }
    }
}

/// Common debug args.
#[derive(Debug, Clone, Args)]
#[group(required = true, multiple = false)]
pub struct Source {
    /// Input code, as an argument.
    #[arg(short, long)]
    pub code: Option<String>,
    /// Path to the input to read.
    #[arg(short, long)]
    pub path: Option<PathBuf>,
}

#[derive(Debug, Clone, Args)]
pub struct FrolicDebugLex {
    #[command(flatten)]
    pub source: Source,
}
impl Runnable for FrolicDebugLex {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        let errs = Mutex::new(DiagnosticPrint::new(
            stderr,
            miette::GraphicalReportHandler::new(),
        ));

        let toks: Vec<Token<PrettySpan>> = match self.source {
            Source {
                code: Some(code),
                path: None,
            } => {
                let file = FILE_REGISTRY.add_file(PackageId::ROOT, "<command line>", code);
                let toks = tokenize(file.contents(), file, &errs);
                toks
            }
            Source {
                code: None,
                path: Some(path),
            } => {
                let code = std::fs::read(&path)?;
                let file = FILE_REGISTRY.add_file(
                    PackageId::ROOT,
                    path.into_os_string().to_string_lossy(),
                    code,
                );
                let toks = tokenize(file.contents(), file, &errs);
                toks
            }
            _ => panic!("exactly one of `code` and `path` should be set!"),
        };

        writeln!(stdout, "{toks:#?}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, Args)]
pub struct FrolicDebugParse {
    #[command(flatten)]
    pub source: Source,
    /// Parse as an expression instead of top-level
    #[arg(short, long)]
    pub expr: bool,
}
impl Runnable for FrolicDebugParse {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        let errs = Mutex::new(DiagnosticPrint::new(
            stderr,
            miette::GraphicalReportHandler::new(),
        ));

        let (file, toks): (_, Vec<Token<PrettySpan>>) = match self.source {
            Source {
                code: Some(code),
                path: None,
            } => {
                let file = FILE_REGISTRY.add_file(PackageId::ROOT, "<command line>", code);
                let toks = tokenize(file.contents(), file, &errs);
                (file, toks)
            }
            Source {
                code: None,
                path: Some(path),
            } => {
                let code = std::fs::read(&path)?;
                let file = FILE_REGISTRY.add_file(
                    PackageId::ROOT,
                    path.into_os_string().to_string_lossy(),
                    code,
                );
                let toks = tokenize(file.contents(), file, &errs);
                (file, toks)
            }
            _ => panic!("exactly one of `code` and `path` should be set!"),
        };

        if self.expr {
            let ast = parse_expr(&toks, file, &errs, DebugAsts::new());
            write!(stdout, "{ast:#?}")?;
        } else {
            let ast = parse_tl(&toks, file, &errs, DebugAsts::new());
            write!(stdout, "{ast:#?}")?;
        }
        errs.into_inner().unwrap().into_result()?;

        Ok(())
    }
}

#[derive(Debug, Clone, Args)]
pub struct FrolicDebugHir {
    #[command(flatten)]
    pub source: Source,
    #[arg(short, long)]
    pub include_input: bool,
}
impl Runnable for FrolicDebugHir {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        let errs = Mutex::new(DiagnosticPrint::new(
            stderr,
            miette::GraphicalReportHandler::new(),
        ));

        let (file, toks): (_, Vec<Token<PrettySpan>>) = match self.source {
            Source {
                code: Some(code),
                path: None,
            } => {
                let file = FILE_REGISTRY.add_file(PackageId::ROOT, "<command line>", code);
                let toks = tokenize(file.contents(), file, &errs);
                (file, toks)
            }
            Source {
                code: None,
                path: Some(path),
            } => {
                let code = std::fs::read(&path)?;
                let file = FILE_REGISTRY.add_file(
                    PackageId::ROOT,
                    path.into_os_string().to_string_lossy(),
                    code,
                );
                let toks = tokenize(file.contents(), file, &errs);
                (file, toks)
            }
            _ => panic!("exactly one of `code` and `path` should be set!"),
        };

        let bump = BumpAlloc::new();
        let ast = parse_tl(&toks, file, &errs, HirAsts::new());
        let module = lower_to_ret_module(&ast, &bump, &errs, None, file, "debug_hir".to_string());
        std::mem::drop(ast);

        {
            fmt2io::write(&mut stdout, |writer| {
                use std::fmt::Write;
                let mut wrapped_stdout = indenter::indented(writer).with_str("# ");
                writeln!(wrapped_stdout, "source input:")?;
                writeln!(wrapped_stdout, "{}", bstr::BStr::new(file.contents()))?;
                writeln!(wrapped_stdout)
            })?;
        }
        write!(stdout, "{module}")?;

        errs.into_inner().unwrap().into_result()?;

        Ok(())
    }
}
