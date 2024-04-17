use super::*;

#[derive(Debug, Clone, Subcommand)]
pub enum FrolicDebug {
    Lex(FrolicDebugLex),
    Parse(FrolicDebugParse),
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
        }
    }
}

/// Common debug args.
#[derive(Debug, Clone, Args)]
#[group(required = true, multiple = false)]
pub struct Source {
    #[arg(short, long)]
    pub code: Option<String>,
    #[arg(short, long)]
    pub path: Option<PathBuf>,
}

#[derive(Debug, Clone, Args)]
pub struct FrolicDebugLex {
    #[command(flatten)]
    source: Source,
}
impl Runnable for FrolicDebugLex {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        use frolic_parser::prelude::*;
        use frolic_utils::prelude::*;
        use std::sync::Mutex;

        let errs = DiagnosticPrint::new(stderr, miette::GraphicalReportHandler::new());
        let errs = Mutex::new(errs);

        type Reporter<E> = Mutex<DiagnosticPrint<E, miette::GraphicalReportHandler>>;

        let toks = match self.source {
            Source {
                code: Some(code),
                path: None,
            } => {
                let file = FILE_REGISTRY.add_file(PackageId::ROOT, "<command line>", code);
                let toks = tokenize::<_, FileId, &Reporter<E>>(file.contents(), file, &errs);
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
                let toks = tokenize::<_, FileId, &Reporter<E>>(file.contents(), file, &errs);
                toks
            }
            _ => panic!("exactly one of `code` and `path` should be set!"),
        };

        for tok in toks {
            writeln!(stdout, "{tok:?}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Args)]
pub struct FrolicDebugParse {
    #[command(flatten)]
    source: Source,
    /// Parse as an expression instead of top-level
    #[arg(short, long)]
    expr: bool,
}
impl Runnable for FrolicDebugParse {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        use frolic_ast::prelude::*;
        use frolic_parser::prelude::*;
        use frolic_utils::prelude::*;
        use std::sync::Mutex;

        let errs = DiagnosticPrint::new(stderr, miette::GraphicalReportHandler::new());
        let errs = Mutex::new(errs);

        type Reporter<E> = Mutex<DiagnosticPrint<E, miette::GraphicalReportHandler>>;

        let (file, toks) = match self.source {
            Source {
                code: Some(code),
                path: None,
            } => {
                let file = FILE_REGISTRY.add_file(PackageId::ROOT, "<command line>", code);
                let toks = tokenize::<_, FileId, &Reporter<E>>(file.contents(), file, &errs);
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
                let toks = tokenize::<_, FileId, &Reporter<E>>(file.contents(), file, &errs);
                (file, toks)
            }
            _ => panic!("exactly one of `code` and `path` should be set!"),
        };

        if self.expr {
            let ast = parse_expr::<DebugAsts, FileId, &Reporter<E>>(&toks, file, &errs, DebugAsts);
            write!(stdout, "{ast:#?}")?;
        } else {
            let ast = parse_tl::<DebugAsts, FileId, &Reporter<E>>(&toks, file, &errs, DebugAsts);
            write!(stdout, "{ast:#?}")?;
        }
        errs.into_inner().unwrap().into_result()?;

        Ok(())
    }
}
