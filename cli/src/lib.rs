#![feature(unsize)]

use clap::{Args, Parser, Subcommand};
use frolic_ir::prelude::*;
use frolic_parser::prelude::*;
use frolic_utils::prelude::*;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::sync::Mutex;

pub mod debug;

frolic_parser::def_box_asts!(pub struct HirAsts<'b, S, F> = 'src : 'b -> dyn ToHir<'b, F, Span = S> + Send + Sync + 'b where S: Span + 'static, F: Copy + 'static);

/// Some kind of command that can be run.
pub trait Runnable: Sized {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        stdin: I,
        stdout: O,
        stderr: E,
    ) -> eyre::Result<()>;
    /// Run this command with standard input/output
    fn run_stdio(self) -> eyre::Result<()> {
        self.run(io::stdin(), io::stdout(), io::stderr())
    }
    /// Run this command with `io::Empty` for all streams
    fn silent(self) -> eyre::Result<()> {
        self.run(io::empty(), io::empty(), io::empty())
    }
}

/// Frolic main API
#[derive(Debug, Clone, Parser)]
pub enum FrolicCli {
    /// Debug subcommands. These are mainly meant to be used for compiler development, and are
    /// skipped during parsing in release builds.
    #[cfg_attr(debug_assertions, command(subcommand))]
    #[cfg_attr(not(debug_assertions), command(skip))]
    Debug(debug::FrolicDebug),
}
impl Runnable for FrolicCli {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        stdin: I,
        stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        match self {
            Self::Debug(cmd) => cmd.run(stdin, stdout, stderr),
        }
    }
}

pub mod prelude {
    pub use crate::FrolicCli;
    pub use crate::Runnable;
    pub use clap::Parser;
}
