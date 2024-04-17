use frolic_cli::prelude::*;

fn main() -> eyre::Result<()> {
    FrolicCli::parse().run_stdio()
}
