mod compiler;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "volette")]
#[command(about = "The volette toolchain")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "build", about = "build <file> | Build a volette program into a binary")]
    Build { file: PathBuf },
    #[command(name = "run", about = "run <file> | Run a volette program")]
    Run { file: PathBuf },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { file } => {
            compiler::build(&file);
        }
        Commands::Run { file } => {
            compiler::run(&file);
        }
    }
}
