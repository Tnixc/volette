use clap::{Parser, Subcommand};
use std::path::PathBuf;
use volette::compiler;

#[derive(Parser)]
#[command(name = "volette")]
#[command(about = "The volette toolchain")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "build", about = "build <file> | Build a volette program into a binary", alias = "b")]
    Build { file: PathBuf },
    #[command(name = "run", about = "run <file> | Run a volette program", alias = "r")]
    Run,
}

fn main() {
    let cli = Cli::parse();

    // let file = PathBuf::from("main.vt");
    // compiler::build(&file);
    match cli.command {
        Commands::Build { file } => {
            compiler::build(&file);
        }
        // Commands::Run => {
        //     let file = PathBuf::from("main.vt");
        //     compiler::build(&file);
        // }
        _ => todo!(),
    }
}
