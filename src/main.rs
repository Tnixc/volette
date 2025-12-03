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
    #[command(name = "build", about = "build <file> | Build a volette program into assembly", alias = "b")]
    Build {
        file: PathBuf,
        #[arg(short = 'o', long = "output", help = "Output assembly file path")]
        output: Option<PathBuf>,
    },
    #[command(name = "run", about = "run <file> | Run a volette program", alias = "r")]
    Run,
}

fn main() {
    let cli = Cli::parse();

    // let file = PathBuf::from("main.vt");
    // compiler::build(&file);
    match cli.command {
        Commands::Build { file, output } => {
            let output_path = output.unwrap_or_else(|| PathBuf::from("output.s"));
            compiler::build(&file, &output_path);
        }
        // Commands::Run => {
        //     let file = PathBuf::from("main.vt");
        //     compiler::build(&file);
        // }
        _ => todo!(),
    }
}
