use crate::{
    diagnostics::Diagnostics, lexer::Lexer, lowering::Lowering, 
    parser::Parser, semantics::Semantics, target::TargetSpec
};
use std::{env, fs};

mod target;
mod diagnostics;
mod lexer;
mod ast;
mod parser;
mod hir;
mod lowering;
mod semantics;
mod layout;

fn print_help(program_name: &str) {
    println!("Unnameable Compiler");
    println!("Usage: {} <source.unn> [options]\n", program_name);
    println!("Options:");
    println!("  -h, --help            Show this help menu");
    println!("  --target-arch <str>   Override the target architecture (e.g., x86_64, arm)");
    println!("  --target-os <str>     Override the target OS (e.g., linux, windows, none)");
    println!("  --ptr-width <bytes>   Override pointer width in bytes (e.g., 4, 8)");
    println!("  --int-width <bytes>   Override default integer (usize/isize) width in bytes");
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help(&args[0]);
        std::process::exit(1);
    }

    // Check for explicit help flags early
    if args.contains(&"-h".to_string()) || args.contains(&"--help".to_string()) {
        print_help(&args[0]);
        std::process::exit(0);
    }

    // Storage for layout target specifications
    let mut filename: Option<&String> = None;
    let mut arch: Option<String> = None;
    let mut os: Option<String> = None;
    let mut ptr_width: Option<usize> = None;
    let mut int_width: Option<usize> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--target-arch" => {
                if i + 1 < args.len() { arch = Some(args[i + 1].clone()); i += 2; } 
                else { cli_error("Missing value for --target-arch"); }
            }
            "--target-os" => {
                if i + 1 < args.len() { os = Some(args[i + 1].clone()); i += 2; } 
                else { cli_error("Missing value for --target-os"); }
            }
            "--ptr-width" => {
                if i + 1 < args.len() {
                    ptr_width = Some(args[i + 1].parse().expect("Invalid byte integer for --ptr-width"));
                    i += 2;
                } else { cli_error("Missing value for --ptr-width"); }
            }
            "--int-width" => {
                if i + 1 < args.len() {
                    int_width = Some(args[i + 1].parse().expect("Invalid byte integer for --int-width"));
                    i += 2;
                } else { cli_error("Missing value for --int-width"); }
            }
            // Treat positional values as the input file path
            flag if flag.starts_with('-') => {
                eprintln!("Unknown compilation flag: {}", flag);
                std::process::exit(1);
            }
            positional => {
                filename = Some(&args[i]);
                i += 1;
            }
        }
    }

    // Validate that we actually got a file to compile
    let filename = match filename {
        Some(f) => f,
        None => {
            eprintln!("Error: No source input file specified.");
            std::process::exit(1);
        }
    };

    //  Resolve target layout (Build system choices override, otherwise fallbacks to host)
    let target_spec = TargetSpec::new(arch, os, ptr_width, int_width);

    let source = fs::read_to_string(filename)?;
    let mut diagnostics = Diagnostics::new(filename.clone(), source.clone());

    let mut lexer = Lexer::new(&source, &mut diagnostics);
    let tokens = lexer.tokenize();
    if lexer.corrupted {
        diagnostics.print();
        std::process::exit(1);
    }

    let mut parser = Parser::new(tokens, &mut diagnostics);
    let ast = parser.parse();
    if parser.corrupted {
        diagnostics.print();
        std::process::exit(1);
    }

    let mut lowering = Lowering::new(ast, &mut diagnostics);
    let hir = lowering.lower();
    if lowering.corrupted {
        diagnostics.print();
        std::process::exit(1);
    }

    let mut semantics = Semantics::new(hir, &target_spec, &mut diagnostics);
    semantics.analyze();
    if semantics.corrupted {
        diagnostics.print();
        std::process::exit(1);
    }

    Ok(())
}

fn cli_error(msg: &str) -> ! {
    eprintln!("Driver Configuration Error: {}", msg);
    std::process::exit(1);
}
