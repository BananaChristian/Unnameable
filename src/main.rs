use crate::{
    const_and_mut_validator::Validator, diagnostics::Diagnostics, import::ImportEngine,
    indexer::NodeIndex, lexer::Lexer, lowering::Lowering, parser::Parser, semantics::Semantics,
    serializer::Serializer, target::TargetSpec,
};

use std::{cell::RefCell, env, fs, path::PathBuf, rc::Rc};

mod ast;
mod cf_checker;
mod const_and_mut_validator;
mod contract_verifier;
mod diagnostics;
mod hir;
mod import;
mod indexer;
mod layout;
mod lexer;
mod lowering;
mod monomorph;
mod parser;
mod semantics;
mod serializer;
mod target;

fn print_help(program_name: &str) {
    println!("Unnameable Compiler");
    println!("Usage: {} <source.unn> [options]\n", program_name);
    println!("Options:");
    println!("  -h, --help            Show this help menu");
    println!("  --target-arch <str>   Override the target architecture (e.g., x86_64, arm)");
    println!("  --target-os <str>     Override the target OS (e.g., linux, windows, none)");
    println!("  --ptr-width <bytes>   Override pointer width in bytes (e.g., 4, 8)");
    println!("  --int-width <bytes>   Override default integer (usize/isize) width in bytes");
    println!("  --emit-stub <path>    Specify the path to spit out the binary .stub file");
    println!(
        "  --load-stub <path>    Specify the path that the compiler should load the stub file from"
    );
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

    let mut emit_stub_path: Option<PathBuf> = None;
    let mut load_stub_paths: Vec<String> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--target-arch" => {
                if i + 1 < args.len() {
                    arch = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    cli_error("Missing value for --target-arch");
                }
            }
            "--target-os" => {
                if i + 1 < args.len() {
                    os = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    cli_error("Missing value for --target-os");
                }
            }
            "--ptr-width" => {
                if i + 1 < args.len() {
                    ptr_width = Some(
                        args[i + 1]
                            .parse()
                            .expect("Invalid byte integer for --ptr-width"),
                    );
                    i += 2;
                } else {
                    cli_error("Missing value for --ptr-width");
                }
            }
            "--int-width" => {
                if i + 1 < args.len() {
                    int_width = Some(
                        args[i + 1]
                            .parse()
                            .expect("Invalid byte integer for --int-width"),
                    );
                    i += 2;
                } else {
                    cli_error("Missing value for --int-width");
                }
            }
            "--emit-stub" => {
                if i + 1 < args.len() {
                    emit_stub_path = Some(PathBuf::from(args[i + 1].clone()));
                    i += 2;
                } else {
                    cli_error("Missing value for --emit-stub");
                }
            }
            "--load-stub" => {
                if i + 1 < args.len() {
                    load_stub_paths.push(args[i + 1].clone());
                    i += 2;
                } else {
                    cli_error("Missing value for --load-stub");
                }
            }
            // Treat positional values as the input file path
            flag if flag.starts_with('-') => {
                eprintln!("Unknown compilation flag: {}", flag);
                std::process::exit(1);
            }
            _positional => {
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

    let module_name = std::path::Path::new(filename)
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .expect("Failed to derive a valid module name from source file path");

    //  Resolve target layout (Build system choices override, otherwise fallbacks to host)
    let target_spec = TargetSpec::new(arch, os, ptr_width, int_width);

    let source = fs::read_to_string(filename)?;
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        filename.clone(),
        source.clone(),
    )));

    let mut lexer = Lexer::new(&source, Rc::clone(&diagnostics));
    let tokens = lexer.tokenize();
    if lexer.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    let mut parser = Parser::new(tokens, Rc::clone(&diagnostics));
    let ast = parser.parse();
    if parser.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    let mut lowering = Lowering::new(ast, Rc::clone(&diagnostics));
    let mut hir = lowering.lower();
    println!("{:?}",hir);
    if lowering.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    let mut importer = ImportEngine::new(Rc::clone(&diagnostics));
    importer.import(&mut hir, &load_stub_paths);
    if importer.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    let mut semantics = Semantics::new(hir, &target_spec);
    semantics.analyze(Rc::clone(&diagnostics), &importer);
    if semantics.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }
    println!("NAMES TABLE {:?}", semantics.ctxt.names);
    println!("TYPES TABLE {:?}", semantics.ctxt.types.types);

    let monormorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monormorphized_hir);

    if semantics.verify_contracts(&hir_index, Rc::clone(&diagnostics)) {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    if semantics.check_control_flow(&hir_index, Rc::clone(&diagnostics)) {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    let mut validator = Validator::new(&hir_index, &semantics.ctxt, Rc::clone(&diagnostics));
    validator.run();
    if validator.corrupted {
        diagnostics.borrow().print();
        std::process::exit(1);
    }

    if let Some(stub_path) = emit_stub_path {
        let serializer = Serializer::new(module_name, &semantics.ctxt, &hir_index);
        let stub = serializer.serialize();

        let binary_bytes =
            bincode::serialize(&stub).expect("Failed to serialize export stub to binary");
        fs::write(stub_path, binary_bytes)?;
    }

    Ok(())
}

fn cli_error(msg: &str) -> ! {
    eprintln!("Driver Configuration Error: {}", msg);
    std::process::exit(1);
}
