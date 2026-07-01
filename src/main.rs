use crate::{diagnostics::Diagnostics, lexer::Lexer, lowering::Lowering, parser::Parser, semantics::Semantics};
use std::{env, fs};

mod diagnostics;
mod lexer;
mod ast;
mod parser;
mod hir;
mod lowering;
mod semantics;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source.unn>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let source = fs::read_to_string(filename)?;

    let mut diagnostics = Diagnostics::new(filename.clone(),source.clone());

    let mut lexer = Lexer::new(&source, &mut diagnostics);
    let tokens = lexer.tokenize();
    for token in &tokens {
        println!("{:?}", token);
    }

    if lexer.corrupted {
        diagnostics.print();
        std::process::exit(1)
    }

    let mut parser = Parser::new(tokens, &mut diagnostics);
    let ast = parser.parse();

    println!("{:?}", ast);
    if parser.corrupted {
        diagnostics.print();
        std::process::exit(1)
    }

    let mut lowering = Lowering::new(ast,&mut diagnostics);
    let hir= lowering.lower();
    println!("{:?}",hir);
    if lowering.corrupted{
        diagnostics.print();
        std::process::exit(1)
    }

    let mut semantics = Semantics::new(hir, &mut diagnostics);
    semantics.analyze();

    Ok(())
}
