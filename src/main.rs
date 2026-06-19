use crate::{lexer::Lexer, parser::Parser};
use std::{env, fs};

mod lexer;
mod parser;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source.unn>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let source = fs::read_to_string(filename)?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    for token in &tokens {
        println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    println!("{:?}",ast);

    Ok(())
}
