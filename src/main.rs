mod ast;
mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.c>", args[0]);
        eprintln!("Example: cargo run -- test.c");
        process::exit(1);
    }

    let filename = &args[1];

    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}", e);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        parser.parse_program()
    })) {
        Ok(ast) => ast,
        Err(_) => {
            eprintln!("\nParsing failed!");
            process::exit(1);
        }
    };

    println!("\n\n\n\n\n======== AST=======");
    println!("{:#?}", ast);

    println!("\n\n\n\n===AST Summary===");
    println!("Structs defined: {}", ast.structs.len());
    for s in &ast.structs {
        println!("  - struct {} (fields: {})", s.name, s.fields.len());
    }

    println!("\nFunctions defined: {}", ast.functions.len());
    for f in &ast.functions {
        let return_type = match &f.return_type {
            Some(t) => format!("{:?}", t),
            None => "void".to_string(),
        };
        println!("  - {} {}({} params, {} statements)",
                 return_type, f.name, f.params.len(), f.body.len());
    }
}