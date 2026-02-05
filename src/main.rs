mod ast;
mod lexer;
mod parser;
mod symbol_table;
mod semantic;

use lexer::Lexer;
use parser::Parser;
use ast::Declaration;
use std::env;
use std::fs;
use std::process;
use std::time::Instant;
use symbol_table::SymbolTable;
use ast::{Type, StorageClass};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} <file.c>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];

    let total_start = Instant::now();

    let read_start = Instant::now();
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };
    let read_time = read_start.elapsed();

    let lex_start = Instant::now();
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let lex_time = lex_start.elapsed();

    let parse_start = Instant::now();
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
    let parse_time = parse_start.elapsed();
    let total_time = total_start.elapsed();

    println!("\n\n\n======== AST ========");
    println!("{:#?}", ast);

    println!("\n\n\n ======== SUMMARY ========");
    println!("Total declarations: {}", ast.declarations.len());
    
    let mut func_count = 0;
    let mut var_count = 0;
    let mut struct_count = 0;
    let mut union_count = 0;
    let mut enum_count = 0;
    let mut typedef_count = 0;
    
    for decl in &ast.declarations {
        match decl {
            Declaration::Function(f) => {
                func_count += 1;
                let has_body = if f.body.is_some() { "definition" } else { "declaration" };
                println!("  [Function] {} ({} params, {})", 
                         f.name, f.params.len(), has_body);
            }
            Declaration::Variable(v) => {
                var_count += 1;
                let has_init = if v.init.is_some() { "initialized" } else { "uninitialized" };
                println!("  [Variable] {} ({})", v.name, has_init);
            }
            Declaration::Struct(s) => {
                struct_count += 1;
                let name = s.name.as_ref().map(|n| n.as_str()).unwrap_or("<anonymous>");
                println!("  [Struct] {} ({} fields)", name, s.fields.len());
            }
            Declaration::Union(u) => {
                union_count += 1;
                let name = u.name.as_ref().map(|n| n.as_str()).unwrap_or("<anonymous>");
                println!("  [Union] {} ({} fields)", name, u.fields.len());
            }
            Declaration::Enum(e) => {
                enum_count += 1;
                let name = e.name.as_ref().map(|n| n.as_str()).unwrap_or("<anonymous>");
                println!("  [Enum] {} ({} variants)", name, e.variants.len());
            }
            Declaration::Typedef(t) => {
                typedef_count += 1;
                println!("  [Typedef] {}", t.name);
            }
        }
    }
    
    println!("\n\n\n======== DECLARATION COUNTS ========");
    println!("Functions: {}", func_count);
    println!("Variables: {}", var_count);
    println!("Structs:   {}", struct_count);
    println!("Unions:    {}", union_count);
    println!("Enums:     {}", enum_count);
    println!("Typedefs:  {}", typedef_count);

    println!("\n\n\n======== TIMINGS ========");
    println!("File reading: {:?}", read_time);
    println!("Lexing:       {:?}", lex_time);
    println!("Parsing:      {:?}", parse_time);
    println!("Total time:   {:?}", total_time);
    
    let total_micros = total_time.as_micros() as f64;
    if total_micros > 0.0 {
        println!("\nbreakdown:");
        println!("  Lexing:  {:.1}%", (lex_time.as_micros() as f64 / total_micros) * 100.0);
        println!("  Parsing: {:.1}%", (parse_time.as_micros() as f64 / total_micros) * 100.0);
    }
}