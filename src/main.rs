#![expect(dead_code, reason = "prototyping")]

use std::io;

mod ast;
mod interperter;
mod lexer;
mod parser;
mod stream;

fn main() {
    // start repl
    eprintln!("glang v0.1.0");
    eprint!("> ");
    while let Some(Ok(line)) = io::stdin().lines().next() {
        run(line);
        eprint!("> ");
    }
    eprintln!("Goodbye! o/");
}

fn run(source: String) {
    let tokens = lexer::lex(source);
    // println!("{tokens:#?}");

    let ast = parser::parse(tokens);
    // println!("{ast:#?}");

    match ast {
        Ok(ast) => {
            let res = interperter::interpert(ast);
            println!("{res:#?}");
        }
        Err(err) => println!("Error while parsing AST: {err:#?}"),
    }
}
