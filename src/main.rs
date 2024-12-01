use std::io;

use interperter::env::{Environment, Scope};

mod ast;
mod interperter;
mod lexer;
mod parser;
mod stream;

fn main() {
    // start repl
    let (mut env, scope) = Environment::new_root();

    eprintln!("glang v0.1.0");
    eprint!("> ");
    while let Some(Ok(line)) = io::stdin().lines().next() {
        run(line, &mut env, scope.clone());
        eprint!("> ");
    }
    eprintln!("Goodbye! o/");
}

fn run(source: String, env: &mut Environment, scope: Scope) {
    let tokens = lexer::lex(source);
    // println!("{tokens:#?}");

    let ast = parser::parse(tokens);
    // println!("{ast:#?}");

    match ast {
        Ok(ast) => {
            eprintln!("ast: {ast:#?}");
            let res = interperter::interpert(ast, env, scope);
            println!("{res:#?}");
        }
        Err(err) => println!("Error while parsing AST: {err:#?}"),
    }
}
