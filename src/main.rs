use std::{fs, io};

use ast::Scope;
use interperter::env::Environment;

mod ast;
mod interperter;
mod lexer;
mod parser;
mod stream;

fn main() {
    let mut args = std::env::args();

    let _ = args.next();
    if let Some(path) = args.next() {
        let source = fs::read_to_string(path).expect("source file is readable");
        let (mut env, stdlib_scope) = Environment::new_root();
        run(source, &mut env, None, stdlib_scope);
    } else {
        run_repl();
    }
}

fn run_repl() {
    // start repl
    let (mut env, stdlib_scope) = Environment::new_root();
    // FIXME: this doesn't work. the stuff is in scope in the dynamic env, but
    // the parser doesn't know it bc it's not in scope in the static env.
    // (needs more general solution than just stdlib, maybe preload others too)
    // Provide a persistent scope
    let scope = stdlib_scope.nest();

    eprintln!("glang v0.1.0");
    eprint!("> ");
    while let Some(Ok(line)) = io::stdin().lines().next() {
        run(line, &mut env, Some(scope.clone()), stdlib_scope.clone());
        eprint!("> ");
    }
    eprintln!("Goodbye! o/");
}

fn run(source: String, env: &mut Environment, scope: Option<Scope>, stdlib_scope: Scope) {
    let tokens = lexer::lex(source);
    // println!("{tokens:#?}");

    let ast = parser::parse(tokens, scope, stdlib_scope);
    // println!("{ast:#?}");

    match ast {
        Ok(ast) => {
            let res = interperter::interpert(ast, env);
            println!("{res:#?}");
        }
        Err(err) => println!("Error while parsing AST: {err:#?}"),
    }
}
