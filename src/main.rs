use std::{fs, io};

mod ast;
mod interperter;
mod lexer;
mod parser;
mod stream;
mod util;

fn main() {
    let mut args = std::env::args();

    let _ = args.next();
    if let Some(path) = args.next() {
        let source = fs::read_to_string(path).expect("source file is readable");
        let mut env = (parser::Env::new(), interperter::Env::new());
        run(source, &mut env);
    } else {
        run_repl();
    }
}

fn run_repl() {
    // start repl
    eprintln!("glang v0.1.0");
    let mut env = (parser::Env::new(), interperter::Env::new());
    eprint!("> ");
    while let Some(Ok(line)) = io::stdin().lines().next() {
        run(line, &mut env);
        eprint!("> ");
    }
    eprintln!("Goodbye! o/");
}

fn run(source: String, env: &mut (parser::Env, interperter::Env)) {
    let tokens = lexer::lex(source);
    // println!("{tokens:#?}");

    let ast = parser::parse(tokens, &mut env.0);
    // dbg!(&ast);

    match ast {
        Ok(ast) => {
            let res = interperter::interpert(ast, &mut env.1);
            println!("{res:#?}");
        }
        Err(err) => println!("Error while parsing AST: {err:#?}"),
    }
}
