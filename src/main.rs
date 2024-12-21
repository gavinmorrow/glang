use std::{fs, io};

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
        run(source);
    } else {
        run_repl();
    }
}

fn run_repl() {
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
