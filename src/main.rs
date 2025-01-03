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
    eprintln!("qua v0.1.0");
    let mut env = (parser::Env::new(), interperter::Env::new());
    eprint!("> ");
    while let Some(Ok(line)) = io::stdin().lines().next() {
        run(line, &mut env);
        eprint!("> ");
    }
    eprintln!("Goodbye! o/");
}

fn run(source: String, env: &mut (parser::Env, interperter::Env)) {
    let tokens = lexer::lex(source.clone());
    // println!("{tokens:#?}");

    let ast = parser::parse(tokens, &mut env.0);
    // dbg!(&ast);

    match ast {
        Ok(ast) => {
            let res = interperter::interpert(ast, &mut env.1);
            if let Err(err) = res {
                let msg = display_error(err, source);
                eprintln!("{msg}");
            }
        }
        Err(err) => println!("Error while parsing AST: {err:#?}"),
    }
}

fn display_error(err: interperter::Error, source: String) -> String {
    format!(
        "Error{}: {:#?}",
        if let Some(pos) = err.pos {
            let (line, col) = pos.calculate_line_col(&source);
            format!(" at {line}:{col}")
        } else {
            "".to_string()
        },
        err.kind
    )
}

impl lexer::Pos {
    /// (line, col)
    ///
    /// e.g.
    /// abc
    /// def
    /// f -> (2, 3)
    fn calculate_line_col(&self, source: &str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 0;
        for c in source.chars().take(self.0) {
            col += 1;
            if c == '\n' {
                line += 1;
                col = 0;
            }
        }
        (line, col)
    }
}
