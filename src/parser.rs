use crate::{
    ast::{Program, Stmt},
    lexer::{Token, TokenData},
    stream::Stream,
};

pub type Parse<T> = Result<T, Error>;

pub fn parse(tokens: Vec<Token>) -> Parse<Program> {
    let mut tokens = Stream::new(tokens);

    let mut stmts = vec![];
    while tokens.peek().is_some() {
        let stmt = stmt(&mut tokens)?;
        stmts.push(stmt);
    }
    Ok(stmts)
}

fn stmt(tokens: &mut Stream<Token>) -> Parse<Stmt> {
    if tokens.advance_if_token(TokenData::Let) {
        // let binding = binding(tokens)?;
    }
    todo!()
}

pub enum Error {}

impl Stream<Token> {
    fn advance_if_token(&mut self, expected_type: TokenData) -> bool {
        self.advance_if(|t| t.data == expected_type)
    }
}
