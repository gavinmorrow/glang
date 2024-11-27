use crate::{
    ast::{Binding, Expr, Identifier, Pattern, Program, Stmt},
    lexer::{Token, TokenData},
    stream::Stream,
};

pub type Parse<T> = Result<T, Error>;

struct Parser {
    tokens: Stream<Token>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: Stream::new(tokens),
        }
    }
}

impl Parser {
    fn parse_program(&mut self) -> Parse<Program> {
        let mut stmts = vec![];
        while self.tokens.peek().is_some() {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Parse<Stmt> {
        if self.matches(&TokenData::Let) {
            let binding = self.parse_binding()?;
            self.expect(TokenData::Semicolon)?;
            Ok(Stmt::Let(binding))
        } else {
            let expr = self.parse_expr()?;
            self.expect(TokenData::Semicolon)?;
            Ok(Stmt::Expr(expr))
        }
    }

    fn parse_binding(&mut self) -> Parse<Binding> {
        let pattern = self.parse_pattern()?;
        self.expect(TokenData::Equals)?;
        let value = self.parse_expr()?;

        Ok(Binding { pattern, value })
    }

    fn parse_pattern(&mut self) -> Parse<Pattern> {
        let identifier = self.parse_identifier()?;
        Ok(Pattern(identifier))
    }

    fn parse_expr(&mut self) -> Parse<Expr> {
        todo!()
    }

    fn parse_identifier(&mut self) -> Parse<Identifier> {
        let Some(next) = self.tokens.peek() else {
            return Err(Error::ExpectedIdentifier);
        };
        let TokenData::Identifier(name) = &next.data else {
            return Err(Error::ExpectedIdentifier);
        };
        let name = name.clone();
        Ok(Identifier { name })
    }
}

impl Parser {
    fn matches(&mut self, expected_type: &TokenData) -> bool {
        self.tokens.advance_if(|t| &t.data == expected_type)
    }

    fn consume(&mut self, f: impl Fn(&Token) -> bool, err: Error) -> Parse<Token> {
        self.tokens.next_if(f).ok_or(err)
    }

    fn expect(&mut self, expected_type: TokenData) -> Parse<()> {
        if self.matches(&expected_type) {
            Ok(())
        } else {
            Err(Error::ExpectedToken(expected_type))
        }
    }
}

fn stmt(tokens: &mut Stream<Token>) -> Parse<Stmt> {
    if tokens.advance_if_token(TokenData::Let) {
        // let binding = binding(tokens)?;
    }
    todo!()
}

pub enum Error {
    ExpectedToken(TokenData),
    ExpectedIdentifier,
}

impl Stream<Token> {
    fn advance_if_token(&mut self, expected_type: TokenData) -> bool {
        self.advance_if(|t| t.data == expected_type)
    }
}
