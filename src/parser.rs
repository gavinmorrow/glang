use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, Call, Expr, Identifier, Pattern, Program, Stmt, UnaryExpr,
        UnaryOp,
    },
    lexer::{Token, TokenData},
    stream::Stream,
};

pub fn parse(tokens: Vec<Token>) -> Parse<Program> {
    Parser::new(tokens).parse_program()
}

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
        let binary_expr = self.parse_logic_or()?;
        Ok(binary_expr)
    }

    fn parse_binary_expr(
        &mut self,
        mut parse_operand: impl FnMut(&mut Parser) -> Parse<Expr>,
        parse_operator: impl Fn(&Token) -> Option<BinaryOp>,
    ) -> Parse<Expr> {
        let mut lhs = parse_operand(self)?;

        while let Some(op) = self.tokens.next_if_map(&parse_operator) {
            let rhs = parse_operand(self)?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, op, rhs }));
        }

        Ok(lhs)
    }

    fn parse_logic_or(&mut self) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_logic_and, |t| match t.data {
            TokenData::Or => Some(BinaryOp::Or),
            _ => None,
        })
    }

    fn parse_logic_and(&mut self) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_equality, |t| match t.data {
            TokenData::And => Some(BinaryOp::And),
            _ => None,
        })
    }

    fn parse_equality(&mut self) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_term, |t| match t.data {
            TokenData::Less => Some(BinaryOp::Less),
            TokenData::LessEquals => Some(BinaryOp::LessEq),
            TokenData::Greater => Some(BinaryOp::Greater),
            TokenData::GreaterEquals => Some(BinaryOp::GreaterEq),
            _ => None,
        })
    }

    fn parse_term(&mut self) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_factor, |t| match t.data {
            TokenData::Minus => Some(BinaryOp::Subtract),
            TokenData::Plus => Some(BinaryOp::Add),
            _ => None,
        })
    }

    fn parse_factor(&mut self) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_unary, |t| match t.data {
            TokenData::Slash => Some(BinaryOp::Divide),
            TokenData::Star => Some(BinaryOp::Multiply),
            _ => None,
        })
    }

    fn parse_unary(&mut self) -> Parse<Expr> {
        let op = self.tokens.next_if_map(|t| match t.data {
            TokenData::Bang => Some(UnaryOp::Not),
            TokenData::Minus => Some(UnaryOp::Negate),
            _ => None,
        });

        if let Some(op) = op {
            let rhs = self.parse_unary()?;
            Ok(Expr::Unary(Box::new(UnaryExpr { op, rhs })))
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Parse<Expr> {
        let target = self.parse_primary()?;

        let mut arguments = vec![];
        while let Ok(arg) = self.parse_expr() {
            arguments.push(arg);
        }

        if arguments.is_empty() {
            Ok(target)
        } else {
            let target = Box::new(target);
            Ok(Expr::Call(Call { target, arguments }))
        }
    }

    fn parse_primary(&mut self) -> Parse<Expr> {
        use crate::ast::Literal::{Bool, Number, Str};
        use Expr::Literal;
        self.tokens
            .next_if_map(|t| match &t.data {
                TokenData::True => Some(Literal(Bool(true))),
                TokenData::False => Some(Literal(Bool(false))),
                TokenData::Number(n) => Some(Literal(Number(*n))),
                TokenData::Str(s) => Some(Literal(Str(s.clone()))),
                _ => None,
            })
            .ok_or(Error::ExpectedPrimary)
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

#[allow(
    clippy::enum_variant_names,
    reason = "Not repeating the enum name, and adds important context."
)]
#[derive(Debug)]
pub enum Error {
    ExpectedToken(TokenData),
    ExpectedIdentifier,
    ExpectedPrimary,
    ExpectedUnary,
}

impl Stream<Token> {
    fn advance_if_token(&mut self, expected_type: TokenData) -> bool {
        self.advance_if(|t| t.data == expected_type)
    }
}
