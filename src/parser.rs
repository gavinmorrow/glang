use env::Env;

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Pattern,
        Program, Stmt, UnaryExpr, UnaryOp,
    },
    lexer::{Pos, Token, TokenData},
    stream::Stream,
};

/// A ancestor of `parent_scope` must include the stdlib.
pub fn parse(tokens: Vec<Token>) -> Parse<Program> {
    Parser::new(tokens).parse_program()
}

pub type Parse<T> = Result<T, Error>;

struct Parser {
    tokens: Stream<Token>,
    env: Env,
}

mod env {
    use std::ops::Deref;

    pub struct Env {
        locals: Vec<Vec<Local>>,
    }

    impl Env {
        pub fn new() -> Self {
            Env {
                locals: vec![Vec::new()],
            }
        }

        pub fn scope_depth(&self) -> usize {
            self.locals.len()
        }

        pub fn create_scope(&mut self) -> ScopeGuard<'_> {
            ScopeGuard::new(self)
        }

        pub fn declare_local(&mut self, name: String) {
            let local = Local {
                name,
                depth: self.scope_depth(),
            };
            self.locals
                .last_mut()
                .expect("scope should exist")
                .push(local);
        }
    }

    pub struct ScopeGuard<'a>(&'a mut Env);
    impl<'a> ScopeGuard<'a> {
        fn new(env: &'a mut Env) -> Self {
            env.locals.push(Vec::new());
            ScopeGuard(env)
        }
    }
    impl Deref for ScopeGuard<'_> {
        type Target = Env;

        fn deref(&self) -> &Self::Target {
            self.0
        }
    }
    impl Drop for ScopeGuard<'_> {
        fn drop(&mut self) {
            self.0.locals.pop().expect("scope should exist");
        }
    }

    struct Local {
        name: String,
        depth: usize,
    }
}

impl Parser {
    /// A ancestor of `parent_scope` must include the stdlib.
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: Stream::new(tokens),
            env: Env::new(),
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
        match self.parse_stmt_or_expr()? {
            StmtOrExpr::Stmt(stmt) => Ok(stmt),
            StmtOrExpr::Expr(_) => Err(Error {
                pos: self.tokens.peek().map(|t| t.pos),
                kind: ErrorKind::ExpectedStmt,
            }),
        }
    }

    // This is done weirdly to allow for blocks to end in an expr easily
    fn parse_stmt_or_expr(&mut self) -> Parse<StmtOrExpr> {
        if self.matches(&TokenData::Let) {
            let binding = self.parse_binding()?;
            self.expect(TokenData::Semicolon)?;
            Ok(StmtOrExpr::Stmt(Stmt::Let(binding)))
        } else {
            let expr = self.parse_expr()?;
            if self.expect(TokenData::Semicolon).is_ok() {
                Ok(StmtOrExpr::Stmt(Stmt::Expr(expr)))
            } else {
                Ok(StmtOrExpr::Expr(expr))
            }
        }
    }

    fn parse_binding(&mut self) -> Parse<Binding> {
        let pattern = self.parse_pattern()?;

        let arguments = if self.matches(&TokenData::OpenParen) {
            Some(self.parse_arguments(|parser| -> Parse<Pattern> {
                let pattern = parser.parse_pattern()?;
                todo!("define arg var");
                Ok(pattern)
            })?)
        } else {
            None
        };

        self.expect(TokenData::Equals)?;

        let is_func = arguments.is_some();
        let name = pattern.0.name.clone();
        let value = if is_func {
            // Insert the var *before* so that the function can be recursive
            self.env.declare_local(name);
            self.parse_expr()?
        } else {
            // Insert the var *after* so that variable shadowing works
            self.parse_expr()
                .inspect(|_| self.env.declare_local(name))?
        };

        Ok(Binding {
            pattern,
            arguments,
            value,
        })
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
        self.parse_binary_expr(Self::parse_comparison, |t| match t.data {
            TokenData::BangEquals => Some(BinaryOp::NotEq),
            TokenData::EqualsEquals => Some(BinaryOp::Eq),
            _ => None,
        })
    }

    fn parse_comparison(&mut self) -> Parse<Expr> {
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

        if !self.matches(&TokenData::OpenParen) {
            return Ok(target);
        }

        let arguments = self.parse_arguments(Self::parse_expr)?;
        let target = Box::new(target);
        Ok(Expr::Call(Call { target, arguments }))
    }

    fn parse_arguments<T>(&mut self, parse_arg: impl Fn(&mut Parser) -> Parse<T>) -> Parse<Vec<T>> {
        if self.matches(&TokenData::CloseParen) {
            return Ok(vec![]);
        }

        // There is at least one argument
        let mut arguments = vec![];
        loop {
            let arg = parse_arg(self)?;
            arguments.push(arg);

            if self.matches(&TokenData::CloseParen) {
                break;
            } else {
                self.expect(TokenData::Comma)?;
            }
        }
        Ok(arguments)
    }

    fn parse_primary(&mut self) -> Parse<Expr> {
        use crate::ast::Literal::{Bool, Nil, Number, Str};
        use Expr::Literal;
        if self.matches(&TokenData::OpenBrace) {
            let block = self.parse_block()?;
            Ok(Expr::Block(block))
        } else if self.matches(&TokenData::If) {
            let if_expr = self.parse_if_expr()?;
            Ok(Expr::If(Box::new(if_expr)))
        } else {
            self.consume_map(
                |t| match &t.data {
                    TokenData::True => Some(Literal(Bool(true))),
                    TokenData::False => Some(Literal(Bool(false))),
                    TokenData::Number(n) => Some(Literal(Number(*n))),
                    TokenData::Str(s) => Some(Literal(Str(s.clone()))),
                    TokenData::Nil => Some(Literal(Nil)),
                    TokenData::Identifier(name) => {
                        Some(Expr::Identifier(Identifier::new(name.clone())))
                    }
                    _ => None,
                },
                ErrorKind::ExpectedPrimary,
            )
        }
    }

    fn parse_block(&mut self) -> Parse<Block> {
        let mut stmts = vec![];
        let mut expr = None;
        while !self.matches(&TokenData::CloseBrace) {
            let stmt_or_expr = self.parse_stmt_or_expr()?;
            match stmt_or_expr {
                StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                StmtOrExpr::Expr(e) => {
                    expr = Some(Box::new(e));
                    self.expect(TokenData::CloseBrace)?;
                    break;
                }
            }
        }

        Ok(Block(stmts, expr))
    }

    fn parse_if_expr(&mut self) -> Parse<IfExpr> {
        let condition = self.parse_expr()?;
        self.expect(TokenData::OpenBrace)?;
        let then_block = self.parse_block()?;

        let else_block = if self.matches(&TokenData::Else) {
            Some(if self.matches(&TokenData::If) {
                ElseBlock::ElseIf(Box::new(self.parse_if_expr()?))
            } else {
                self.expect(TokenData::OpenBrace)?;
                ElseBlock::Else(self.parse_block()?)
            })
        } else {
            None
        };

        Ok(IfExpr {
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_identifier(&mut self) -> Parse<Identifier> {
        let name = self.consume_map(
            |t| match &t.data {
                TokenData::Identifier(name) => Some(name.clone()),
                _ => None,
            },
            ErrorKind::ExpectedIdentifier,
        )?;
        Ok(Identifier::new(name))
    }
}

impl Parser {
    fn matches(&mut self, expected_type: &TokenData) -> bool {
        self.tokens.advance_if(|t| &t.data == expected_type)
    }

    fn consume_map<U>(&mut self, f: impl Fn(&Token) -> Option<U>, err: ErrorKind) -> Parse<U> {
        self.tokens.next_if_map(f).ok_or_else(|| Error {
            pos: self.tokens.peek().map(|t| t.pos),
            kind: err,
        })
    }

    fn expect(&mut self, expected_type: TokenData) -> Parse<()> {
        if self.matches(&expected_type) {
            Ok(())
        } else {
            Err(Error {
                pos: self.tokens.peek().map(|t| t.pos),
                kind: ErrorKind::ExpectedToken(expected_type),
            })
        }
    }
}

#[derive(Clone, Debug)]
enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}

#[expect(dead_code, reason = "Pretty error printing not implemented yet")]
#[derive(Debug)]
pub struct Error {
    pos: Option<Pos>,
    kind: ErrorKind,
}
#[expect(dead_code, reason = "Pretty error printing not implemented yet")]
#[allow(
    clippy::enum_variant_names,
    reason = "Not repeating the enum name, and adds important context."
)]
#[derive(Debug)]
pub enum ErrorKind {
    ExpectedToken(TokenData),
    ExpectedIdentifier,
    ExpectedPrimary,
    ExpectedUnary,
    ExpectedStmt,
    VarNotInScope { identifier: Identifier },
}
