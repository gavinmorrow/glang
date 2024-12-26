mod env;

pub use env::Env;

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, BindingMetadata, Block, Call, ElseBlock, Expr, Identifier,
        IfExpr, Pattern, Program, Stmt, UnaryExpr, UnaryOp,
    },
    lexer::{Pos, Token, TokenData},
    stream::Stream,
};

/// A ancestor of `parent_scope` must include the stdlib.
pub fn parse(tokens: Vec<Token>, env: &mut Env) -> Parse<Program> {
    Parser::new(tokens).parse_program(env)
}

pub type Parse<T> = Result<T, Error>;

struct Parser {
    tokens: Stream<Token>,
}

impl Parser {
    /// A ancestor of `parent_scope` must include the stdlib.
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: Stream::new(tokens),
        }
    }
}

impl Parser {
    fn parse_program(&mut self, env: &mut Env) -> Parse<Program> {
        let mut stmts = vec![];
        while self.tokens.peek().is_some() {
            let stmt = self.parse_stmt(env)?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self, env: &mut Env) -> Parse<Stmt> {
        match self.parse_stmt_or_expr(env)? {
            StmtOrExpr::Stmt(stmt) => Ok(stmt),
            StmtOrExpr::Expr(_) => Err(Error {
                pos: self.tokens.peek().map(|t| t.pos),
                kind: ErrorKind::ExpectedStmt,
            }),
        }
    }

    // This is done weirdly to allow for blocks to end in an expr easily
    fn parse_stmt_or_expr(&mut self, env: &mut Env) -> Parse<StmtOrExpr> {
        if self.matches(&TokenData::Let) {
            let binding = self.parse_binding(env)?;
            self.expect(TokenData::Semicolon)?;
            Ok(StmtOrExpr::Stmt(Stmt::Let(binding)))
        } else {
            let expr = self.parse_expr(env)?;
            if self.expect(TokenData::Semicolon).is_ok() {
                Ok(StmtOrExpr::Stmt(Stmt::Expr(expr)))
            } else {
                Ok(StmtOrExpr::Expr(expr))
            }
        }
    }

    fn parse_binding(&mut self, env: &mut Env) -> Parse<Binding> {
        let pattern = self.parse_pattern(env)?;
        let name = pattern.0.name.clone();

        let is_func = self.matches(&TokenData::OpenParen);

        if is_func {
            env.declare_local(name.clone());
            let mut env = env.new_frame(name.clone());

            let arguments = self.parse_arguments(
                |parser, env| -> Parse<Pattern> {
                    let pattern = parser.parse_pattern(env)?;
                    env.declare_local(pattern.0.name.clone());
                    Ok(pattern)
                },
                &mut env,
            )?;

            self.expect(TokenData::Equals)?;

            let value = self.parse_expr(&mut env)?;
            let upvalues = env.upvalues();

            Ok(Binding {
                pattern,
                metadata: BindingMetadata::Func {
                    arguments,
                    upvalues,
                },
                value,
            })
        } else {
            self.expect(TokenData::Equals)?;

            // Insert the var *after* parsing value so that shadowing works
            let value = self.parse_expr(env).inspect(|_| env.declare_local(name))?;

            Ok(Binding {
                pattern,
                metadata: BindingMetadata::Var,
                value,
            })
        }
    }

    fn parse_pattern(&mut self, env: &mut Env) -> Parse<Pattern> {
        let identifier = self.parse_identifier(env)?;
        Ok(Pattern(identifier))
    }

    fn parse_expr(&mut self, env: &mut Env) -> Parse<Expr> {
        let binary_expr = self.parse_logic_or(env)?;
        Ok(binary_expr)
    }

    fn parse_binary_expr(
        &mut self,
        mut parse_operand: impl FnMut(&mut Parser, &mut Env) -> Parse<Expr>,
        env: &mut Env,
        parse_operator: impl Fn(&Token) -> Option<BinaryOp>,
    ) -> Parse<Expr> {
        let mut lhs = parse_operand(self, env)?;

        while let Some(op) = self.tokens.next_if_map(&parse_operator) {
            let rhs = parse_operand(self, env)?;
            lhs = Expr::Binary(Box::new(BinaryExpr { lhs, op, rhs }));
        }

        Ok(lhs)
    }

    fn parse_logic_or(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_logic_and, env, |t| match t.data {
            TokenData::Or => Some(BinaryOp::Or),
            _ => None,
        })
    }

    fn parse_logic_and(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_equality, env, |t| match t.data {
            TokenData::And => Some(BinaryOp::And),
            _ => None,
        })
    }

    fn parse_equality(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_comparison, env, |t| match t.data {
            TokenData::BangEquals => Some(BinaryOp::NotEq),
            TokenData::EqualsEquals => Some(BinaryOp::Eq),
            _ => None,
        })
    }

    fn parse_comparison(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_term, env, |t| match t.data {
            TokenData::Less => Some(BinaryOp::Less),
            TokenData::LessEquals => Some(BinaryOp::LessEq),
            TokenData::Greater => Some(BinaryOp::Greater),
            TokenData::GreaterEquals => Some(BinaryOp::GreaterEq),
            _ => None,
        })
    }

    fn parse_term(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_factor, env, |t| match t.data {
            TokenData::Minus => Some(BinaryOp::Subtract),
            TokenData::Plus => Some(BinaryOp::Add),
            _ => None,
        })
    }

    fn parse_factor(&mut self, env: &mut Env) -> Parse<Expr> {
        self.parse_binary_expr(Self::parse_unary, env, |t| match t.data {
            TokenData::Slash => Some(BinaryOp::Divide),
            TokenData::Star => Some(BinaryOp::Multiply),
            _ => None,
        })
    }

    fn parse_unary(&mut self, env: &mut Env) -> Parse<Expr> {
        let op = self.tokens.next_if_map(|t| match t.data {
            TokenData::Bang => Some(UnaryOp::Not),
            TokenData::Minus => Some(UnaryOp::Negate),
            _ => None,
        });

        if let Some(op) = op {
            let rhs = self.parse_unary(env)?;
            Ok(Expr::Unary(Box::new(UnaryExpr { op, rhs })))
        } else {
            self.parse_call(env)
        }
    }

    fn parse_call(&mut self, env: &mut Env) -> Parse<Expr> {
        let mut target = self.parse_primary(env)?;

        while self.matches(&TokenData::OpenParen) {
            let arguments = self.parse_arguments(Self::parse_expr, env)?;
            target = Expr::Call(Call {
                target: Box::new(target),
                arguments,
            });
        }

        Ok(target)
    }

    fn parse_arguments<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser, &mut Env) -> Parse<T>,
        env: &mut Env,
    ) -> Parse<Vec<T>> {
        if self.matches(&TokenData::CloseParen) {
            return Ok(vec![]);
        }

        // There is at least one argument
        let mut arguments = vec![];
        loop {
            let arg = parse_arg(self, env)?;
            arguments.push(arg);

            if self.matches(&TokenData::CloseParen) {
                break;
            } else {
                self.expect(TokenData::Comma)?;
            }
        }
        Ok(arguments)
    }

    fn parse_primary(&mut self, env: &mut Env) -> Parse<Expr> {
        use crate::ast::Literal::{Bool, Nil, Number, Str};
        use Expr::Literal;
        if self.matches(&TokenData::OpenBrace) {
            let block = self.parse_block(env)?;
            Ok(Expr::Block(block))
        } else if self.matches(&TokenData::If) {
            let if_expr = self.parse_if_expr(env)?;
            Ok(Expr::If(Box::new(if_expr)))
        } else {
            self.consume_map(
                |t| {
                    Some(Ok(match &t.data {
                        TokenData::True => Literal(Bool(true)),
                        TokenData::False => Literal(Bool(false)),
                        TokenData::Number(n) => Literal(Number(*n)),
                        TokenData::Str(s) => Literal(Str(s.clone())),
                        TokenData::Nil => Literal(Nil),
                        TokenData::Identifier(name) => {
                            let location = match env.resolve(name) {
                                Some(l) => l,
                                None => {
                                    return Some(Err(Error {
                                        pos: Some(t.pos),
                                        kind: ErrorKind::VarNotInScope {
                                            identifier: Identifier::new(name.clone()),
                                        },
                                    }))
                                }
                            };
                            let identifier = Identifier::new(name.clone()).resolve(location);
                            return Some(Ok(Expr::Identifier(identifier)));
                        }
                        _ => return None,
                    }))
                },
                ErrorKind::ExpectedPrimary,
            )?
        }
    }

    fn parse_block(&mut self, env: &mut Env) -> Parse<Block> {
        let mut env = env.create_scope();

        let mut stmts = vec![];
        let mut return_expr = None;
        while !self.matches(&TokenData::CloseBrace) {
            let stmt_or_expr = self.parse_stmt_or_expr(&mut env)?;
            match stmt_or_expr {
                StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                StmtOrExpr::Expr(e) => {
                    return_expr = Some(Box::new(e));
                    self.expect(TokenData::CloseBrace)?;
                    break;
                }
            }
        }

        Ok(Block { stmts, return_expr })
    }

    fn parse_if_expr(&mut self, env: &mut Env) -> Parse<IfExpr> {
        let condition = self.parse_expr(env)?;
        self.expect(TokenData::OpenBrace)?;
        let then_block = self.parse_block(env)?;

        let else_block = if self.matches(&TokenData::Else) {
            Some(if self.matches(&TokenData::If) {
                ElseBlock::ElseIf(Box::new(self.parse_if_expr(env)?))
            } else {
                self.expect(TokenData::OpenBrace)?;
                ElseBlock::Else(self.parse_block(env)?)
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

    fn parse_identifier(&mut self, _env: &mut Env) -> Parse<Identifier> {
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

    fn consume_map<U>(&mut self, f: impl FnOnce(&Token) -> Option<U>, err: ErrorKind) -> Parse<U> {
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
