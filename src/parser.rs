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

mod env {
    use std::ops::{Deref, DerefMut};

    use crate::{
        ast::{IdentLocation, Upvalue},
        util::nonempty_vec::NEVec,
    };

    type Scope = Vec<Local>;

    #[derive(Debug)]
    pub struct Env {
        /// A list of call frames, which is a list of blocks, each with a list
        /// of locals.
        frames: NEVec<CallFrame>,
    }

    impl Env {
        pub fn new() -> Self {
            let mut env = Env {
                frames: NEVec::default(),
            };

            crate::interperter::stub_stdlib(&mut env);
            // new scope for non-stdlib
            env.frames.last_mut().scopes.push(Vec::default());

            env
        }

        pub fn scope_depth(&self) -> usize {
            assert!(!self.frames.is_empty());
            self.frames.len() - 1
        }

        pub fn create_scope(&mut self) -> ScopeGuard<'_> {
            ScopeGuard::new(self)
        }

        pub fn new_frame(&mut self) -> FrameGuard<'_> {
            FrameGuard::new(self)
        }

        pub fn declare_local(&mut self, name: String) {
            let local = Local {
                name,
                depth: self.scope_depth(),
            };
            self.frames.last_mut().scopes.last_mut().push(local);
        }

        // Look for the most deeply-scoped local with the given name.
        pub fn resolve(&mut self, name: &str) -> Option<IdentLocation> {
            // can always subtract 1 from len b/c it is non-empty (a NEVec)
            self.resolve_with_frame(name, self.frames.len() - 1)
        }

        fn resolve_with_frame(
            // Mutable so upvalues can be recorded
            &mut self,
            name: &str,
            frame_index: usize,
        ) -> Option<IdentLocation> {
            // This was much more annoying than I thought it would be
            // <https://users.rust-lang.org/t/cant-flatten-enumerate-and-then-reverse-iterator/122931>

            let call_frame = &self.frames[frame_index];
            let stack = call_frame.scopes.iter().flatten();
            let len = stack.clone().count();
            let indexes_rev = (0..len).rev();

            let mut stack = stack.rev().zip(indexes_rev);
            if let Some((_, i)) = stack.find(|(local, _)| local.name == name) {
                eprintln!("found {name} in stack. i: {i}");
                Some(IdentLocation::Stack(i))
            } else if let Some(parent_index) = frame_index.checked_sub(1) {
                // make upvalue
                let location = self.resolve_with_frame(name, parent_index)?;
                let (location, is_local) = match location {
                    IdentLocation::Stack(i) => (i, true),
                    IdentLocation::Upvalue(i) => (i, false),
                };

                // This is the upvalue that goes in the func metadata
                let upvalue = Upvalue {
                    index: location,
                    is_local,
                };
                eprintln!("Found upvalue for {name}: {upvalue:?}");

                let current_frame = &mut self.frames[frame_index];
                let upvalue_index = current_frame.upvalues.len();
                current_frame.upvalues.push(upvalue);

                // This is the upvalue that goes into the ident
                Some(IdentLocation::Upvalue(upvalue_index))
            } else {
                None
            }
        }
    }

    #[derive(Debug, Default)]
    pub struct CallFrame {
        scopes: NEVec<Scope>,
        upvalues: Vec<Upvalue>,
    }

    #[clippy::has_significant_drop]
    pub struct ScopeGuard<'a>(&'a mut Env);
    impl<'a> ScopeGuard<'a> {
        fn new(env: &'a mut Env) -> Self {
            env.frames.last_mut().scopes.push(Vec::new());
            ScopeGuard(env)
        }
    }
    impl Deref for ScopeGuard<'_> {
        type Target = Env;

        fn deref(&self) -> &Self::Target {
            self.0
        }
    }
    impl DerefMut for ScopeGuard<'_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.0
        }
    }
    impl Drop for ScopeGuard<'_> {
        fn drop(&mut self) {
            self.frames.last_mut().scopes.pop_unchecked();
        }
    }

    #[clippy::has_significant_drop]
    pub struct FrameGuard<'a>(&'a mut Env);
    impl<'a> FrameGuard<'a> {
        fn new(env: &'a mut Env) -> Self {
            env.frames.push(CallFrame::default());
            FrameGuard(env)
        }

        // Take `self` so that upvalues must be up-to-date.
        pub fn upvalues(mut self) -> Vec<Upvalue> {
            let upvalues = self.frames.pop_unchecked().upvalues;
            // prevent destructor from double-popping
            std::mem::forget(self);
            upvalues
        }
    }
    impl Deref for FrameGuard<'_> {
        type Target = Env;

        fn deref(&self) -> &Self::Target {
            self.0
        }
    }
    impl DerefMut for FrameGuard<'_> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.0
        }
    }
    impl Drop for FrameGuard<'_> {
        fn drop(&mut self) {
            self.frames.pop_unchecked();
        }
    }

    #[derive(Debug)]
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
            // Insert the var *before* parsing body so that it can be recursive
            env.declare_local(name.clone());

            let mut env = env.new_frame();

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

            eprintln!("Upvalues for {name}: {upvalues:#?}");

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
        let target = self.parse_primary(env)?;

        if !self.matches(&TokenData::OpenParen) {
            return Ok(target);
        }

        let arguments = self.parse_arguments(Self::parse_expr, env)?;
        let target = Box::new(target);
        Ok(Expr::Call(Call { target, arguments }))
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
        let mut expr = None;
        while !self.matches(&TokenData::CloseBrace) {
            let stmt_or_expr = self.parse_stmt_or_expr(&mut env)?;
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
