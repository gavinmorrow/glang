mod stdlib;

pub use env::Env;
pub use stdlib::stub_stdlib;

use crate::ast::{
    BinaryExpr, BinaryOp, Binding, BindingMetadata, Block, Call, ElseBlock, Expr, IdentLocation,
    Identifier, IfExpr, Literal, Pattern, Program, Stmt, UnaryExpr, UnaryOp,
};

pub fn interpert(program: Program, env: &mut Env) -> Result<Value> {
    program.evaluate(env)
}

mod env {
    use std::ops::{Deref, DerefMut};

    use crate::ast::{IdentLocation, StackIndex, Upvalue, UpvalueIndex};

    use super::{Func, Value};

    pub struct Env {
        locals_stack: Vec<Value>,
        call_frames: Vec<CallFrame>,
    }

    impl Env {
        pub fn new() -> Self {
            let mut env = Env {
                locals_stack: Vec::new(),
                call_frames: Vec::new(),
            };
            super::stdlib::define_stdlib(&mut env);
            env
        }

        pub fn get(&self, i: IdentLocation) -> &Value {
            eprintln!(
                "getting from env {i:?} offset {}",
                self.call_frames.last().map(|f| f.stack_offset).unwrap_or(0)
            );
            match i {
                IdentLocation::Stack(i) => self.get_local_from_frame(i, self.call_frames.last()),

                IdentLocation::Upvalue(upvalue_index) => {
                    if let Some(frame_index) = (self.call_frames.len()).checked_sub(1) {
                        let frame = &self.call_frames[frame_index];

                        let Func::User(func) = &frame.func else {
                            panic!("call frame should not be native func");
                        };

                        &func.upvalues[upvalue_index.0]
                    } else {
                        // a global
                        eprintln!("getting global {upvalue_index:?}");
                        &self.locals_stack[upvalue_index.0]
                    }
                }
            }
        }

        pub fn resolve_upvalue(&self, upvalue: Upvalue) -> &Value {
            let current_frame = self.call_frames.last();
            eprintln!(
                "resolving arg {upvalue:?} in frame {}",
                current_frame.map(|f| f.stack_offset).unwrap_or(0)
            );
            match upvalue.target {
                IdentLocation::Stack(stack_index) => {
                    self.get_local_from_frame(stack_index, current_frame)
                }
                IdentLocation::Upvalue(upvalue_index) => {
                    self.get_upvalue_from_frame(upvalue_index, current_frame)
                }
            }
        }

        fn get_local_from_frame(
            &self,
            StackIndex(i): StackIndex,
            frame: Option<&CallFrame>,
        ) -> &Value {
            let stack_offset = frame.map(|f| f.stack_offset).unwrap_or(0);
            self.locals_stack.get(stack_offset + i).unwrap_or_else(|| {
                panic!(
                    "stack location should be valid. i: {i}, offset: {}, stack: {:#?}",
                    stack_offset, self.locals_stack
                )
            })
        }

        fn get_upvalue_from_frame<'f>(
            &self,
            UpvalueIndex(i): UpvalueIndex,
            frame: Option<&'f CallFrame>,
        ) -> &'f Value {
            let Func::User(func) = &frame.expect("frame should exist for upvalue").func else {
                panic!("call frame should not be native func");
            };
            &func.upvalues[i]
        }

        pub fn define(&mut self, value: Value) {
            self.locals_stack.push(value);
        }

        pub fn new_frame(&mut self, func: Func) -> FrameGuard {
            FrameGuard::new(self, func)
        }
    }

    #[derive(Debug)]
    pub struct CallFrame {
        func: Func,
        stack_offset: usize,
    }

    #[clippy::has_significant_drop]
    pub struct FrameGuard<'a>(&'a mut Env);
    impl<'a> FrameGuard<'a> {
        fn new(env: &'a mut Env, func: Func) -> Self {
            let stack_offset = env.locals_stack.len();
            let call_frame = CallFrame { func, stack_offset };
            env.call_frames.push(call_frame);

            FrameGuard(env)
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
            let offset = self
                .call_frames
                .last()
                .expect("env should have call frame")
                .stack_offset;
            self.locals_stack.drain(offset..);

            self.call_frames
                .pop()
                .expect("env should have call frame to pop");
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

trait Evaluate {
    fn evaluate(&self, env: &mut Env) -> Result<Value>;
}

impl Evaluate for Program {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        for stmt in self {
            stmt.evaluate(env)?;
        }
        Ok(Value::Nil)
    }
}

impl Evaluate for Stmt {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        match self {
            Stmt::Let(binding) => binding.evaluate(env),
            Stmt::Expr(expr) => expr.evaluate(env),
        }
    }
}

impl Evaluate for Binding {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        let identifier = self.pattern.0.clone();

        let value = match &self.metadata {
            // Don't `.evaluate()` anything for a function
            BindingMetadata::Func {
                arguments,
                upvalues,
            } => {
                let arguments = arguments.clone();
                let upvalues = upvalues
                    .iter()
                    .map(|upvalue| env.resolve_upvalue(upvalue.clone()).clone())
                    .collect();
                let body = self.value.clone();
                Value::Func(Func::User(UserFunc {
                    arguments,
                    upvalues,
                    body,
                }))
            }
            // But do for a variable
            BindingMetadata::Var => self.value.evaluate(env)?,
        };
        env.define(value);
        Ok(Value::Nil)
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        match self {
            Expr::Block(block) => block.evaluate(env),
            Expr::Call(call) => call.evaluate(env),
            Expr::If(if_expr) => if_expr.evaluate(env),
            Expr::Binary(binary_expr) => binary_expr.evaluate(env),
            Expr::Unary(unary_expr) => unary_expr.evaluate(env),
            Expr::Literal(literal) => literal.evaluate(env),
            Expr::Identifier(identifier) => identifier.evaluate(env),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        for stmt in &self.0 {
            stmt.evaluate(env)?;
        }

        self.1
            .as_ref()
            .map(|e| e.evaluate(env))
            .unwrap_or(Ok(Value::Nil))
    }
}

impl Evaluate for Call {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        let func = self.target.evaluate(env)?;
        let Value::Func(func) = func else {
            return Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::Func,
                actual: func.into(),
            }));
        };

        let arguments: Vec<Value> = self
            .arguments
            .iter()
            .map(|value| value.evaluate(env))
            .collect::<Result<_>>()?;

        // Create new frame *after* evaluating arguments
        let mut env = env.new_frame(func.clone());

        match func {
            Func::User(func) => {
                // Actually define arguments
                arguments.into_iter().for_each(|v| env.define(v));

                func.body.evaluate(&mut env)
            }
            Func::Native(func) => func.call(arguments),
        }
    }
}

impl Evaluate for IfExpr {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        let condition = self.condition.evaluate(env)?;
        if condition.is_truthy() {
            self.then_block.evaluate(env)
        } else {
            match &self.else_block {
                Some(ElseBlock::Else(else_block)) => Ok(else_block.evaluate(env)?),
                Some(ElseBlock::ElseIf(if_expr)) => Ok(if_expr.evaluate(env)?),
                None => Ok(Value::Nil),
            }
        }
    }
}

impl Evaluate for BinaryExpr {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        use Value::{Bool, Num, Str};

        let lhs = self.lhs.evaluate(env)?;
        // A closure so that it is lazy, for short-circuiting
        let mut rhs = || self.rhs.evaluate(env);
        Ok(match &self.op {
            BinaryOp::Or => {
                if lhs.is_truthy() {
                    lhs
                } else {
                    rhs()?
                }
            }
            BinaryOp::And => {
                if lhs.is_falsy() {
                    lhs
                } else {
                    rhs()?
                }
            }
            BinaryOp::NotEq => Bool(lhs != rhs()?),
            BinaryOp::Eq => Bool(lhs == rhs()?),
            BinaryOp::Greater => Bool(lhs.as_num()? > rhs()?.as_num()?),
            BinaryOp::GreaterEq => Bool(lhs.as_num()? >= rhs()?.as_num()?),
            BinaryOp::Less => Bool(lhs.as_num()? < rhs()?.as_num()?),
            BinaryOp::LessEq => Bool(lhs.as_num()? <= rhs()?.as_num()?),
            BinaryOp::Subtract => Num(lhs.as_num()? - rhs()?.as_num()?),
            BinaryOp::Add => match (lhs, rhs()?) {
                (Num(a), Num(b)) => Num(a + b),
                (Str(a), Num(b)) => Str(a + &b.to_string()),
                (Num(a), Str(b)) => Str(a.to_string() + &b),
                (Str(a), Str(b)) => Str(a + &b),

                // Errors:
                (Num(_), b) | (Str(_), b) => {
                    return Err(Error::new(ErrorKind::TypeError {
                        expected: DiagnosticType::Num,
                        actual: b.into(),
                    }))
                }
                (a, _) => {
                    return Err(Error::new(ErrorKind::TypeError {
                        expected: DiagnosticType::Num,
                        actual: a.into(),
                    }))
                }
            },
            BinaryOp::Divide => Num(lhs.as_num()? / rhs()?.as_num()?),
            BinaryOp::Multiply => Num(lhs.as_num()? * rhs()?.as_num()?),
        })
    }
}

impl Evaluate for UnaryExpr {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        let rhs = self.rhs.evaluate(env)?;
        match self.op {
            UnaryOp::Not => Ok(Value::Bool(!rhs.is_truthy())),
            UnaryOp::Negate => Ok(Value::Num(-rhs.as_num()?)),
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, _env: &mut Env) -> Result<Value> {
        Ok(match self {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Number(n) => Value::Num(*n),
            Literal::Str(s) => Value::Str(s.clone()),
            Literal::Nil => Value::Nil,
        })
    }
}

impl Evaluate for Identifier {
    fn evaluate(&self, env: &mut Env) -> Result<Value> {
        eprintln!("evaluating ident {:?}", self.name);
        Ok(env
            .get(self.location.expect("parser should have resolved variable"))
            .clone())
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Func(Func),
    List(Vec<Value>),
    Nil,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
    fn is_falsy(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }

    fn as_num(&self) -> Result<f64> {
        match self {
            Self::Num(n) => Ok(*n),
            _ => Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::Num,
                actual: DiagnosticType::from(self),
            })),
        }
    }

    fn as_str(&self) -> Result<String> {
        match self {
            Self::Str(s) => Ok(s.clone()),
            _ => Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::Str,
                actual: DiagnosticType::from(self),
            })),
        }
    }

    fn as_list(&self) -> Result<Vec<Value>> {
        match self {
            Self::List(l) => Ok(l.clone()),
            _ => Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::List,
                actual: DiagnosticType::from(self),
            })),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Num(l), Self::Num(r)) => l == r,
            (Self::Str(l), Self::Str(r)) => l == r,
            (Self::Func(_), Self::Func(_)) => false,
            (Self::List(l), Self::List(r)) => l == r,
            (Self::Nil, Self::Nil) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Func {
    User(UserFunc),
    Native(&'static dyn NativeFunc),
}

#[derive(Clone, Debug)]
pub struct UserFunc {
    arguments: Vec<Pattern>,
    upvalues: Vec<Value>,
    body: Expr,
}

pub trait NativeFunc: std::fmt::Debug {
    fn call(&self, arguments: Vec<Value>) -> Result<Value>;
}

#[expect(dead_code, reason = "Pretty error printing not implemented yet")]
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}

#[expect(dead_code, reason = "Pretty error printing not implemented yet")]
#[derive(Debug)]
pub enum ErrorKind {
    TypeError {
        expected: DiagnosticType,
        actual: DiagnosticType,
    },
    IOError(std::io::Error),
    IncorrectArity {
        given: usize,
        correct: usize,
    },
}

#[derive(Debug)]
pub enum DiagnosticType {
    Bool,
    Num,
    Str,
    Func,
    List,
    Nil,
}
impl From<&Value> for DiagnosticType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Num(_) => Self::Num,
            Value::Str(_) => Self::Str,
            Value::Func(_) => Self::Func,
            Value::List(_) => Self::List,
            Value::Nil => Self::Nil,
        }
    }
}
impl From<Value> for DiagnosticType {
    fn from(value: Value) -> Self {
        Self::from(&value)
    }
}
