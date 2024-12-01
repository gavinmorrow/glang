pub mod env;
mod stdlib;

use std::iter::zip;

use env::{Environment, Scope};

use crate::ast::{
    BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
    Pattern, Program, Stmt, UnaryExpr, UnaryOp,
};

pub fn interpert(program: Program, env: &mut Environment, scope: Scope) -> Result<Value> {
    program.evaluate(env, scope)
}

pub type Result<T> = std::result::Result<T, Error>;

trait Evaluate {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value>;
}

impl Evaluate for Program {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        for stmt in self {
            stmt.evaluate(env, scope.clone())?;
        }
        Ok(Value::Void)
    }
}

impl Evaluate for Stmt {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        match self {
            Stmt::Let(binding) => binding.evaluate(env, scope),
            Stmt::Expr(expr) => expr.evaluate(env, scope),
        }
    }
}

impl Evaluate for Binding {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        let name = self.pattern.0.name.clone();
        let identifier = env::Identifier::new(scope.clone(), name);

        let value = match &self.arguments {
            // Don't `.evaluate()` anything for a function
            Some(arguments) => {
                let arguments = arguments.clone();
                let body = self.value.clone();
                Value::Func(Func::User(UserFunc { arguments, body }))
            }
            // But do for a variable
            None => self.value.evaluate(env, scope)?,
        };
        env.define(identifier, value);
        Ok(Value::Void)
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        match self {
            Expr::Block(block) => block.evaluate(env, scope),
            Expr::Call(call) => call.evaluate(env, scope),
            Expr::If(if_expr) => if_expr.evaluate(env, scope),
            Expr::Binary(binary_expr) => binary_expr.evaluate(env, scope),
            Expr::Unary(unary_expr) => unary_expr.evaluate(env, scope),
            Expr::Literal(literal) => literal.evaluate(env, scope),
            Expr::Identifier(identifier) => identifier.evaluate(env, scope),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        for stmt in &self.0 {
            stmt.evaluate(env, scope.clone())?;
        }

        self.1
            .as_ref()
            .map(|e| e.evaluate(env, scope))
            .unwrap_or(Ok(Value::Void))
    }
}

impl Evaluate for Call {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        let func = self.target.evaluate(env, scope.clone())?;
        let Value::Func(func) = func else {
            return Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::Func,
                actual: func.into(),
            }));
        };

        match func {
            Func::User(func) => {
                // Create new scope for function
                let func_scope = scope.nest();

                // Initialize arguments as values in that scope
                for (pattern, value) in zip(func.arguments, &self.arguments) {
                    let name = pattern.0.name.clone();
                    let identifier = env::Identifier::new(func_scope.clone(), name);

                    // Evaluate values in outer scope, to prevent them from using
                    // previously defined arguments (that would be really confusing)
                    let value = value.evaluate(env, scope.clone())?;
                    env.define(identifier, value);
                }

                // Evaluate function body
                func.body.evaluate(env, func_scope)
            }
            Func::Native(func) => {
                let mut arguments = vec![];
                for argument in &self.arguments {
                    let argument = argument.evaluate(env, scope.clone())?;
                    arguments.push(argument);
                }
                func.call(arguments)
            }
        }
    }
}

impl Evaluate for IfExpr {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        let condition = self.condition.evaluate(env, scope.clone())?;
        if condition.is_truthy() {
            self.then_block.evaluate(env, scope)
        } else {
            match &self.else_block {
                Some(ElseBlock::Else(else_block)) => Ok(else_block.evaluate(env, scope)?),
                Some(ElseBlock::ElseIf(if_expr)) => Ok(if_expr.evaluate(env, scope)?),
                None => Ok(Value::Void),
            }
        }
    }
}

impl Evaluate for BinaryExpr {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        use Value::{Bool, Num, Str};

        let lhs = self.lhs.evaluate(env, scope.clone())?;
        // A closure so that it is lazy, for short-circuiting
        let rhs = || self.rhs.evaluate(env, scope);
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
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        let rhs = self.rhs.evaluate(env, scope)?;
        match self.op {
            UnaryOp::Not => Ok(Value::Bool(!rhs.is_truthy())),
            UnaryOp::Negate => Ok(Value::Num(-rhs.as_num()?)),
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, _env: &mut Environment, _scope: Scope) -> Result<Value> {
        Ok(match self {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Number(n) => Value::Num(*n),
            Literal::Str(s) => Value::Str(s.clone()),
        })
    }
}

impl Evaluate for Identifier {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        env.get(&env::Identifier::new(scope, self.name.clone()))
            .ok_or_else(|| {
                Error::new(ErrorKind::VariableNotDefinied {
                    name: self.name.clone(),
                })
            })
            .cloned()
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Func(Func),
    List(Vec<Value>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }
    fn is_falsy(&self) -> bool {
        matches!(self, Value::Bool(false))
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
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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
    VariableNotDefinied {
        name: String,
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
    Void,
}
impl From<&Value> for DiagnosticType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Num(_) => Self::Num,
            Value::Str(_) => Self::Str,
            Value::Func(_) => Self::Func,
            Value::List(_) => Self::List,
            Value::Void => Self::Void,
        }
    }
}
impl From<Value> for DiagnosticType {
    fn from(value: Value) -> Self {
        Self::from(&value)
    }
}
