mod env;

use env::{Environment, Scope};

use crate::ast::{
    BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
    Program, Stmt, UnaryExpr, UnaryOp,
};

pub fn interpert(program: Program) -> Result<Value> {
    let mut env = Environment::new();
    let scope = Scope::new();
    program.evaluate(&mut env, scope)
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
        todo!()
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

        Ok(Value::Void)
    }
}

impl Evaluate for Call {
    fn evaluate(&self, env: &mut Environment, scope: Scope) -> Result<Value> {
        todo!()
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
                        actual: (&b).into(),
                    }))
                }
                (a, _) => {
                    return Err(Error::new(ErrorKind::TypeError {
                        expected: DiagnosticType::Num,
                        actual: (&a).into(),
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
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
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

    fn as_str(&self) -> Result<String> {
        match self {
            Self::Str(s) => Ok(s.clone()),
            _ => Err(Error::new(ErrorKind::TypeError {
                expected: DiagnosticType::Str,
                actual: DiagnosticType::from(self),
            })),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    TypeError {
        expected: DiagnosticType,
        actual: DiagnosticType,
    },
}

#[derive(Debug)]
pub enum DiagnosticType {
    Bool,
    Num,
    Str,
    Void,
}
impl From<&Value> for DiagnosticType {
    fn from(value: &Value) -> Self {
        match value {
            Value::Bool(_) => Self::Bool,
            Value::Num(_) => Self::Num,
            Value::Str(_) => Self::Str,
            Value::Void => Self::Void,
        }
    }
}
