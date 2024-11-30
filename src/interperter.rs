use crate::ast::{
    BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
    Program, Stmt, UnaryExpr, UnaryOp,
};

pub fn interpert(program: Program) -> Result<Value> {
    program.evaluate()
}

pub type Result<T> = std::result::Result<T, Error>;

trait Evaluate {
    fn evaluate(&self) -> Result<Value>;
}

impl Evaluate for Program {
    fn evaluate(&self) -> Result<Value> {
        for stmt in self {
            stmt.evaluate()?;
        }
        Ok(Value::Void)
    }
}

impl Evaluate for Stmt {
    fn evaluate(&self) -> Result<Value> {
        match self {
            Stmt::Let(binding) => binding.evaluate(),
            Stmt::Expr(expr) => expr.evaluate(),
        }
    }
}

impl Evaluate for Binding {
    fn evaluate(&self) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Expr {
    fn evaluate(&self) -> Result<Value> {
        match self {
            Expr::Block(block) => block.evaluate(),
            Expr::Call(call) => call.evaluate(),
            Expr::If(if_expr) => if_expr.evaluate(),
            Expr::Binary(binary_expr) => binary_expr.evaluate(),
            Expr::Unary(unary_expr) => unary_expr.evaluate(),
            Expr::Literal(literal) => literal.evaluate(),
            Expr::Identifier(identifier) => identifier.evaluate(),
        }
    }
}

impl Evaluate for Block {
    fn evaluate(&self) -> Result<Value> {
        for stmt in &self.0 {
            stmt.evaluate()?;
        }

        Ok(Value::Void)
    }
}

impl Evaluate for Call {
    fn evaluate(&self) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for IfExpr {
    fn evaluate(&self) -> Result<Value> {
        let condition = self.condition.evaluate()?;
        if condition.is_truthy() {
            self.then_block.evaluate()
        } else {
            match &self.else_block {
                Some(ElseBlock::Else(else_block)) => Ok(else_block.evaluate()?),
                Some(ElseBlock::ElseIf(if_expr)) => Ok(if_expr.evaluate()?),
                None => Ok(Value::Void),
            }
        }
    }
}

impl Evaluate for BinaryExpr {
    fn evaluate(&self) -> Result<Value> {
        use Value::{Bool, Num, Str};

        let lhs = self.lhs.evaluate()?;
        // A closure so that it is lazy, for short-circuiting
        let rhs = || self.rhs.evaluate();
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
    fn evaluate(&self) -> Result<Value> {
        let rhs = self.rhs.evaluate()?;
        match self.op {
            UnaryOp::Not => Ok(Value::Bool(!rhs.is_truthy())),
            UnaryOp::Negate => Ok(Value::Num(-rhs.as_num()?)),
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self) -> Result<Value> {
        Ok(match self {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Number(n) => Value::Num(*n),
            Literal::Str(s) => Value::Str(s.clone()),
        })
    }
}

impl Evaluate for Identifier {
    fn evaluate(&self) -> Result<Value> {
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
