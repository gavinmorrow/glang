use crate::ast::{
    BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
    Program, Stmt, UnaryExpr,
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
                if lhs.is_truthy() {
                    rhs()?
                } else {
                    lhs
                }
            }
            BinaryOp::NotEq => Bool(lhs != rhs()?),
            BinaryOp::Eq => Bool(lhs == rhs()?),
            BinaryOp::Greater => todo!(),
            BinaryOp::GreaterEq => todo!(),
            BinaryOp::Less => todo!(),
            BinaryOp::LessEq => todo!(),
            BinaryOp::Subtract => todo!(),
            BinaryOp::Add => todo!(),
            BinaryOp::Divide => todo!(),
            BinaryOp::Multiply => todo!(),
        })
    }
}

impl Evaluate for UnaryExpr {
    fn evaluate(&self) -> Result<Value> {
        todo!()
    }
}

impl Evaluate for Literal {
    fn evaluate(&self) -> Result<Value> {
        todo!()
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
        !matches!(self, Value::Bool(false))
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {}
