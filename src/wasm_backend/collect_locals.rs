use crate::ast::{Expr, Stmt};

// TODO: when types are added, put that type instead of `()`
type Locals = Vec<()>;

pub fn collect_locals(stmts: &Vec<Stmt>) -> Locals {
    let mut locals = Vec::new();

    for stmt in stmts {
        locals.append(&mut stmt_locals(stmt));
    }

    locals
}

fn stmt_locals(stmt: &Stmt) -> Locals {
    match stmt {
        Stmt::Let(binding) => vec![()],
        Stmt::Expr(expr) => expr_locals(expr),
    }
}

fn expr_locals(expr: &Expr) -> Locals {
    match expr {
        Expr::Block(block) => {
            let mut locals = collect_locals(&block.stmts);
            if let Some(expr) = &block.return_expr {
                locals.append(&mut expr_locals(expr))
            }
            locals
        }
        Expr::Call(call) => {
            let mut locals = expr_locals(&call.target);
            locals.extend(&mut call.arguments.iter().flat_map(expr_locals));
            locals
        }
        Expr::If(if_expr) => todo!(),
        Expr::Binary(binary_expr) => todo!(),
        Expr::Unary(unary_expr) => todo!(),
        Expr::Literal(literal) => todo!(),
        Expr::Identifier(identifier) => todo!(),
    }
}
