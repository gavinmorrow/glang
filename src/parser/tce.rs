use crate::ast::{
    BinaryExpr, Binding, BindingMetadata, Block, Call, Expr, IfExpr, Program, Stmt, UnaryExpr,
};

pub trait MarkTailCalls {
    fn mark_tail_calls(&mut self);
}

impl MarkTailCalls for Program {
    fn mark_tail_calls(&mut self) {
        for stmt in self {
            stmt.mark_tail_calls();
        }
    }
}

impl MarkTailCalls for Stmt {
    fn mark_tail_calls(&mut self) {
        match self {
            Stmt::Let(binding) => binding.mark_tail_calls(),
            Stmt::Expr(expr) => expr.mark_tail_calls(),
        }
    }
}

impl MarkTailCalls for Binding {
    fn mark_tail_calls(&mut self) {
        let BindingMetadata::Func { .. } = &mut self.metadata else {
            return;
        };

        let func_body = &mut self.value;
        let tail_calls = func_body.get_tail_calls_mut();

        for call in tail_calls {
            call.is_tail_call = true;
        }
    }
}

impl MarkTailCalls for Expr {
    fn mark_tail_calls(&mut self) {
        match self {
            Expr::Block(block) => block.mark_tail_calls(),
            Expr::If(if_expr) => if_expr.mark_tail_calls(),
            Expr::Binary(binary_expr) => binary_expr.mark_tail_calls(),
            Expr::Unary(unary_expr) => unary_expr.mark_tail_calls(),
            Expr::Call(_) => {}
            Expr::Literal(_) => {}
            Expr::Identifier(_) => {}
        }
    }
}

impl MarkTailCalls for Block {
    fn mark_tail_calls(&mut self) {
        for stmt in &mut self.stmts {
            stmt.mark_tail_calls();
        }
        if let Some(expr) = &mut self.return_expr {
            expr.mark_tail_calls();
        }
    }
}

impl MarkTailCalls for IfExpr {
    fn mark_tail_calls(&mut self) {
        self.then_block.mark_tail_calls();
        if let Some(else_block) = &mut self.else_block {
            use crate::ast::ElseBlock::{Else, ElseIf};
            match else_block {
                ElseIf(if_expr) => if_expr.mark_tail_calls(),
                Else(block) => block.mark_tail_calls(),
            }
        }
    }
}

impl MarkTailCalls for BinaryExpr {
    fn mark_tail_calls(&mut self) {
        self.lhs.mark_tail_calls();
        self.rhs.mark_tail_calls();
    }
}

impl MarkTailCalls for UnaryExpr {
    fn mark_tail_calls(&mut self) {
        self.rhs.mark_tail_calls();
    }
}

trait TailCalls {
    fn get_tail_calls_mut(&mut self) -> Vec<&mut Call>;
}

impl TailCalls for Expr {
    fn get_tail_calls_mut(&mut self) -> Vec<&mut Call> {
        match self {
            Expr::Block(block) => block.get_tail_calls_mut(),
            Expr::Call(call) => vec![call],
            Expr::If(if_expr) => if_expr.get_tail_calls_mut(),
            _ => vec![],
        }
    }
}

impl TailCalls for Block {
    fn get_tail_calls_mut(&mut self) -> Vec<&mut Call> {
        self.return_expr
            .as_mut()
            .map(|e| match e.as_mut() {
                Expr::Call(c) => vec![c],
                _ => vec![],
            })
            .unwrap_or_default()
    }
}

impl TailCalls for IfExpr {
    fn get_tail_calls_mut(&mut self) -> Vec<&mut Call> {
        let mut then_block = self.then_block.get_tail_calls_mut();
        let mut else_block = {
            match &mut self.else_block {
                Some(else_block) => match else_block {
                    crate::ast::ElseBlock::ElseIf(if_expr) => if_expr.get_tail_calls_mut(),
                    crate::ast::ElseBlock::Else(block) => block.get_tail_calls_mut(),
                },
                None => vec![],
            }
        };
        then_block.append(&mut else_block);
        then_block
    }
}
