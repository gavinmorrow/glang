use wasm_encoder::Module;

use crate::ast::{
    BinaryExpr, Binding, Block, Call, Expr, Identifier, IfExpr, Literal, Program, Stmt, UnaryExpr,
};

pub fn gen_wasm(program: Program) -> Module {
    let mut out = Module::new();

    for stmt in program {
        todo!()
    }

    out
}
