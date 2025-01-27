#![expect(unused)]

mod collect_locals;

use collect_locals::collect_locals;
use wasm_encoder::{
    self as wasm, CodeSection, DataCountSection, DataSection, ElementSection, ElementSegment,
    EntityType, ExportSection, Function, FunctionSection, GlobalSection, ImportSection,
    MemorySection, Module, StartSection, TableSection, TypeSection, ValType,
};

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
        Program, Stmt, UnaryExpr, UnaryOp,
    },
    interperter::Func,
};

pub fn gen_wasm(program: &Program) -> Module {
    // TODO: when type info become available actually type locals
    let locals = [(collect_locals(program).len() as u32, ValType::F64)];
    let mut main = Function::new(locals);

    // Generate main func body
    for stmt in program {
        gen_stmt(&mut main, stmt);
    }
    main.instruction(&wasm::Instruction::End);

    // Generate actual binary
    let mut type_section = TypeSection::new();
    let mut import_section = ImportSection::new();
    let mut function_section = FunctionSection::new();
    let mut code_section = CodeSection::new();
    let mut start_section = StartSection { function_index: 1 };

    // Set up imports
    type_section.ty().function([ValType::F64], []);
    import_section.import("std", "print", EntityType::Function(0));

    // Set up main function
    type_section.ty().function([], []);
    function_section.function(1);
    start_section.function_index = 1;
    code_section.function(&main);

    let mut module = Module::new();
    module
        .section(&type_section)
        .section(&import_section)
        .section(&function_section)
        .section(&start_section)
        .section(&code_section);
    module
}

fn gen_stmt(func: &mut wasm::Function, stmt: &Stmt) {
    match stmt {
        Stmt::Let(binding) => todo!(),
        Stmt::Expr(expr) => gen_expr(func, expr),
    }
}

fn gen_expr(func: &mut wasm::Function, expr: &Expr) {
    match expr {
        Expr::Block(block) => {
            for stmt in &block.stmts {
                gen_stmt(func, stmt);
            }

            if let Some(expr) = &block.return_expr {
                gen_expr(func, expr);
            }
        }
        Expr::Call(call) => {
            // Put args on the stack
            for arg in &call.arguments {
                gen_expr(func, arg);
            }

            // Get target onto the stack
            // gen_expr(func, &call.target);
            func.instruction(&wasm::Instruction::Call(0));
        }
        Expr::If(if_expr) => gen_if(func, if_expr),
        Expr::Binary(binary_expr) => {
            // Put lhs, then rhs on stack
            gen_expr(func, &binary_expr.lhs);
            gen_expr(func, &binary_expr.rhs);

            use wasm::Instruction::{
                F64Add, F64Div, F64Eq, F64Ge, F64Gt, F64Le, F64Lt, F64Mul, F64Ne, F64Sub, I32And,
                I32Or,
            };
            let opcode = match binary_expr.op {
                BinaryOp::Or => I32And,
                BinaryOp::And => I32Or,
                BinaryOp::NotEq => F64Ne,
                BinaryOp::Eq => F64Eq,
                BinaryOp::Greater => F64Gt,
                BinaryOp::GreaterEq => F64Ge,
                BinaryOp::Less => F64Lt,
                BinaryOp::LessEq => F64Le,
                BinaryOp::Subtract => F64Sub,
                BinaryOp::Add => F64Add,
                BinaryOp::Divide => F64Div,
                BinaryOp::Multiply => F64Mul,
            };
            func.instruction(&opcode);
        }
        Expr::Unary(unary_expr) => {
            gen_expr(func, &unary_expr.rhs);

            let op = match unary_expr.op {
                UnaryOp::Not => {
                    // Use XOR 1 as not
                    // 0 xor 1 -> 1
                    // 1 xor 1 -> 0
                    func.instruction(&wasm::Instruction::I32Const(1));
                    wasm::Instruction::I32Xor
                }
                UnaryOp::Negate => wasm::Instruction::F64Neg,
            };
            func.instruction(&op);
        }
        Expr::Literal(literal) => match literal {
            Literal::Bool(b) => {
                let int_val = if *b { 1 } else { 0 };
                func.instruction(&wasm::Instruction::I32Const(int_val));
            }
            Literal::Number(n) => {
                func.instruction(&wasm::Instruction::F64Const(*n));
            }
            Literal::Str(_) => todo!(),
            Literal::Nil => todo!(),
        },
        Expr::Identifier(identifier) => todo!(),
    }
}

fn gen_if(func: &mut Function, if_expr: &IfExpr) {
    // Condition
    gen_expr(func, &if_expr.condition);

    // if instruction
    // TODO: put actual type or smth
    func.instruction(&wasm::Instruction::If(wasm::BlockType::Result(
        ValType::F64,
    )));

    // then block
    let then_expr = Expr::Block(if_expr.then_block.clone());
    gen_expr(func, &then_expr);

    // else block
    if let Some(else_block) = &if_expr.else_block {
        func.instruction(&wasm::Instruction::Else);

        match else_block {
            ElseBlock::ElseIf(if_expr) => {
                gen_if(func, if_expr);
            }
            ElseBlock::Else(block) => {
                let else_expr = Expr::Block(block.clone());
                gen_expr(func, &else_expr);
            }
        }
    }

    func.instruction(&wasm::Instruction::End);
}
