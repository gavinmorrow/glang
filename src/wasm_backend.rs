#![expect(unused)]

use wasm_encoder::{
    CodeSection, DataCountSection, DataSection, ElementSection, ElementSegment, EntityType,
    ExportSection, Function, FunctionSection, GlobalSection, ImportSection, MemorySection, Module,
    StartSection, TableSection, TypeSection, ValType,
};

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
        Program, Stmt, UnaryExpr, UnaryOp,
    },
    interperter::Func,
};

pub fn gen_wasm(program: Program) -> Module {
    let mut wasm_gen = WasmGen::new(program);
    wasm_gen.gen()
}

struct WasmGen {
    program: Program,
}

impl WasmGen {
    fn new(program: Program) -> Self {
        WasmGen { program }
    }

    fn finish(mut self, main: &Function) -> Module {
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
        code_section.function(main);

        let mut module = Module::new();
        module
            .section(&type_section)
            .section(&import_section)
            .section(&function_section)
            .section(&code_section)
            .section(&start_section);
        module
    }

    fn gen(mut self) -> Module {
        let locals = [];
        let mut main = Function::new(locals);

        for stmt in self.program.clone() {
            self.gen_stmt(&mut main, &stmt);
        }
        main.instruction(&wasm_encoder::Instruction::End);

        self.finish(&main)
    }

    fn gen_stmt(&mut self, func: &mut wasm_encoder::Function, stmt: &Stmt) {
        match stmt {
            Stmt::Let(binding) => todo!(),
            Stmt::Expr(expr) => self.gen_expr(func, expr),
        }
    }

    fn gen_expr(&mut self, func: &mut wasm_encoder::Function, expr: &Expr) {
        match expr {
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.gen_stmt(func, stmt);
                }

                if let Some(expr) = &block.return_expr {
                    self.gen_expr(func, expr);
                }
            }
            Expr::Call(call) => {
                // Put args on the stack
                for arg in &call.arguments {
                    self.gen_expr(func, arg);
                }

                // Get target onto the stack
                // self.gen_expr(func, &call.target);
                func.instruction(&wasm_encoder::Instruction::Call(0));
            }
            Expr::If(if_expr) => self.gen_if(func, if_expr),
            Expr::Binary(binary_expr) => {
                // Put lhs, then rhs on stack
                self.gen_expr(func, &binary_expr.lhs);
                self.gen_expr(func, &binary_expr.rhs);

                use wasm_encoder::Instruction::{
                    F64Add, F64Div, F64Eq, F64Ge, F64Gt, F64Le, F64Lt, F64Mul, F64Ne, F64Sub,
                    I32And, I32Or,
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
                self.gen_expr(func, &unary_expr.rhs);

                let op = match unary_expr.op {
                    UnaryOp::Not => {
                        // Use XOR 1 as not
                        // 0 xor 1 -> 1
                        // 1 xor 1 -> 0
                        func.instruction(&wasm_encoder::Instruction::I32Const(1));
                        wasm_encoder::Instruction::I32Xor
                    }
                    UnaryOp::Negate => wasm_encoder::Instruction::F64Neg,
                };
                func.instruction(&op);
            }
            Expr::Literal(literal) => match literal {
                Literal::Bool(b) => {
                    let int_val = if *b { 1 } else { 0 };
                    func.instruction(&wasm_encoder::Instruction::I32Const(int_val));
                }
                Literal::Number(n) => {
                    func.instruction(&wasm_encoder::Instruction::F64Const(*n));
                }
                Literal::Str(_) => todo!(),
                Literal::Nil => todo!(),
            },
            Expr::Identifier(identifier) => todo!(),
        }
    }

    fn gen_if(&mut self, func: &mut Function, if_expr: &IfExpr) {
        // Condition
        self.gen_expr(func, &if_expr.condition);

        // if instruction
        // TODO: put actual type or smth
        func.instruction(&wasm_encoder::Instruction::If(
            wasm_encoder::BlockType::Result(ValType::F64),
        ));

        // then block
        let then_expr = Expr::Block(if_expr.then_block.clone());
        self.gen_expr(func, &then_expr);

        // else block
        if let Some(else_block) = &if_expr.else_block {
            func.instruction(&wasm_encoder::Instruction::Else);

            match else_block {
                ElseBlock::ElseIf(if_expr) => {
                    self.gen_if(func, if_expr);
                }
                ElseBlock::Else(block) => {
                    let else_expr = Expr::Block(block.clone());
                    self.gen_expr(func, &else_expr);
                }
            }
        }

        func.instruction(&wasm_encoder::Instruction::End);
    }
}
