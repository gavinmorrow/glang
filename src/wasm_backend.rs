#![expect(unused)]

use wasm_encoder::{
    CodeSection, DataCountSection, DataSection, ElementSection, ElementSegment, EntityType,
    ExportSection, Function, FunctionSection, GlobalSection, ImportSection, MemorySection, Module,
    StartSection, TableSection, TypeSection, ValType,
};

use crate::{
    ast::{
        BinaryExpr, BinaryOp, Binding, Block, Call, ElseBlock, Expr, Identifier, IfExpr, Literal,
        Program, Stmt, UnaryExpr,
    },
    interperter::Func,
};

pub fn gen_wasm(program: Program) -> Module {
    let mut wasm_gen = WasmGen::new(program);
    wasm_gen.gen();
    wasm_gen.finish()
}

struct WasmGen {
    program: Program,

    module: Module,

    type_section: TypeSection,
    import_section: ImportSection,
    function_section: FunctionSection,
    table_section: TableSection,
    memory_section: MemorySection,
    global_section: GlobalSection,
    export_section: ExportSection,
    start_section: StartSection,
    element_section: ElementSection,
    code_section: CodeSection,
    data_section: DataSection,
    data_count_section: DataCountSection,
}

impl WasmGen {
    fn new(program: Program) -> Self {
        WasmGen {
            program,

            module: Module::new(),

            type_section: TypeSection::new(),
            import_section: ImportSection::new(),
            function_section: FunctionSection::new(),
            table_section: TableSection::new(),
            memory_section: MemorySection::new(),
            global_section: GlobalSection::new(),
            export_section: ExportSection::new(),
            start_section: StartSection { function_index: 0 },
            element_section: ElementSection::new(),
            code_section: CodeSection::new(),
            data_section: DataSection::new(),
            data_count_section: DataCountSection { count: 0 },
        }
    }

    fn finish(mut self) -> Module {
        self.module.section(&self.type_section);
        self.module.section(&self.import_section);
        self.module.section(&self.function_section);
        self.module.section(&self.table_section);
        self.module.section(&self.memory_section);
        self.module.section(&self.global_section);
        self.module.section(&self.export_section);
        self.module.section(&self.start_section);
        self.module.section(&self.element_section);
        self.module.section(&self.data_count_section);
        self.module.section(&self.code_section);
        self.module.section(&self.data_section);

        self.module
    }

    fn gen(&mut self) {
        // Set up imports
        self.type_section.ty().function([ValType::F64], []);
        self.import_section
            .import("std", "print", EntityType::Function(0));

        // Set up main function
        self.type_section.ty().function([], []);
        self.function_section.function(1);
        self.start_section.function_index = 1;

        let locals = [];

        let mut main = Function::new(locals);
        for stmt in self.program.clone() {
            self.gen_stmt(&mut main, &stmt);
        }
        main.instruction(&wasm_encoder::Instruction::End);

        self.code_section.function(&main);
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

                use wasm_encoder::Instruction::{F64Add, F64Div, F64Mul, F64Sub};
                let opcode = match binary_expr.op {
                    BinaryOp::Or => todo!(),
                    BinaryOp::And => todo!(),
                    BinaryOp::NotEq => todo!(),
                    BinaryOp::Eq => todo!(),
                    BinaryOp::Greater => todo!(),
                    BinaryOp::GreaterEq => todo!(),
                    BinaryOp::Less => todo!(),
                    BinaryOp::LessEq => todo!(),
                    BinaryOp::Subtract => F64Sub,
                    BinaryOp::Add => F64Add,
                    BinaryOp::Divide => F64Div,
                    BinaryOp::Multiply => F64Mul,
                };
                func.instruction(&opcode);
            }
            Expr::Unary(unary_expr) => todo!(),
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
        // FIXME: put actual type or smth
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
