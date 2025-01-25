use wasm_encoder::{
    CodeSection, DataCountSection, DataSection, ElementSection, ElementSegment, EntityType,
    ExportSection, Function, FunctionSection, GlobalSection, ImportSection, MemorySection, Module,
    StartSection, TableSection, TypeSection, ValType,
};

use crate::{
    ast::{
        BinaryExpr, Binding, Block, Call, Expr, Identifier, IfExpr, Literal, Program, Stmt,
        UnaryExpr,
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
        self.type_section.ty().function([ValType::I32], []);
        self.import_section
            .import("std", "print", EntityType::Function(0));

        // Set up main function
        self.type_section.ty().function([], []);
        self.function_section.function(1);
        self.start_section.function_index = 1;

        let locals = [];

        let mut main = Function::new(locals);
        main.instruction(&wasm_encoder::Instruction::I32Const(42));
        main.instruction(&wasm_encoder::Instruction::Call(0));
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
            Expr::Block(block) => todo!(),
            Expr::Call(call) => todo!(),
            Expr::If(if_expr) => todo!(),
            Expr::Binary(binary_expr) => todo!(),
            Expr::Unary(unary_expr) => todo!(),
            Expr::Literal(literal) => todo!(),
            Expr::Identifier(identifier) => todo!(),
        }
    }
}
