use wasm_encoder::{
    CodeSection, DataCountSection, DataSection, ElementSection, ElementSegment, ExportSection,
    Function, FunctionSection, GlobalSection, ImportSection, MemorySection, Module, StartSection,
    TableSection, TypeSection,
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
        // Set up main function
        self.type_section.ty().function([], []);
        self.function_section.function(0);

        let locals = [];

        let mut main = Function::new(locals);
        main.instruction(&wasm_encoder::Instruction::End);

        self.code_section.function(&main);
    }

    fn gen_stmt(&mut self, func: &mut wasm_encoder::Function, stmt: &Stmt) {
        match stmt {
            Stmt::Let(binding) => todo!(),
            Stmt::Expr(expr) => todo!(),
        }
    }
}
