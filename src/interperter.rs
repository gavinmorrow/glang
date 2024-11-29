use crate::ast::Program;

pub fn interpert(program: Program) -> Value {
    todo!()
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Void,
}
