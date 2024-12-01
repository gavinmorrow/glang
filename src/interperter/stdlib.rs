use std::io::Write;

use super::{
    env::{Environment, Identifier, Scope},
    Error, ErrorKind, Func, NativeFunc, Value,
};

impl Environment {
    pub fn new_root() -> (Self, Scope) {
        let mut root = Self::new();
        let root_scope = Scope::new();

        define_stdlib(&mut root, root_scope.clone());

        // Define a new scope so that user code can't overwrite the stdlib
        let scope = root_scope.nest();

        (root, scope)
    }
}

fn define_stdlib(env: &mut Environment, scope: Scope) {
    macro_rules! native_func {
        ($struct_name:ident, $func_def:ident) => {
            #[derive(Debug)]
            struct $struct_name;
            impl $struct_name {
                fn define(env: &mut Environment, scope: Scope) {
                    let identifier = Identifier::new(scope, stringify!($func_def).to_string());
                    env.define(identifier, Value::Func(Func::Native(&$struct_name)));
                }
            }
            impl NativeFunc for $struct_name {
                fn call(&self, arguments: Vec<Value>) -> super::Result<Value> {
                    $func_def(arguments)
                }
            }
            $struct_name::define(env, scope.clone());
        };
    }

    native_func!(PrintFunc, print);
    native_func!(InputFunc, input);
    native_func!(ListFunc, list);
    native_func!(ListGetFunc, list_get);
    native_func!(ListSetFunc, list_set);
    native_func!(ListPushFunc, list_push);
}

fn print(arguments: Vec<Value>) -> super::Result<Value> {
    for arg in arguments {
        println!("{arg:#?}");
    }
    Ok(Value::Nil)
}

fn input(arguments: Vec<Value>) -> super::Result<Value> {
    if arguments.len() > 1 {
        return Err(Error::new(ErrorKind::IncorrectArity {
            given: arguments.len(),
            correct: 1,
        }));
    }
    let empty_string = Value::Str("".to_string());
    let question_str = arguments.first().unwrap_or(&empty_string);
    print!("{question_str:?}");
    std::io::stdout().flush().unwrap();

    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|err| Error::new(ErrorKind::IOError(err)))?;
    input.pop().expect("trailing newline should be removed");
    Ok(Value::Str(input))
}

fn list(arguments: Vec<Value>) -> super::Result<Value> {
    Ok(Value::List(arguments))
}

fn list_get(arguments: Vec<Value>) -> super::Result<Value> {
    let list = arguments.first().unwrap().as_list()?;
    let index = arguments.get(1).unwrap().as_num()? as usize;
    Ok(list.get(index).unwrap().clone())
}

fn list_set(arguments: Vec<Value>) -> super::Result<Value> {
    let mut list = arguments.first().unwrap().as_list()?;
    let index = arguments.get(1).unwrap().as_num()? as usize;
    let new_value = arguments.get(2).unwrap().clone();
    *list.get_mut(index).unwrap() = new_value;
    Ok(Value::List(list))
}

fn list_push(arguments: Vec<Value>) -> super::Result<Value> {
    let mut list = arguments.first().unwrap().as_list()?;
    let value = arguments.get(1).unwrap().clone();
    list.push(value);
    Ok(Value::List(list))
}
