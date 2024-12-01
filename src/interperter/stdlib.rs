use std::io::Write;

use crate::ast::{Identifier, Scope};

use super::{env::Environment, Error, ErrorKind, Func, NativeFunc, Value};

impl Environment {
    pub fn new_root() -> (Self, Scope) {
        let mut root = Self::new();
        let root_scope = Scope::new();

        define_stdlib(&mut root, root_scope.clone());

        (root, root_scope)
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
    native_func!(ListLenFunc, list_len);
    native_func!(StrToCharsFunc, str_to_chars);
    native_func!(StrFromCharsFunc, str_from_chars);
    native_func!(ReadFileFunc, read_file);
    native_func!(ReadFileLinesFunc, read_file_lines);
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
    Ok(list.get(index).cloned().unwrap_or(Value::Nil))
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

fn list_len(arguments: Vec<Value>) -> super::Result<Value> {
    let list = arguments.first().unwrap().as_list()?;
    Ok(Value::Num(list.len() as f64))
}

fn str_to_chars(arguments: Vec<Value>) -> super::Result<Value> {
    let str = arguments.first().unwrap().as_str()?;
    let chars: Vec<_> = str.chars().map(|c| Value::Str(c.into())).collect();
    Ok(Value::List(chars))
}

fn str_from_chars(arguments: Vec<Value>) -> super::Result<Value> {
    let chars = arguments.first().unwrap().as_list()?;
    let str: Option<Vec<_>> = chars
        .into_iter()
        .map(|v| match v {
            Value::Str(s) => {
                let chars: Vec<_> = s.chars().collect();
                if chars.len() == 1 {
                    Some(chars[0])
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect();
    if let Some(str) = str {
        let str = String::from_iter(str);
        Ok(Value::Str(str))
    } else {
        Ok(Value::Nil)
    }
}

fn read_file(arguments: Vec<Value>) -> super::Result<Value> {
    let path = arguments.first().unwrap().as_str()?;
    let Ok(contents) = std::fs::read_to_string(path) else {
        return Ok(Value::Nil);
    };
    Ok(Value::Str(contents))
}

// To get around stack overflow w/ split by \n
fn read_file_lines(arguments: Vec<Value>) -> super::Result<Value> {
    let Value::Str(contents) = read_file(arguments)? else {
        return Ok(Value::Nil);
    };
    let lines = contents
        .lines()
        .map(|l| Value::Str(l.to_string()))
        .collect();
    Ok(Value::List(lines))
}
