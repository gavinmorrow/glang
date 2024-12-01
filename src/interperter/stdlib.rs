use super::{
    env::{Environment, Identifier, Scope},
    Func, NativeFunc, Value,
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
    PrintFunc::define(env, scope.clone());
}

#[derive(Debug)]
struct PrintFunc;
impl PrintFunc {
    fn define(env: &mut Environment, scope: Scope) {
        let identifier = Identifier::new(scope, "print".to_string());
        env.define(identifier, Value::Func(Func::Native(&PrintFunc)));
    }
}
impl NativeFunc for PrintFunc {
    fn call(&self, arguments: Vec<Value>) -> super::Result<Value> {
        for arg in arguments {
            println!("{arg:#?}");
        }
        Ok(Value::Void)
    }
}
