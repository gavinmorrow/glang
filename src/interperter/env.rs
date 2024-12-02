use std::collections::HashMap;

use crate::ast::Identifier;

use super::Value;

pub struct Environment {
    values: HashMap<Identifier, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, identifier: Identifier, value: Value) {
        self.values.insert(identifier, value);
    }

    pub fn get(&self, identifier: &Identifier) -> Option<&Value> {
        self.values.get(identifier)
    }

    fn get_mut(&mut self, identifier: &Identifier) -> Option<&mut Value> {
        self.values.get_mut(identifier)
    }

    #[expect(
        dead_code,
        reason = "Setters aren't a thing, but might be in the future"
    )]
    /// Update a value if it exists.
    ///
    /// Returns `Ok(())` if the value exists and was updated, and `Err(())`
    /// otherwise.
    pub fn set(&mut self, identifier: &Identifier, value: Value) -> Result<(), ()> {
        if let Some(variable) = self.get_mut(identifier) {
            *variable = value;
            Ok(())
        } else {
            Err(())
        }
    }
}
