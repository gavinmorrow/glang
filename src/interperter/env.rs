use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

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

    fn resolve_scope(&self, identifier: &Identifier) -> Option<Identifier> {
        if self.values.contains_key(identifier) {
            Some(identifier.clone())
        } else {
            let mut id = identifier.clone();
            while let Some(id) = id.in_parent_scope() {
                if let Some(resolved_ident) = self.resolve_scope(&id) {
                    return Some(resolved_ident);
                }
            }
            None
        }
    }

    pub fn get(&self, identifier: &Identifier) -> Option<&Value> {
        self.resolve_scope(identifier)
            .and_then(|identifier| self.values.get(&identifier))
    }

    fn get_mut(&mut self, identifier: &Identifier) -> Option<&mut Value> {
        self.resolve_scope(identifier)
            .and_then(|identifier| self.values.get_mut(&identifier))
    }

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

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Identifier {
    scope: Scope,
    name: String,
}
impl Identifier {
    pub fn new(scope: Scope, name: String) -> Self {
        Self { scope, name }
    }

    pub fn in_parent_scope(&mut self) -> Option<Self> {
        self.scope.parent.clone().map(|parent_scope| {
            *self = Identifier {
                scope: *parent_scope,
                name: self.name.clone(),
            };
            self.clone()
        })
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    id: ScopeId,
}
impl Scope {
    pub fn new() -> Self {
        let id = NEXT_SCOPE_ID.fetch_add(
            1,
            // Use Ordering::SeqCst b/c I'm not confident that anything else would work properly, and this guarentees it.
            Ordering::SeqCst,
        );
        Scope { parent: None, id }
    }

    pub fn nest(&self) -> Self {
        let mut scope = Scope::new();
        scope.parent = Some(Box::new(self.clone()));
        scope
    }
}

type ScopeId = usize;
static NEXT_SCOPE_ID: AtomicUsize = AtomicUsize::new(0);
