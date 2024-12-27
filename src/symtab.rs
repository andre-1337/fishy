use std::collections::{BTreeMap, VecDeque};

// type environment implementation from `jun.codes` on the r/programminglanguages discord server

#[derive(Debug)]
pub struct TypeEnvironment<T> {
    scopes: VecDeque<BTreeMap<String, T>>,
}

impl<T> TypeEnvironment<T> {
    pub fn new() -> TypeEnvironment<T> {
        TypeEnvironment {
            scopes: VecDeque::new(),
        }
    }

    // push a new scope to the deque
    pub fn enter_scope(&mut self) {
        let scope = BTreeMap::new();
        self.scopes.push_front(scope);
    }

    // pop the topmost scope from the deque
    pub fn leave_scope(&mut self) {
        self.scopes.pop_front();
    }

    // get the depth of the global scope
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn find(&self, name: &str) -> Option<&T> {
        for scope in &self.scopes {
            if let Some(item) = scope.get(name) {
                return Some(item);
            }
        }

        None
    }

    pub fn add(&mut self, name: &str, id: T) {
        let scope = self
            .scopes
            .front_mut()
            .expect("TypeEnvironment::add has no scope.");
        scope.insert(name.to_string(), id);
    }

    pub fn remove(&mut self, name: &str) {
        let scope = self
            .scopes
            .front_mut()
            .expect("TypeEnvironment::remove has no scope.");
        scope.remove(name);
    }
}
