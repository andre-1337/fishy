// the typechecker for fishy

use crate::{ast::{Expr, Module, Stmt}, symtab::TypeEnvironment, types::Type};

// this is here for the time being until fishy gets proper error reporting
#[derive(Debug, Clone)]
pub struct TypeCheckError(String);

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error while typechecking: {}", self.0)
    }
}

impl std::error::Error for TypeCheckError {}

#[derive(Debug, Clone)]
pub struct TypeChecker {
    type_env: TypeEnvironment<Type>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            type_env: TypeEnvironment::new()
        }
    }

    pub fn check_program(&mut self, module: &Module) -> Result<(), TypeCheckError> {
        self.type_env.enter_scope();

        for stmt in &module.statements {
            self.check_statement(stmt)?;
        }

        self.type_env.leave_scope();

        Ok(())
    }

    fn check_statement(&mut self, statement: &Stmt) -> Result<(), TypeCheckError> {
        Ok(())
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        Ok(Type::Void)
    }
}