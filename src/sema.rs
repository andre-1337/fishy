// the typechecker for fishy

use crate::{ast::*, symtab::TypeEnvironment, types::{FnPtr, Type}};

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
        match statement {
            Stmt::LetStmt(LetStmt { name, type_, initializer }) => {
                if let Some(expr) = initializer {
                    let expr_type = self.check_expression(expr)?;

                    if type_.clone().unwrap() != expr_type {
                        return Err(TypeCheckError(format!("Type mismatch: expected {:?}, but got {:?} instead.", type_.clone().unwrap(), expr_type)));
                    }
                }

                self.type_env.add(&name.value, type_.clone().unwrap_or(Type::Void));
                Ok(())
            }

            Stmt::FnStmt(FnStmt { name, params, return_type, body, .. }) => {
                let fn_ptr = Type::FunctionPtr(FnPtr {
                    param_types: params.iter().map(|(_, typ)| typ.clone()).collect(),
                    return_type: Box::new(return_type.clone().unwrap_or(Type::Void))
                });

                self.type_env.add(&name.value, fn_ptr);

                self.type_env.enter_scope();
                for (param_name, param_type) in params {
                    self.type_env.add(&param_name.value, param_type.clone());
                }

                for stmt in &*body.fn_body_as_iter() {
                    self.check_statement(stmt)?;
                }
                self.type_env.leave_scope();

                Ok(())
            }

            _ => {
                Ok(())
            }
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        Ok(Type::Void)
    }
}