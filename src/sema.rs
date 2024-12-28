// the typechecker for fishy

use crate::{ast::*, symtab::TypeEnvironment, types::*};

// this is here for the time being until fishy gets proper error reporting
#[derive(Debug, Clone)]
pub struct TypeCheckError(String);

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeError: {}", self.0)
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
            type_env: TypeEnvironment::new(),
        }
    }

    pub fn check_program(
        &mut self,
        module: &Module,
    ) -> Result<TypeEnvironment<Type>, TypeCheckError> {
        self.type_env.enter_scope();
        for stmt in &module.statements {
            self.check_statement(stmt)?;
        }

        Ok(self.type_env.clone())
    }

    fn check_statement(&mut self, statement: &Stmt) -> Result<(), TypeCheckError> {
        match statement {
            Stmt::AliasStmt(AliasStmt { name, aliased_type }) => {
                self.type_env.add(&name.value, aliased_type.clone());
                Ok(())
            }

            Stmt::LetStmt(LetStmt {
                name,
                type_,
                initializer,
            }) => {
                if let Some(init) = initializer {
                    let init_type = self.check_expression(init)?;
                    let mut typ = type_.as_ref().unwrap_or(&init_type);

                    if let Type::UserDefined(name) = typ {
                        typ = self.type_env.find(name.as_str()).unwrap_or(&init_type);
                    }

                    if *typ != init_type {
                        return Err(TypeCheckError(format!(
                            "Type mismatch: expected {}, but got {} instead.",
                            typ, init_type
                        )));
                    }

                    self.type_env.add(&name.value, typ.clone());
                } else {
                    self.type_env.add(&name.value, Type::Void);
                }
                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        match expr {
            Expr::LiteralExpr(LiteralExpr { inferred_type, .. }) => {
                if let Some(inferred) = inferred_type {
                    return Ok(inferred.clone());
                } else {
                    Ok(Type::Void)
                }
            }

            _ => Ok(Type::Void),
        }
    }
}
