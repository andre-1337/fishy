// the typechecker for fishy

use crate::{ast::*, error::FishyError, parser::Parser, symtab::TypeEnvironment, types::*};

type TypeCheckResult<T> = Result<T, FishyError>;

#[derive(Debug)]
pub struct TypeChecker {
    parser: Parser,
    type_env: TypeEnvironment<Type>,
}

impl TypeChecker {
    pub fn new(parser: Parser) -> TypeChecker {
        TypeChecker {
            parser,
            type_env: TypeEnvironment::new(),
        }
    }

    pub fn check_program(&mut self, module: &Module) -> TypeCheckResult<TypeEnvironment<Type>> {
        self.type_env.enter_scope();
        for stmt in &module.statements {
            self.check_statement(stmt)?;
        }

        Ok(self.type_env.clone())
    }

    fn check_statement(&mut self, statement: &Stmt) -> TypeCheckResult<()> {
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
                        let fmt = format!(
                            "Type mismatch: expected {}, but got {} instead.",
                            typ, init_type
                        );
                        let string = fmt.as_str();
                        return Err(FishyError::new_typechecker_error(
                            self.parser.lexer.unit.clone(),
                            name.position.clone(),
                            string,
                            None,
                        ));
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

    fn check_expression(&mut self, expr: &Expr) -> TypeCheckResult<Type> {
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
