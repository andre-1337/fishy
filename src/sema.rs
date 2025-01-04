// the typechecker for fishy

use crate::{ast::*, error::FishyError, parser::Parser, symtab::TypeEnvironment, token::Token, types::*};

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

            Stmt::StructStmt(StructStmt { name, fields, .. }) => {
                // will be improved later
                let fields = fields.iter().map(|(name, typ)| (name.value.clone(), typ.clone())).collect();
                let typ = Type::Struct(name.value.clone(), fields, vec![], false);
                self.type_env.add(&name.value, typ);

                Ok(())
            }

            Stmt::LetStmt(LetStmt {
                name,
                type_,
                initializer,
            }) => {
                if let Some(init) = initializer {
                    let init_type = self.check_expression(init)?;
                    let typ = self.extract_type(name, type_, &init_type)?;

                    println!("{}: {:?}", name.value, typ);

                    if typ != init_type {
                        let fmt = format!(
                            "Type mismatch: expected {}, but got {} instead.",
                            typ, init_type
                        );
                        let string = fmt.as_str();
                        return Err(FishyError::new_typechecker_error(
                            self.parser.lexer.unit.clone(),
                            name.position.clone(),
                            string,
                            Some(format!("the initializer has type {}", init_type)),
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

    fn extract_type(&mut self, tok: &Token, type1: &Option<Type>, type2: &Type) -> TypeCheckResult<Type> {
        let typ = type1.as_ref().unwrap_or(&type2);

        if let Type::UserDefined(name) = typ {
            return match self.type_env.find(name.as_str()).cloned() {
                Some(typ) => Ok(typ),
                None => {
                    let fmt = format!("Type {} not found", typ);
                    let msg = fmt.as_str();
                    Err(FishyError::new_typechecker_error(self.parser.lexer.unit.clone(), tok.position.clone(), msg, Some(format!("make sure you defined the type '{}'", typ))))
                }
            };
        } else if let Type::Struct(name, _, _, is_trait) = typ {
            return match self.type_env.find(name.as_str()).cloned() {
                Some(typ) => Ok(typ),
                None => {
                    let fmt = format!("{} type {typ} not found", if *is_trait { "trait" } else { "struct" });
                    let msg = fmt.as_str();
                    Err(FishyError::new_typechecker_error(self.parser.lexer.unit.clone(), tok.position.clone(), msg, Some(format!("make sure you defined the type '{} {}'", if *is_trait { "trait" } else { "struct" }, name))))
                }
            }
        } else {
            return Ok(typ.clone());
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

            Expr::StructInitializerExpr(StructInitializerExpr { name, inferred_type, .. }) => {
                if let Some(inferred) = inferred_type {
                    return match self.type_env.find(&name.value).cloned() {
                        Some(typ) => Ok(typ),
                        None => Ok(inferred.clone())
                    };
                } else {
                    Ok(Type::new_user_type(&name.value))
                }
            }

            _ => Ok(Type::Void),
        }
    }
}
