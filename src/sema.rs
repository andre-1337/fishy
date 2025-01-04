// the typechecker for fishy

use crate::{
    ast::*,
    error::{FishyError, TypeCheckError},
    parser::Parser,
    symtab::TypeEnvironment,
    token::Token,
    types::*,
};

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

            Stmt::StructStmt(StructStmt {
                name,
                fields,
                methods,
                ..
            }) => {
                // will be improved later
                self.type_env.enter_scope();
                let fields = fields
                    .iter()
                    .map(|(name, typ)| (name.value.clone(), typ.clone()))
                    .collect();

                let mut methds = vec![];
                for method in methods {
                    // sorry, don't really know any better way to do this
                    if let Stmt::FnStmt(FnStmt {
                        name,
                        params,
                        return_type,
                        body,
                        ..
                    }) = method
                    {
                        // define parameters to be usable inside functions
                        self.type_env.enter_scope();
                        let mut param_types = vec![];
                        for (param, typ) in params {
                            param_types.push(typ.clone());
                            self.type_env.add(&param.value, typ.clone());
                        }

                        // leave the scope so that the parameters don't "leak" outside the function
                        self.type_env.leave_scope();

                        // check the return type
                        let mut ret = Type::new_void();
                        if let Stmt::BlockStmt(BlockStmt { statements }) = body.as_ref() {
                            for stmt in statements {
                                if let Some(ret_type) = return_type {
                                    if let Stmt::ReturnStmt(ReturnStmt { keyword, value }) = stmt {
                                        if let Some(value) = value {
                                            println!("current function: {}", name.value);
                                            let value_type = self.check_expression(value)?;
                                            let typ = self.extract_type(
                                                keyword,
                                                &return_type,
                                                &value_type,
                                            )?;

                                            if typ != value_type {
                                                let fmt = format!(
                                                    "Type mismatch: expected {}, but got {} instead.",
                                                    typ, value_type
                                                );
                                                let string = fmt.as_str();
                                                return Err(FishyError::new_typechecker_error(
                                                    self.parser.lexer.unit.clone(),
                                                    keyword.position.clone(),
                                                    string,
                                                    Some(format!(
                                                        "the return value has type {}",
                                                        value_type
                                                    )),
                                                ));
                                            } else {
                                                ret = typ;
                                            }
                                        }
                                    } else {
                                        // don't throw an error if the annotated type is 'void'
                                        if ret_type == &Type::new_void() {
                                            if let Stmt::ReturnStmt(ReturnStmt { keyword, value }) =
                                                stmt
                                            {
                                                if value.is_some() {
                                                    return Err(FishyError::TypeChecker(TypeCheckError {
                                                        src: self.parser.lexer.unit.clone(),
                                                        location: keyword.position.clone(),
                                                        message: "Function must not return a value".to_string(),
                                                        hint: Some(format!("the expected return type is 'void', but the function returns type '{}'", self.check_expression(value.as_ref().unwrap())?))
                                                    }));
                                                } else {
                                                    return Ok(());
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    // if there's no annotated return type, infer the type from the return statement
                                    println!("current function: {}", name.value);
                                    if let Stmt::ReturnStmt(ReturnStmt { value, .. }) = stmt {
                                        if let Some(value) = value {
                                            ret = self.check_expression(value)?;
                                        } else {
                                            // or just return void if there's no return value
                                            ret = Type::new_void();
                                        }
                                    }
                                }
                            }
                        }

                        let typ = Type::FunctionPtr(FnPtr {
                            return_type: Box::new(ret),
                            param_types,
                        });
                        methds.push((name.value.clone(), typ));
                    }
                }
                self.type_env.leave_scope();

                let typ = Type::Struct(name.value.clone(), fields, methds, false);
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

            // welcome to spaghetti land, fellas!
            // (someone make this pretty please, i'm too dumb to do it)
            Stmt::FnStmt(FnStmt {
                name,
                params,
                return_type,
                body,
                ..
            }) => {
                // define parameters to be usable inside functions
                self.type_env.enter_scope();
                let mut param_types = vec![];
                for (param, typ) in params {
                    param_types.push(typ.clone());
                    self.type_env.add(&param.value, typ.clone());
                }

                // leave the scope so that the parameters don't "leak" outside the function
                self.type_env.leave_scope();

                // check the return type
                let mut ret = Type::new_void();
                if let Stmt::BlockStmt(BlockStmt { statements }) = body.as_ref() {
                    for stmt in statements {
                        if let Some(ret_type) = return_type {
                            if let Stmt::ReturnStmt(ReturnStmt { keyword, value }) = stmt {
                                if let Some(value) = value {
                                    println!("current function: {}", name.value);
                                    let value_type = self.check_expression(value)?;
                                    let typ =
                                        self.extract_type(keyword, &return_type, &value_type)?;

                                    if typ != value_type {
                                        let fmt = format!(
                                            "Type mismatch: expected {}, but got {} instead.",
                                            typ, value_type
                                        );
                                        let string = fmt.as_str();
                                        return Err(FishyError::new_typechecker_error(
                                            self.parser.lexer.unit.clone(),
                                            keyword.position.clone(),
                                            string,
                                            Some(format!(
                                                "the return value has type {}",
                                                value_type
                                            )),
                                        ));
                                    } else {
                                        ret = typ;
                                    }
                                }
                            } else {
                                // don't throw an error if the annotated type is 'void'
                                if ret_type == &Type::new_void() {
                                    if let Stmt::ReturnStmt(ReturnStmt { keyword, value }) = stmt {
                                        if value.is_some() {
                                            return Err(FishyError::TypeChecker(TypeCheckError {
                                                src: self.parser.lexer.unit.clone(),
                                                location: keyword.position.clone(),
                                                message: "Function must not return a value".to_string(),
                                                hint: Some(format!("the expected return type is 'void', but the function returns type '{}'", self.check_expression(value.as_ref().unwrap())?))
                                            }));
                                        } else {
                                            return Ok(());
                                        }
                                    }
                                }
                            }
                        } else {
                            // if there's no annotated return type, infer the type from the return statement
                            println!("current function: {}", name.value);
                            if let Stmt::ReturnStmt(ReturnStmt { value, .. }) = stmt {
                                if let Some(value) = value {
                                    ret = self.check_expression(value)?;
                                } else {
                                    // or just return void if there's no return value
                                    ret = Type::new_void();
                                }
                            }
                        }
                    }
                }

                let typ = Type::FunctionPtr(FnPtr {
                    return_type: Box::new(ret),
                    param_types,
                });
                self.type_env.add(&name.value, typ);

                Ok(())
            }

            _ => Ok(()),
        }
    }

    fn extract_type(
        &mut self,
        tok: &Token,
        type1: &Option<Type>,
        type2: &Type,
    ) -> TypeCheckResult<Type> {
        let typ = type1.as_ref().unwrap_or(&type2);

        if let Type::UserDefined(name) = typ {
            return match self.type_env.find(name.as_str()).cloned() {
                Some(typ) => Ok(typ),
                None => {
                    let fmt = format!("Type {} not found", typ);
                    let msg = fmt.as_str();
                    Err(FishyError::new_typechecker_error(
                        self.parser.lexer.unit.clone(),
                        tok.position.clone(),
                        msg,
                        Some(format!("make sure you defined the type '{}'", typ)),
                    ))
                }
            };
        } else if let Type::Struct(name, _, _, is_trait) = typ {
            return match self.type_env.find(name.as_str()).cloned() {
                Some(typ) => Ok(typ),
                None => {
                    let fmt = format!(
                        "{} type {typ} not found",
                        if *is_trait { "trait" } else { "struct" }
                    );
                    let msg = fmt.as_str();
                    Err(FishyError::new_typechecker_error(
                        self.parser.lexer.unit.clone(),
                        tok.position.clone(),
                        msg,
                        Some(format!(
                            "make sure you defined the type '{} {}'",
                            if *is_trait { "trait" } else { "struct" },
                            name
                        )),
                    ))
                }
            };
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

            Expr::StructInitializerExpr(StructInitializerExpr {
                name,
                inferred_type,
                ..
            }) => {
                if let Some(inferred) = inferred_type {
                    return match self.type_env.find(&name.value).cloned() {
                        Some(typ) => Ok(typ),
                        None => Ok(inferred.clone()),
                    };
                } else {
                    Ok(Type::new_user_type(&name.value))
                }
            }

            _ => Ok(Type::Void),
        }
    }
}
