use std::collections::HashMap;

use crate::{ast::*, types::Type};

// really hacky symtab. this will be changed for a better implementation soon

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<String, Type>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn declare_symbols(&mut self, module: &Module) {
        for stmt in &module.statements {
            match stmt {
                Stmt::StructStmt(StructStmt {
                    name,
                    fields,
                    methods,
                    ..
                }) => {
                    let fields = fields
                        .iter()
                        .map(|(name, typ)| (name.value.clone(), typ.clone()))
                        .collect::<Vec<(String, Type)>>();
                    let methods = methods.iter().map(|method| {
                        match method {
                            Stmt::FnStmt(FnStmt { name, params, return_type, .. }) => {
                                let params = params.iter().map(|(_, typ)| typ.clone()).collect::<Vec<Type>>();
                                return (name.value.clone(), params, return_type.clone().unwrap_or(Type::Void));
                            }

                            _ => panic!("YOU SHOULDN'T BE ABLE TO SEE THIS; PLEASE OPEN AN ISSUE IN THE REPO!")
                        }
                    }).collect::<Vec<(String, Vec<Type>, Type)>>();

                    self.symbols
                        .insert(name.value.clone(), Type::Struct(fields, methods));
                }

                Stmt::FnStmt(FnStmt { name, params, return_type, .. }) => {
                    let params = params.iter().map(|param| param.1.clone()).collect::<Vec<Type>>();
                    self.symbols.insert(name.value.clone(), Type::FunctionPtr(Box::new(return_type.as_ref().unwrap().clone()), params));
                }

                Stmt::ExternFnStmt(ExternFnStmt { name, param_types, return_type }) => {
                    self.symbols.insert(name.value.clone(), Type::FunctionPtr(Box::new(return_type.clone()), param_types.to_vec()));
                }

                Stmt::LetStmt(LetStmt { name, type_, .. }) => {
                    self.symbols
                        .insert(name.value.clone(), type_.clone().unwrap());
                }

                Stmt::TraitStmt(TraitStmt { name, methods }) => {
                    let methods = methods.iter().map(|method| {
                        match method {
                            Stmt::FnStmt(FnStmt { name, params, return_type, .. }) => {
                                let params = params.iter().map(|(_, typ)| typ.clone()).collect::<Vec<Type>>();
                                return (name.value.clone(), params, return_type.clone().unwrap());
                            }

                            _ => panic!("YOU SHOULDN'T BE ABLE TO SEE THIS; PLEASE OPEN AN ISSUE IN THE REPO!")
                        }
                    }).collect::<Vec<(String, Vec<Type>, Type)>>();

                    // yes, traits are being treated as struct types here, but that's irrelevant for the time being
                    // they will (maybe) have their own "type" soon
                    self.symbols.insert(name.value.clone(), Type::Struct(vec![], methods));
                }

                // enums aren't treated yet

                Stmt::AliasStmt(AliasStmt { name, aliased_type }) => {
                    self.symbols.insert(name.value.clone(), aliased_type.clone());
                }

                _ => {}
            }
        }
    }
}
