use crate::{
    ast::*,
    token::{Token, TokenType},
    types::Type,
};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    token: Token,
}

impl ParseError {
    pub fn new(msg: &str, token: &Token) -> ParseError {
        ParseError {
            msg: msg.to_string(),
            token: token.clone(),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "error at [{}:{}]: {}",
            self.token.position.line, self.token.position.column, self.msg
        )
    }
}

impl std::error::Error for ParseError {}

type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Module> {
        let mut stmts = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(Module::new("main".to_string(), stmts))
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        match self.current_token().ttype {
            TokenType::Struct => self.struct_decl(),
            TokenType::Trait => self.trait_decl(),
            TokenType::Enum => self.enum_decl(),
            TokenType::Fn => self.fn_decl(false),
            TokenType::Let => self.let_decl(),
            TokenType::Alias => self.alias_decl(),
            TokenType::Extern => self.extern_decl(),
            _ => match self.statement() {
                Ok(stmt) => Ok(stmt),
                Err(e) => {
                    self.sync();
                    Err(ParseError::new(&e.msg, &self.current_token()))
                }
            },
        }
    }

    fn struct_decl(&mut self) -> ParseResult<Stmt> {
        // advance passed the struct keyword
        self.advance();

        let name = self
            .consume(TokenType::Identifier, "Expected struct name.")?
            .clone();

        println!("name {:?}", name);
        let mut traits = vec![];
        if matches!(self.current_token().ttype, TokenType::With) {
            self.advance();

            dbg!("struct_decl (trait) {}", self.current_token());

            loop {
                if matches!(self.current_token().ttype, TokenType::Identifier) {
                    self.advance();
                    dbg!(self.tokens[self.current - 1].clone());
                    traits.push(Expr::VarExpr(VarExpr {
                        name: self.tokens[self.current - 1].clone(),
                    }));

                    if matches!(self.current_token().ttype, TokenType::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    return Err(ParseError::new(
                        "Expected trait name.",
                        &self.current_token(),
                    ));
                }
            }
        }
        println!("traits {:?}", traits);
        dbg!(self.current_token());

        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after struct name or traits.",
        )?;

        let mut fields = vec![];
        let mut methods = vec![];

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if self.check(TokenType::Fn) {
                methods.push(self.fn_decl(false)?);
                // if comma is present, skip over it
                if matches!(self.current_token().ttype, TokenType::Comma) {
                    self.advance();
                }
            } else {
                loop {
                    println!("field loop {:?}", self.current_token());

                    // break out if right brace is reached
                    if matches!(self.current_token().ttype, TokenType::RightBrace) {
                        break;
                    }

                    let field_name = self
                        .consume(TokenType::Identifier, "Expected field name.")?
                        .clone();

                    println!("field name {:?}", field_name);

                    self.consume(TokenType::Colon, "Expected ':' after field name.")?;

                    let field_type = self.parse_type()?;
                    println!("field type {:?}", field_type);

                    fields.push((field_name.clone(), field_type));

                    // break out if right brace is reached
                    if matches!(self.current_token().ttype, TokenType::RightBrace) {
                        break;
                    }

                    if matches!(self.current_token().ttype, TokenType::Comma) {
                        self.advance();
                        break;
                    }
                }
            }
        }

        println!("fields {:?}", fields);
        dbg!(self.current_token());

        self.consume(TokenType::RightBrace, "Expected '}' after struct body.")?;

        Ok(Stmt::StructStmt(StructStmt {
            name: name.clone(),
            traits,
            fields,
            methods,
        }))
    }

    fn trait_decl(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self
            .consume(TokenType::Identifier, "Expected trait name.")?
            .clone();
        self.consume(TokenType::LeftBrace, "Expected '{' after trait name.")?;

        let mut methods = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if matches!(self.current_token().ttype, TokenType::Fn) {
                self.advance();
                let name = self
                    .consume(TokenType::Identifier, "Expected function name.")?
                    .clone();

                self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;

                let mut params = vec![];
                while !self.check(TokenType::RightParen) && !self.is_at_end() {
                    dbg!(self.current_token());

                    let param_name = self
                        .consume(TokenType::Identifier, "Expected parameter name.")?
                        .clone();
                    dbg!(&param_name);
                    self.consume(TokenType::Colon, "Expected ':' after parameter name.")?;
                    let param_type = self.parse_type()?;
                    dbg!(&param_type);
                    params.push((param_name.clone(), param_type));

                    if matches!(self.current_token().ttype, TokenType::Comma) {
                        self.advance();
                    }
                }

                self.consume(
                    TokenType::RightParen,
                    "Expected ')' after function parameters.",
                )?;

                self.consume(TokenType::Colon, "Expect ':' after function parameters.")?;

                let return_type = self.parse_type()?;
                self.consume(
                    TokenType::Semicolon,
                    "Expected ';' after function declaration.",
                )?;

                methods.push(Stmt::FnStmt(FnStmt {
                    name,
                    params,
                    return_type: Some(return_type),
                    body: Box::new(Stmt::BlockStmt(BlockStmt { statements: vec![] })),
                    meta: FnMeta {
                        is_abstract: true,
                        is_pub: false,
                    },
                }))
            } else {
                return Err(ParseError::new(
                    "Expected function declaration in trait body.",
                    &self.current_token(),
                ));
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after trait body.")?;

        Ok(Stmt::TraitStmt(TraitStmt { name, methods }))
    }

    fn enum_decl(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self
            .consume(TokenType::Identifier, "Expected enum name.")?
            .clone();

        self.consume(TokenType::LeftBrace, "Expected '{' after enum name.")?;

        let mut variants = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let variant_name = self
                .consume(TokenType::Identifier, "Expected variant name.")?
                .clone();
            let mut fields = vec![];

            if matches!(self.current_token().ttype, TokenType::LeftParen) {
                self.advance();
                loop {
                    let field_name = self
                        .consume(TokenType::Identifier, "Expected field name.")?
                        .clone();
                    self.consume(TokenType::Colon, "Expected ':' after field name.")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name.clone(), field_type));

                    if !matches!(self.current_token().ttype, TokenType::Comma) {
                        break;
                    }
                }

                self.consume(TokenType::RightParen, "Expected ')' after variant fields.")?;
            }

            variants.push((variant_name.clone(), fields));

            if !matches!(self.current_token().ttype, TokenType::Comma) {
                self.advance();
                break;
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after enum body.")?;

        Ok(Stmt::EnumStmt(EnumStmt {
            name: name.clone(),
            variants,
        }))
    }

    fn fn_decl(&mut self, is_pub: bool) -> ParseResult<Stmt> {
        self.advance();
        dbg!(self.current_token());

        let name = self
            .consume(TokenType::Identifier, "Expected function name.")?
            .clone();

        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;

        let mut params = vec![];
        while !self.check(TokenType::RightParen) && !self.is_at_end() {
            dbg!(self.current_token());

            let param_name = self
                .consume(TokenType::Identifier, "Expected parameter name.")?
                .clone();
            dbg!(&param_name);
            self.consume(TokenType::Colon, "Expected ':' after parameter name.")?;
            let param_type = self.parse_type()?;
            dbg!(&param_type);
            params.push((param_name.clone(), param_type));

            if matches!(self.current_token().ttype, TokenType::Comma) {
                self.advance();
            }
        }

        dbg!(&params);
        dbg!(self.current_token());
        self.consume(
            TokenType::RightParen,
            "Expected ')' after function parameters.",
        )?;

        let return_type = if matches!(self.current_token().ttype, TokenType::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expected '{' before function body.")?;

        let body = self.body()?;

        Ok(Stmt::FnStmt(FnStmt {
            name: name.clone(),
            params,
            return_type,
            body: Box::new(body),
            meta: FnMeta {
                is_abstract: false,
                is_pub,
            },
        }))
    }

    fn let_decl(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self
            .consume(TokenType::Identifier, "Expected variable name.")?
            .clone();

        println!("name {:?}", name);
        let mut type_ = None;
        if matches!(self.current_token().ttype, TokenType::Colon) {
            self.advance();
            type_ = Some(self.parse_type()?);
        }

        let mut initializer = None;

        println!("current {:?}", self.current_token());
        if self.current_token().ttype == TokenType::Equal {
            self.advance();
            initializer = Some(self.parse_expr()?);
            println!("init {:?}", initializer);
        }

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;

        Ok(Stmt::LetStmt(LetStmt {
            name: name.clone(),
            type_,
            initializer,
        }))
    }

    fn alias_decl(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self
            .consume(TokenType::Identifier, "Expected alias name.")?
            .clone();

        self.consume(TokenType::Equal, "Expected '=' after alias name.")?;

        let aliased_type = self.parse_type()?;

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after alias declaration.",
        )?;

        Ok(Stmt::AliasStmt(AliasStmt { name, aliased_type }))
    }

    fn extern_decl(&mut self) -> ParseResult<Stmt> {
        self.advance();
        self.consume(TokenType::Fn, "Expected 'fn' after 'extern'.")?;
        let name = self
            .consume(TokenType::Identifier, "Expected function name.")?
            .clone();
        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;
        let mut param_types = vec![];
        loop {
            if self.check(TokenType::RightParen) {
                break;
            }

            let param_type = self.parse_type()?;
            param_types.push(param_type);

            if matches!(self.current_token().ttype, TokenType::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.consume(TokenType::RightParen, "Expeced ')' after parameter types.")?;
        self.consume(TokenType::Arrow, "Expected '->' after parameter types.")?;
        let return_type = self.parse_type()?;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after extern function declaration.",
        )?;

        Ok(Stmt::ExternFnStmt(ExternFnStmt {
            name,
            return_type,
            param_types,
        }))
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        // struct types and function pointers are not supported
        // for the time being to keep things simple.
        // pointers are enough of a headache as it is

        match self.current_token().ttype {
            TokenType::LeftBracket => {
                self.advance();
                dbg!(self.current_token());
                let inner = self.parse_type()?;
                self.consume(TokenType::Semicolon, "Expected ';' after array type.")?;
                let size = self.current_token().value.clone();

                let size = if size == "?" {
                    0
                } else {
                    size.parse().unwrap()
                };

                self.advance();
                self.consume(TokenType::RightBracket, "Expected ']' after array type.")?;

                Ok(Type::Array(Box::new(inner), size))
            }

            TokenType::Caret => {
                self.advance();
                dbg!(&self.current_token());
                let is_mut_ptr = if matches!(self.current_token().ttype, TokenType::Mut) {
                    self.advance();
                    true
                } else if matches!(self.current_token().ttype, TokenType::Const) {
                    self.advance();
                    false
                } else {
                    Err(ParseError::new(
                        "Expected 'mut' or 'const' after '^'.",
                        &self.current_token(),
                    ))?
                };

                let inner = self.parse_type()?;
                Ok(Type::Pointer(Box::new(inner), is_mut_ptr))
            }

            TokenType::Ampersand => {
                self.advance();
                let is_mut_ref = if matches!(self.current_token().ttype, TokenType::Mut) {
                    self.advance();
                    true
                } else {
                    false
                };

                let inner = self.parse_type()?;
                Ok(Type::Reference(Box::new(inner), is_mut_ref))
            }

            TokenType::Fn => {
                self.advance();
                self.consume(TokenType::LeftParen, "Expected '(' after 'fn'.")?;
                let mut param_types = vec![];
                loop {
                    if self.check(TokenType::RightParen) {
                        break;
                    }

                    let param_type = self.parse_type()?;
                    param_types.push(param_type);

                    if matches!(self.current_token().ttype, TokenType::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.consume(TokenType::RightParen, "Expeced ')' after parameter types.")?;
                self.consume(TokenType::Arrow, "Expected '->' after parameter types.")?;
                let return_type = self.parse_type()?;
                Ok(Type::FunctionPtr(Box::new(return_type), param_types))
            }

            TokenType::Identifier => {
                println!("id {:?}", self.current_token());
                let token = self.current_token().clone();
                self.advance();

                match token.value.as_str() {
                    "i8" => Ok(Type::Int(8, true)),
                    "i16" => Ok(Type::Int(16, true)),
                    "i32" => Ok(Type::Int(32, true)),
                    "i64" => Ok(Type::Int(64, true)),

                    "u8" => Ok(Type::Int(8, false)),
                    "u16" => Ok(Type::Int(16, false)),
                    "u32" => Ok(Type::Int(32, false)),
                    "u64" => Ok(Type::Int(64, false)),

                    "f16" => Ok(Type::Float(16)),
                    "f32" => Ok(Type::Float(32)),
                    "f64" => Ok(Type::Float(64)),

                    "bool" => Ok(Type::Boolean),

                    "void" => Ok(Type::Void),

                    _ => Ok(Type::UserDefined(token.value)),
                }
            }

            _ => panic!("Unrecognized token for type."),
        }
    }

    fn body(&mut self) -> ParseResult<Stmt> {
        let mut statements = vec![];

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block.")?;

        Ok(Stmt::BlockStmt(BlockStmt { statements }))
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        match self.current_token().ttype {
            TokenType::If => self.if_stmt(),
            TokenType::While => self.while_stmt(),
            TokenType::For => self.for_stmt(),
            TokenType::Return => self.return_stmt(),
            TokenType::Break => self.break_stmt(),
            _ => self.expression_stmt(),
        }
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let condition = self.parse_expr()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if matches!(self.current_token().ttype, TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::IfStmt(IfStmt {
            condition,
            then_branch: Box::new(then_branch),
            else_branch,
        }))
    }

    fn while_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;
        let condition = self.parse_expr()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition.")?;

        let body = self.statement()?;

        Ok(Stmt::WhileStmt(WhileStmt {
            condition,
            body: Box::new(body),
        }))
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;

        let initializer = if matches!(self.current_token().ttype, TokenType::Semicolon) {
            None
        } else if matches!(self.current_token().ttype, TokenType::Let) {
            Some(self.let_decl()?)
        } else {
            Some(self.expression_stmt()?)
        };

        let condition = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        self.consume(TokenType::Semicolon, "Expected ';' after loop condition.")?;

        let increment = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        self.consume(TokenType::RightParen, "Expected ')' after for clauses.")?;

        let body = self.body()?;

        Ok(Stmt::ForStmt(ForStmt {
            initializer: Box::new(initializer.expect("Failed to parse for loop initializer.")),
            condition: condition.expect("Failed to parse for loop condition."),
            increment: increment.expect("Failed to parse for loop increment."),
            body: Box::new(body),
        }))
    }

    fn return_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let keyword = self.current_token().clone();
        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return value.")?;

        Ok(Stmt::ReturnStmt(ReturnStmt { keyword, value }))
    }

    fn break_stmt(&mut self) -> ParseResult<Stmt> {
        let keyword = self.current_token().clone();
        self.consume(TokenType::Semicolon, "Expected ';' after 'break'.")?;

        Ok(Stmt::BreakStmt(BreakStmt { keyword }))
    }

    fn expression_stmt(&mut self) -> ParseResult<Stmt> {
        let expression = self.parse_expr()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;

        Ok(Stmt::ExprStmt(ExprStmt { expression }))
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.or()?;
        println!("assign {:?}", expr);

        if matches!(self.current_token().ttype, TokenType::Equal) {
            self.advance();
            let value = self.assignment()?;

            match &expr {
                Expr::VarExpr(VarExpr { name }) => Ok(Expr::AssignExpr(AssignExpr {
                    name: name.clone(),
                    value: Box::new(value),
                })),

                Expr::GetExpr(GetExpr { name, object }) => Ok(Expr::SetExpr(SetExpr {
                    name: name.clone(),
                    object: (*object).clone(),
                    value: Box::new(value),
                })),

                _ => {
                    let token = &self.current_token().clone();
                    Err(self.error(token, "Invalid assignment target."))
                }
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.and()?;
        println!("or {:?}", expr);
        while matches!(self.current_token().ttype, TokenType::PipePipe) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.and()?;
            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.equality()?;
        println!("and {:?}", expr);
        while matches!(self.current_token().ttype, TokenType::AmpersandAmpersand) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.equality()?;
            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;
        println!("eq {:?}", expr);

        while matches!(
            self.current_token().ttype,
            TokenType::EqualEqual | TokenType::ExclamationEqual
        ) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.comparison()?;

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;
        println!("comp {:?}", expr);

        while matches!(
            self.current_token().ttype,
            TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual
        ) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.term()?;

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;
        println!("term {:?}", expr);
        while matches!(
            self.current_token().ttype,
            TokenType::Plus | TokenType::Minus
        ) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.factor()?;

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;
        println!("factor {:?}", expr);
        while matches!(
            self.current_token().ttype,
            TokenType::Star | TokenType::Slash
        ) {
            self.advance();
            let operator = self.current_token().clone();
            let right = self.unary()?;

            expr = Expr::BinaryExpr(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        println!("unary {:?}", self.current_token());
        if matches!(
            self.current_token().ttype,
            TokenType::Exclamation | TokenType::Minus
        ) {
            self.advance();
            let operator = self.current_token().clone();
            self.advance();
            let right = self.unary()?;
            println!("op {:?}", operator);
            println!("right {:?}", right);
            Ok(Expr::UnaryExpr(UnaryExpr {
                operator,
                right: Box::new(right),
            }))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.primary()?;
        println!("call {:?}", expr);
        loop {
            if matches!(self.current_token().ttype, TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if matches!(self.current_token().ttype, TokenType::Dot) {
                self.advance();
                let name = self.consume(TokenType::Identifier, "Expected property name.")?;
                expr = Expr::GetExpr(GetExpr {
                    name: name.clone(),
                    object: Box::new(expr),
                });
            } else {
                break;
            }
        }
        println!("call {:?}", expr);
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let mut arguments = vec![];
        self.consume(TokenType::LeftParen, "Expected '(' after callee.")?;
        if !self.check(TokenType::RightParen) {
            loop {
                if matches!(self.current_token().ttype, TokenType::RightParen) {
                    break;
                }

                arguments.push(self.parse_expr()?);

                if matches!(self.current_token().ttype, TokenType::Comma) {
                    self.advance();
                }

                if matches!(self.current_token().ttype, TokenType::RightParen) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expected ')' after arguments.")?;

        Ok(Expr::CallExpr(CallExpr {
            callee: Box::new(callee),
            arguments,
        }))
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        println!("prim {:?}", self.current_token());
        let token = self.current_token().clone();
        self.advance();

        match token.ttype {
            TokenType::Number => Ok(Expr::LiteralExpr(LiteralExpr {
                value: token.clone(),
            })),

            TokenType::String => Ok(Expr::LiteralExpr(LiteralExpr {
                value: token.clone(),
            })),

            TokenType::Identifier => {
                dbg!(self.current_token());
                let name = token.clone();

                if matches!(self.current_token().ttype, TokenType::LeftBrace) {
                    self.advance();
    
                    let mut arguments = vec![];
                    loop {
                        let field_name = self
                            .consume(TokenType::Identifier, "Expected field name.")?
                            .clone();
                        self.consume(TokenType::Colon, "Expected ':' after field name.")?;
                        let field_value = self.parse_expr()?;
    
                        arguments.push((field_name.clone(), field_value));
    
                        if matches!(self.current_token().ttype, TokenType::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
    
                    self.consume(
                        TokenType::RightBrace,
                        "Expected '}' after struct initializer literal.",
                    )?;
    
                    return Ok(Expr::StructInitializerExpr(StructInitializerExpr { name, arguments }));
                } else {
                    // otherwise it's just a variable reference
                    return Ok(Expr::VarExpr(VarExpr {
                        name: token.clone(),
                    }));
                }
            }

            TokenType::LeftParen => {
                let expr = self.parse_expr()?;
                self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
                Ok(Expr::GroupingExpr(GroupingExpr {
                    expression: Box::new(expr),
                }))
            }

            TokenType::LeftBracket => {
                let mut values = vec![];
                loop {
                    if matches!(self.current_token().ttype, TokenType::RightBracket) {
                        break;
                    }

                    values.push(self.parse_expr()?);

                    if matches!(self.current_token().ttype, TokenType::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.consume(TokenType::RightBracket, "Expected ']' after array literal.")?;

                Ok(Expr::TupleExpr(TupleExpr { values }))
            }

            _ => Err(self.error(&token, "Expected expression.")),
        }
    }

    fn consume(&mut self, ttype: TokenType, msg: &str) -> ParseResult<Token> {
        let token = self.current_token().clone();
        if self.check(ttype) {
            self.current += 1;
            return Ok(token);
        }
        dbg!(self.current_token());
        Err(self.error(&token, msg))
    }

    fn check(&mut self, ttype: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.current_token().ttype == ttype
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn is_at_end(&mut self) -> bool {
        self.current_token().ttype == TokenType::Eof
    }

    fn current_token(&mut self) -> &Token {
        &self.tokens[self.current]
    }

    fn error(&mut self, token: &Token, msg: &str) -> ParseError {
        ParseError::new(msg, token)
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.current_token().ttype == TokenType::Semicolon {
                return;
            }

            match self.current_token().ttype {
                TokenType::If
                | TokenType::Else
                | TokenType::While
                | TokenType::For
                | TokenType::Return
                | TokenType::Break
                | TokenType::Struct
                | TokenType::Alias
                | TokenType::Enum
                | TokenType::Fn
                | TokenType::Extern
                | TokenType::Use
                | TokenType::In
                | TokenType::As
                | TokenType::Pub
                | TokenType::Const
                | TokenType::Module
                | TokenType::Trait
                | TokenType::Match
                | TokenType::Mut => return,

                _ => {}
            }

            self.advance();
        }
    }
}
