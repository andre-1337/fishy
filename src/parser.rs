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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.sync();
                    panic!("{}", err);
                },
            }
        }

        stmts
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        if self.matches(vec![TokenType::Struct]) {
            return self.struct_decl();
        }

        if self.matches(vec![TokenType::Trait]) {
            return self.trait_decl();
        }

        if self.matches(vec![TokenType::Enum]) {
            return self.enum_decl();
        }

        if self.matches(vec![TokenType::Fn]) {
            return self.fn_decl();
        }

        if self.matches(vec![TokenType::Let]) {
            return self.let_decl();
        }

        match self.statement() {
            Ok(stmt) => Ok(stmt),
            Err(_) => {
                self.sync();
                Err(ParseError::new(
                    "Failed to parse declaration.",
                    self.previous(),
                ))
            }
        }
    }

    fn struct_decl(&mut self) -> ParseResult<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected struct name.")?.clone();

        let mut traits = vec![];
        if self.matches(vec![TokenType::With]) {
            loop {
                traits.push(self.parse_expr()?);
                if !self.matches(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::LeftBrace,
            "Expected '{' after struct name or traits.",
        )?;

        let mut fields = vec![];
        let mut methods = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if self.check(TokenType::Fn) {
                methods.push(self.fn_decl()?);
            } else {
                let field_name = self.consume(TokenType::Identifier, "Expected field name.")?.clone();
                self.consume(TokenType::Colon, "Expected ':' after field name.")?;
                let field_type = self.parse_type()?;
                fields.push((field_name.clone(), field_type));

                if !self.matches(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after struct body.")?;

        Ok(Stmt::StructStmt(StructStmt {
            name: name.clone(),
            traits,
            fields,
            methods,
        }))
    }

    fn trait_decl(&mut self) -> ParseResult<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected trait name.")?.clone();
        self.consume(TokenType::LeftBrace, "Expected '{' after trait name.")?;

        let mut methods = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.fn_decl()?);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after trait body.")?;

        Ok(Stmt::TraitStmt(TraitStmt {
            name,
            methods,
        }))
    }

    fn enum_decl(&mut self) -> ParseResult<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected enum name.")?.clone();

        self.consume(TokenType::LeftBrace, "Expected '{' after enum name.")?;

        let mut variants = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let variant_name = self.consume(TokenType::Identifier, "Expected variant name.")?.clone();
            let mut fields = vec![];

            if self.matches(vec![TokenType::LeftParen]) {
                loop {
                    let field_name = self.consume(TokenType::Identifier, "Expected field name.")?.clone();
                    self.consume(TokenType::Colon, "Expected ':' after field name.")?;
                    let field_type = self.parse_type()?;
                    fields.push((field_name.clone(), field_type));

                    if !self.matches(vec![TokenType::Comma]) {
                        break;
                    }
                }

                self.consume(TokenType::RightParen, "Expected ')' after variant fields.")?;
            }

            variants.push((variant_name.clone(), fields));

            if !self.matches(vec![TokenType::Comma]) {
                break;
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after enum body.")?;

        Ok(Stmt::EnumStmt(EnumStmt {
            name: name.clone(),
            variants,
        }))
    }

    fn fn_decl(&mut self) -> ParseResult<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected function name.")?.clone();

        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;

        let mut params = vec![];
        while !self.check(TokenType::RightParen) && !self.is_at_end() {
            let is_mut = self.matches(vec![TokenType::Mut]);
            let param_name = self.consume(TokenType::Identifier, "Expected parameter name.")?.clone();
            self.consume(TokenType::Colon, "Expected ':' after parameter name.")?;
            let param_type = self.parse_type()?;
            params.push((param_name.clone(), param_type, is_mut));

            if !self.matches(vec![TokenType::Comma]) {
                break;
            }
        }

        self.consume(
            TokenType::RightParen,
            "Expected ')' after function parameters.",
        )?;

        let return_type = if self.matches(vec![TokenType::Colon]) {
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
        }))
    }

    fn let_decl(&mut self) -> ParseResult<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected variable name.")?.clone();

        let mut type_ = None;
        if self.matches(vec![TokenType::Colon]) {
            type_ = Some(self.parse_type()?);
        }

        let mut initializer = None;
        if self.matches(vec![TokenType::Equal]) {
            initializer = Some(self.parse_expr()?);
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

    fn parse_type(&mut self) -> ParseResult<Type> {
        // struct types and function pointers are not supported
        // for the time being to keep things simple.
        // pointers are enough of a headache as it is.

        match self.peek().ttype {
            TokenType::LeftBracket => {
                self.advance();
                let inner = self.parse_type()?;
                self.consume(TokenType::Semicolon, "Expected ';' after array type.")?;
                let size = self.previous().value.parse().unwrap();
                self.consume(TokenType::RightBracket, "Expected ']' after array type.")?;

                Ok(Type::Array(Box::new(inner), size))
            }

            TokenType::Caret => {
                self.advance();
                let inner = self.parse_type()?;
                Ok(Type::Pointer(Box::new(inner)))
            }

            TokenType::Identifier => {
                let token = self.previous().clone();

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

                    _ => Err(self.error(&token, "Unrecognized type.")),
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
        if self.matches(vec![TokenType::If]) {
            return self.if_stmt();
        }

        if self.matches(vec![TokenType::While]) {
            return self.while_stmt();
        }

        if self.matches(vec![TokenType::For]) {
            return self.for_stmt();
        }

        if self.matches(vec![TokenType::Return]) {
            return self.return_stmt();
        }

        if self.matches(vec![TokenType::Break]) {
            return self.break_stmt();
        }

        self.expression_stmt()
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let condition = self.parse_expr()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.matches(vec![TokenType::Else]) {
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
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;
        let condition = self.parse_expr()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition.")?;

        let body = self.statement()?;

        Ok(Stmt::WhileStmt(WhileStmt { condition, body: Box::new(body) }))
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;

        let initializer = if self.matches(vec![TokenType::Semicolon]) {
            None
        } else if self.matches(vec![TokenType::Let]) {
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
        let keyword = self.previous().clone();
        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return value.")?;

        Ok(Stmt::ReturnStmt(ReturnStmt { keyword, value }))
    }

    fn break_stmt(&mut self) -> ParseResult<Stmt> {
        let keyword = self.previous().clone();
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

        if self.matches(vec![TokenType::Equal]) {
            let value = self.assignment()?;

            match &expr {
                Expr::VarExpr(VarExpr { name }) => {
                    Ok(Expr::AssignExpr(AssignExpr {
                        name: name.clone(),
                        value: Box::new(value),
                    }))
                }

                Expr::GetExpr(GetExpr { name, object }) => {
                    Ok(Expr::SetExpr(SetExpr {
                        name: name.clone(),
                        object: (*object).clone(),
                        value: Box::new(value),
                    }))
                }

                _ => {
                    let token = &self.previous().clone();
                    Err(self.error(token, "Invalid assignment target."))
                }
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.and()?;

        while self.matches(vec![TokenType::PipePipe]) {
            let operator = self.previous().clone();
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

        while self.matches(vec![TokenType::AmpersandAmpersand]) {
            let operator = self.previous().clone();
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

        while self.matches(vec![TokenType::EqualEqual, TokenType::ExclamationEqual]) {
            let operator = self.previous().clone();
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

        while self.matches(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
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

        while self.matches(vec![TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
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

        while self.matches(vec![TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
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
        if self.matches(vec![TokenType::Exclamation, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;

            Ok(Expr::UnaryExpr(UnaryExpr { operator, right: Box::new(right) }))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(vec![TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(vec![TokenType::Dot]) {
                let name = self.consume(TokenType::Identifier, "Expected property name.")?;
                expr = Expr::GetExpr(GetExpr {
                    name: name.clone(),
                    object: Box::new(expr),
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let mut arguments = vec![];

        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(self.parse_expr()?);

                if !self.matches(vec![TokenType::Comma]) {
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
        let token = &self.peek().clone();

        if self.matches(vec![TokenType::Identifier]) {
            self.advance();
            let literal = self.previous();

            return match literal.value.as_str() {
                "true" | "false" => Ok(Expr::LiteralExpr(LiteralExpr { value: literal.clone() })),
                _ => Ok(Expr::LiteralExpr(LiteralExpr { value: literal.clone() })),
            };
        }

        if self.matches(vec![TokenType::Number, TokenType::String]) {
            return Ok(Expr::LiteralExpr(LiteralExpr {
                value: self.previous().clone(),
            }));
        }

        if self.matches(vec![TokenType::Identifier]) {
            return Ok(Expr::VarExpr(VarExpr {
                name: self.previous().clone(),
            }));
        }

        if self.matches(vec![TokenType::LeftParen]) {
            let expr = self.parse_expr()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
            return Ok(Expr::GroupingExpr(GroupingExpr { expression: Box::new(expr) }));
        }

        Err(self.error(token, "Expected expression."))
    }

    fn matches(&mut self, types: Vec<TokenType>) -> bool {
        for ttype in types {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, ttype: TokenType, msg: &str) -> ParseResult<&Token> {
        if self.check(ttype) {
            return Ok(self.advance());
        }

        let token = self.peek().clone();
        Err(self.error(&token, msg))
    }

    fn check(&mut self, ttype: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().ttype == ttype
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().ttype == TokenType::Eof
    }

    fn peek(&mut self) -> &Token {
        if self.tokens.len() <= self.current {
            return &self.tokens[self.tokens.len() - 1];
        }

        &self.tokens[self.current]
    }

    fn previous(&mut self) -> &Token {
        if self.tokens.len() <= self.current {
            return &self.tokens.last().unwrap();
        }

        &self.tokens[self.current - 1]
    }

    fn error(&mut self, token: &Token, msg: &str) -> ParseError {
        ParseError::new(msg, token)
    }

    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().ttype == TokenType::Semicolon {
                return;
            }

            match self.peek().ttype {
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
        }
    }
}
