use crate::token::{Position, Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    pub source: String,
    pub position: usize,
    pub line: usize,
    pub column: usize,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Lexer {
            source,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.source.chars().nth(self.position);
        if let Some('\n') = c {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.position += 1;
        c
    }

    pub fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.position)
    }

    pub fn previous(&self) -> Option<char> {
        self.source.chars().nth(self.position - 1)
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    pub fn lex_identifier(&mut self) -> String {
        let mut ident = String::new();
        
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.next();
            } else {
                break;
            }
        }

        ident
    }

    pub fn lex_number(&mut self) -> String {
        let mut number = String::new();
        let mut is_float = false;

        if let Some('0') = self.peek() {
            number.push('0');
            self.next();

            if let Some(c) = self.peek() {
                match c {
                    'x' | 'X' => {
                        number.push(c);
                        self.next();
                        while let Some(c) = self.peek() {
                            if c.is_digit(16) && c.is_ascii_hexdigit() {
                                number.push(c);
                                self.next();
                            } else {
                                break;
                            }
                        }
                        return number;
                    }

                    'o' | 'O' => {
                        number.push(c);
                        self.next();
                        while let Some(c) = self.peek() {
                            if c.is_digit(8) {
                                number.push(c);
                                self.next();
                            } else {
                                break;
                            }
                        }
                        return number;
                    }

                    'b' | 'B' => {
                        number.push(c);
                        self.next();
                        while let Some(c) = self.peek() {
                            if c.is_digit(2) {
                                number.push(c);
                                self.next();
                            } else {
                                break;
                            }
                        }
                        return number;
                    }

                    _ => {}
                }
            }
        }

        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                number.push(c);
                self.next();
            } else if c == '.' && !is_float {
                is_float = true;
                number.push(c);
                self.next();
            } else {
                break;
            }
        }

        if is_float {
            while let Some(c) = self.peek() {
                if c.is_digit(10) {
                    number.push(c);
                    self.next();
                } else {
                    break;
                }
            }
        }

        number
    }

    pub fn lex_string(&mut self) -> String {
        let mut string = String::new();
        self.next();
        while let Some(c) = self.peek() {
            if c == '"' {
                self.next();
                break;
            } else {
                string.push(c);
                self.next();
            }
        }
        string
    }

    pub fn lex_char(&mut self) -> String {
        let mut string = String::new();
        self.next();
        while let Some(c) = self.peek() {
            if c == '\'' {
                self.next();
                break;
            } else {
                string.push(c);
                self.next();
            }
        }
        string
    }

    fn make_token(&mut self, ttype: TokenType, value: String) -> Token {
        Token::new(ttype, Position::new(self.line, self.column), value)
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.skip_whitespace();
            } else if c.is_alphabetic() || c == '_' {
                let ident = self.lex_identifier();

                let ttype = match ident.as_str() {
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    "for" => TokenType::For,
                    "return" => TokenType::Return,
                    "break" => TokenType::Break,
                    "struct" => TokenType::Struct,
                    "alias" => TokenType::Alias,
                    "enum" => TokenType::Enum,
                    "fn" => TokenType::Fn,
                    "extern" => TokenType::Extern,
                    "use" => TokenType::Use,
                    "in" => TokenType::In,
                    "as" => TokenType::As,
                    "pub" => TokenType::Pub,
                    "const" => TokenType::Const,
                    "module" => TokenType::Module,
                    "trait" => TokenType::Trait,
                    "match" => TokenType::Match,
                    "mut" => TokenType::Mut,
                    "let" => TokenType::Let,
                    "with" => TokenType::With,

                    _ => TokenType::Identifier,
                };

                tokens.push(self.make_token(ttype, ident));
            } else if c.is_numeric() {
                let number = self.lex_number();
                tokens.push(self.make_token(TokenType::Number, number));
            } else {
                match c {
                    '"' => {
                        let string = self.lex_string();
                        tokens.push(self.make_token(TokenType::String, string));
                    }

                    '\'' => {
                        let string = self.lex_char();
                        tokens.push(self.make_token(TokenType::Char, string));
                    }

                    '+' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::PlusEqual, "+=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Plus, "+".to_string()));
                        }
                    }

                    '-' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::MinusEqual, "-=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Minus, "-".to_string()));
                        }
                    }

                    '*' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::StarEqual, "*=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Star, "*".to_string()));
                        }
                    }

                    '/' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::SlashEqual, "/=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Slash, "/".to_string()));
                        }
                    }

                    '(' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::LeftParen, "(".to_string()));
                    }

                    ')' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::RightParen, ")".to_string()));
                    }

                    '{' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::LeftBrace, "{".to_string()));
                    }

                    '}' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::RightBrace, "}".to_string()));
                    }

                    '[' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::LeftBracket, "[".to_string()));
                    }

                    ']' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::RightBracket, "]".to_string()));
                    }

                    ',' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Comma, ",".to_string()));
                    }

                    '.' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Dot, ".".to_string()));
                    }

                    ';' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Semicolon, ";".to_string()));
                    }

                    ':' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Colon, ":".to_string()));
                    }

                    '?' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Question, "?".to_string()));
                    }

                    '%' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::PercentEqual, "%=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Percent, "%".to_string()));
                        }
                    }

                    '^' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::CaretEqual, "^=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Caret, "^".to_string()));
                        }
                    }

                    '~' => {
                        self.next();
                        tokens.push(self.make_token(TokenType::Tilde, "~".to_string()));
                    }

                    '=' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::EqualEqual, "==".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Equal, "=".to_string()));
                        }
                    }

                    '!' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(
                                self.make_token(TokenType::ExclamationEqual, "!=".to_string()),
                            );
                        } else {
                            tokens.push(self.make_token(TokenType::Exclamation, "!".to_string()));
                        }
                    }

                    '<' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::LessEqual, "<=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Less, "<".to_string()));
                        }
                    }

                    '>' => {
                        self.next();
                        if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::GreaterEqual, ">=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Greater, ">".to_string()));
                        }
                    }

                    '&' => {
                        self.next();
                        if let Some('&') = self.peek() {
                            self.next();
                            tokens.push(
                                self.make_token(TokenType::AmpersandAmpersand, "&&".to_string()),
                            );
                        } else if let Some('=') = self.peek() {
                            self.next();
                            tokens
                                .push(self.make_token(TokenType::AmpersandEqual, "&=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Ampersand, "&".to_string()));
                        }
                    }

                    '|' => {
                        self.next();
                        if let Some('|') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::PipePipe, "||".to_string()));
                        } else if let Some('=') = self.peek() {
                            self.next();
                            tokens.push(self.make_token(TokenType::PipeEqual, "|=".to_string()));
                        } else {
                            tokens.push(self.make_token(TokenType::Pipe, "|".to_string()));
                        }
                    }

                    _ => {
                        panic!("unexpected character '{}'", c);
                    }
                }
            }
        }
        tokens
    }
}
