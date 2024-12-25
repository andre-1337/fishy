#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Question,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Ampersand,
    Pipe,
    Tilde,
    Exclamation,
    Equal,
    Less,
    Greater,

    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    CaretEqual,
    AmpersandEqual,
    PipeEqual,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    ExclamationEqual,
    AmpersandAmpersand,
    PipePipe,

    Identifier,
    String,
    Number,
    Char,

    If,
    Else,
    While,
    For,
    Return,
    Break,
    Struct,
    Alias,
    Enum,
    Fn,
    Extern,
    Use,
    In,
    As,
    Pub,
    Const,
    Module,
    Trait,
    Match,
    Mut,
    Let,
    With,

    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Question => write!(f, "?"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Star => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Percent => write!(f, "%"),
            TokenType::Caret => write!(f, "^"),
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::Tilde => write!(f, "~"),
            TokenType::Exclamation => write!(f, "!"),
            TokenType::Equal => write!(f, "="),
            TokenType::Less => write!(f, "<"),
            TokenType::Greater => write!(f, ">"),

            TokenType::PlusEqual => write!(f, "+="),
            TokenType::MinusEqual => write!(f, "-="),
            TokenType::StarEqual => write!(f, "*="),
            TokenType::SlashEqual => write!(f, "/="),
            TokenType::PercentEqual => write!(f, "%="),
            TokenType::CaretEqual => write!(f, "^="),
            TokenType::AmpersandEqual => write!(f, "&="),
            TokenType::PipeEqual => write!(f, "|="),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::ExclamationEqual => write!(f, "!="),
            TokenType::AmpersandAmpersand => write!(f, "&&"),
            TokenType::PipePipe => write!(f, "||"),

            TokenType::Identifier => write!(f, "<identifier>"),
            TokenType::String => write!(f, "<string>"),
            TokenType::Number => write!(f, "<number>"),
            TokenType::Char => write!(f, "<char>"),

            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::While => write!(f, "while"),
            TokenType::For => write!(f, "for"),
            TokenType::Return => write!(f, "return"),
            TokenType::Break => write!(f, "break"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Alias => write!(f, "alias"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Extern => write!(f, "extern"),
            TokenType::Use => write!(f, "use"),
            TokenType::In => write!(f, "in"),
            TokenType::As => write!(f, "as"),
            TokenType::Pub => write!(f, "pub"),
            TokenType::Const => write!(f, "const"),
            TokenType::Module => write!(f, "module"),
            TokenType::Trait => write!(f, "trait"),
            TokenType::Match => write!(f, "match"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::Let => write!(f, "let"),
            TokenType::With => write!(f, "with"),

            TokenType::Eof => write!(f, "<eof>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position { line, column }
    }
}

impl Default for Position {
    fn default() -> Position {
        Position::new(1, 1)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub position: Position,
    pub value: String,
}

impl Token {
    pub fn new(ttype: TokenType, position: Position, value: String) -> Token {
        Token {
            ttype,
            position,
            value,
        }
    }
}
