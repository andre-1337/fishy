use crate::{token::Position, unit::Unit};

#[derive(Debug)]
pub enum Color {
    Black = 30,
    Red = 31,
    Green = 32,
    Yellow = 33,
    Blue = 34,
    Magenta = 35,
    Cyan = 36,
    White = 37,
    Default = 39,
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::Black => write!(f, "\x1b[30m"),
            Color::Red => write!(f, "\x1b[31m"),
            Color::Green => write!(f, "\x1b[32m"),
            Color::Yellow => write!(f, "\x1b[33m"),
            Color::Blue => write!(f, "\x1b[34m"),
            Color::Magenta => write!(f, "\x1b[35m"),
            Color::Cyan => write!(f, "\x1b[36m"),
            Color::White => write!(f, "\x1b[37m"),
            Color::Default => write!(f, "\x1b[39m"),
        }
    }
}

#[derive(Debug)]
pub enum ColorStyles {
    Bold = 1,
    Italic = 3,
    Underlined = 4,
    Reset = 0,
}

impl std::fmt::Display for ColorStyles {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ColorStyles::Bold => write!(f, "\x1b[1m"),
            ColorStyles::Italic => write!(f, "\x1b[3m"),
            ColorStyles::Underlined => write!(f, "\x1b[4m"),
            ColorStyles::Reset => write!(f, "\x1b[0m"),
        }
    }
}

macro_rules! make_error {
    ($self: ident, $f: ident, $typ: expr) => {{
        let lines = $self.src.source.lines().collect::<Vec<&str>>();
                let line = lines.get($self.location.line - 1);
                let line = match line {
                    None => "[no source code to show]",
                    Some(line) => *line,
                };

                let char = line.chars().nth($self.location.column - 1).unwrap();
                let before = line.chars().take($self.location.column - 1).collect::<String>();
                let after = line
                    .chars()
                    .skip($self.location.column)
                    .collect::<String>();

                let string = format!(
                    "{}{}{}{}{}{}",
                    before,
                    ColorStyles::Bold,
                    Color::Red,
                    char,
                    ColorStyles::Reset,
                    after
                );

                writeln!(
                    $f,
                    "┌ {}{}[{} {}:{}]{} {}{}{}{}",
                    ColorStyles::Bold,
                    Color::Blue,
                    $self.src.filename,
                    $self.location.line,
                    $self.location.column,
                    ColorStyles::Reset,
                    ColorStyles::Bold,
                    Color::Red,
                    $typ,
                    ColorStyles::Reset
                )?;
                writeln!($f, "│   {}{}{}", Color::Red, $self.message, Color::Default)?;
                writeln!($f, "│")?;
                writeln!($f, "├ {}", string)?;
                writeln!($f, "│ {}{}{}^ here{}", " ".repeat($self.location.column - 1), ColorStyles::Bold, Color::Blue, ColorStyles::Reset)?;

                match &$self.hint {
                    None => {}
                    Some(hint) => writeln!($f, "└ {}{}hint:{} {}", ColorStyles::Bold, Color::Blue, ColorStyles::Reset, hint)?,
                }

                writeln!($f, "")
    }
}}

#[derive(Debug)]
pub enum FishyError {
    Lexer(LexerError),
    Parser(ParserError),
    TypeChecker(TypeCheckError),
}

impl FishyError {
    pub fn new_lexer_error(
        src: Unit,
        location: Position,
        message: &str,
        hint: Option<String>,
    ) -> FishyError {
        FishyError::Lexer(LexerError {
            src,
            location,
            message: message.to_string(),
            hint,
        })
    }

    pub fn new_parser_error(
        src: Unit,
        location: Position,
        message: &str,
        hint: Option<String>,
    ) -> FishyError {
        FishyError::Parser(ParserError {
            src,
            location,
            message: message.to_string(),
            hint,
        })
    }

    pub fn new_typechecker_error(
        src: Unit,
        location: Position,
        message: &str,
        hint: Option<String>,
    ) -> FishyError {
        FishyError::TypeChecker(TypeCheckError {
            src,
            location,
            message: message.to_string(),
            hint,
        })
    }
}

impl std::fmt::Display for FishyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FishyError::Lexer(lexer) => {
                make_error!(lexer, f, "LexerError")
            }

            FishyError::Parser(parser) => {
                make_error!(parser, f, "ParserError")
            },

            FishyError::TypeChecker(typechecker) => {
                make_error!(typechecker, f, "TypeError")
            },
        }
    }
}

impl std::error::Error for FishyError {}

#[derive(Debug)]
pub struct LexerError {
    pub src: Unit,
    pub location: Position,
    pub message: String,
    pub hint: Option<String>,
}

#[derive(Debug)]
pub struct ParserError {
    pub src: Unit,
    pub location: Position,
    pub message: String,
    pub hint: Option<String>,
}

#[derive(Debug)]
pub struct TypeCheckError {
    pub src: Unit,
    pub location: Position,
    pub message: String,
    pub hint: Option<String>,
}
