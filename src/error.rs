use crate::{token::Position, unit::Unit};

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
                let lines = lexer.src.source.lines().collect::<Vec<&str>>();
                let line = lines.get(lexer.location.line - 1);
                let line = match line {
                    None => "[no source code to show]",
                    Some(line) => *line,
                };

                writeln!(
                    f,
                    "┌ [{} {}:{}] LexerError",
                    lexer.src.filename, lexer.location.line, lexer.location.column
                )?;
                writeln!(f, "├ {}", lexer.message)?;
                writeln!(f, "│")?;
                writeln!(f, "├ {}", line)?;
                writeln!(f, "├ {}^ here", " ".repeat(lexer.location.column - 1))?;

                match &lexer.hint {
                    None => {}
                    Some(hint) => writeln!(f, "└ {}", hint)?,
                }

                writeln!(f, "")
            }
            FishyError::Parser(ParserError { .. }) => Ok(()),
            FishyError::TypeChecker(TypeCheckError { .. }) => Ok(()),
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
