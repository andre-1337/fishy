use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = "fn main";

    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex();

    let mut parser = Parser::new(tokens);
    let module = parser.parse();

    println!("{:?}", module);
}
