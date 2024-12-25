use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
let name: ^u8 = "";
    "#;

    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.lex();

    for token in tokens.clone() {
        println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);
    let module = parser.parse();

    println!("{:?}", module);
}
