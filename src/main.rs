use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
struct Person {
    name: ^u8,
    age: u64,
    address: ^u8
}
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
