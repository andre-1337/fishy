use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
    
struct Person {
    name: ^u8,
    age: u8,
    fn get_name(): ^u8 {},
    fn get_age(): u8 {},
    address: ^u8,
}

let x: [u8; 10] = 10;
let p = "wow";

fn main(): u8 {}

fn test(x: u8, y: u8): u8 {}
fn test2(wacky: u8, other: u8, more: u8): u8 {}

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
