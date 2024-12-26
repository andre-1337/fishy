use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
alias str = ^const u8;

extern fn printf(str) -> i32;

struct Person {
    name: str,
    age: i64,
    address: str,
    relatives: [Person; ?],

    fn new(name: str, age: i64, address: str): Person {}

    fn add_relative(self: &mut Person, relative: Person) {}
}

fn main(): i32 {
    printf("Hello, world!");
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
