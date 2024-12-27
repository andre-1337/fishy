use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
alias str = ^const u8;

extern fn printf(str) -> i32;

struct Person {
    name: str,
    age: u8,
    address: str,
    relatives: [Person; ?],

    fn new(name: str, age: u8, address: str): Person {
        return Person {
            name: name,
            age: age,
            address: address,
            relatives: [ ]
        };
    }

    fn add_relative(self: &mut Person, relative: Person) {
        self.relatives.insert(relative);
    }
}

fn main(): i32 {
    printf("Hello, world!");
}
"#;

    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex();

    for token in tokens.clone() {
        println!("{:?}", token);
    }

    let mut parser = Parser::new(tokens);
    let module = match parser.parse() {
        Ok(module) => module,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    println!("{:?}", module);
}
