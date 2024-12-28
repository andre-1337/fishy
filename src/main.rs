use fishy::{lexer::Lexer, parser::Parser, sema::TypeChecker, unit::Unit};

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

let x = 1;
let name: str = "André";
"#;
    let unit = Unit::new("test.fsh", code);

    let mut lexer = Lexer::new(unit);
    let tokens = lexer.lex();

    let tokens = match tokens {
        Err(e) => {
            eprintln!("{}", e);
            return;
        }

        Ok(tokens) => tokens,
    };

    let mut parser = Parser::new(tokens);
    let module = parser.parse().unwrap();

    println!("{:?}", module);

    let mut typechecker = TypeChecker::new();
    let check = typechecker.check_program(&module).unwrap();

    println!("{:?}", check.scopes);
}
