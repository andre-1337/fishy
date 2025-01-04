use fishy::{lexer::Lexer, parser::Parser, sema::TypeChecker, unit::Unit};

fn main() {
    let code = r#"
extern fn printf(str) -> i32;

struct Person {
    name: str,
    age: u8,
    address: str,
    relatives: [Person; ?],

    fn new(name: str, age: u8, address: str) {
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

fn main() {
    printf("Hello, world!");
}

struct Point {}
"#;
    let unit = Unit::new("test.fsh", code);

    let mut lexer = Lexer::new(unit);
    let tokens = match lexer.lex() {
        Err(e) => {
            eprintln!("{}", e);
            return;
        }

        Ok(tokens) => tokens,
    };

    let mut parser = Parser::new(lexer, tokens);
    let module = match parser.parse() {
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
        Ok(module) => module,
    };

    //println!("{:?}", module);

    let mut typechecker = TypeChecker::new(parser);
    let check = match typechecker.check_program(&module) {
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
        Ok(check) => check,
    };

    println!("{:#?}", check.scopes);
}
