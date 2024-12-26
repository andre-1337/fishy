use fishy::{lexer::Lexer, parser::Parser};

fn main() {
    let code = r#"
printf("Hello, world");
self.relatives.insert(relative);
"#;

    let mut lexer = Lexer::new(code.to_string());
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
