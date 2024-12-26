# Fishy

![Fishy Logo](/asset/fishy.png)

<style>
img[alt="Fishy Logo"] {
  width: 256px;
}
</style>

Fishy is a programming language that aims to be a middle point between C/C++ and Rust. A (somewhat) memory safe language, without holding your hands.

## Goals

- (Somewhat) memory safe
- Low-level enough to be able to write performance-critical software
- High-level enough with enough niceties and abstractions, but without too much overhead
- A FFI to interact with C/C++ code
- Possibly multiple backends

## Syntax Example

```rs
alias str = ^const u8;

extern fn printf(str) -> i32;

struct Person {
    name: str,
    age: u8,
    address: str,
    relatives: [Person; ?],

    fn new(name: str, age: u8, address: str): Person {}

    fn add_relative(self: &mut Person, relative: Person) {
        self.relatives.insert(relative);
    }
}

fn main(): i32 {
    printf("Hello, world!");
}
```

## Roadmap

- [x] Lexer
- [x] Parser (WIP)
- [ ] Symbol resolution
- [ ] Typechecking
- [ ] I.R. Pass
- [ ] Code Generation
