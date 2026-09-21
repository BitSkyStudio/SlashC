use crate::{lex::Lexer, parse::parse_module};

mod lex;
mod parse;

fn main() {
    let mut lexer = Lexer::lex(&std::fs::read_to_string("source.sl").unwrap()).unwrap();
    println!("{:?}", lexer.tokens);
    let module = parse_module(&mut lexer).unwrap();
    println!("{:?}", module);
}
