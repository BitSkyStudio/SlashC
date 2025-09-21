use crate::compile::{Compiler, ItemPath};

mod ast;
mod codegen;
mod compile;
mod lexer;

fn main() {
    let file = std::fs::read_to_string("example/main.sl").unwrap();
    let mut tokens = lexer::TokenList::lex(file).unwrap();
    let sources = ast::parse_sources(&mut tokens).unwrap();
    let mut compiler = Compiler::new();
    compiler.add_sources(sources);
    codegen::testrun(&compiler).unwrap();
}
