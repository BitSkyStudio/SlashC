use crate::{
    ast::{ASTFunction, ASTFunctionParameter, ASTMember, DataType, ParamType},
    compile::{Compiler, ItemPath},
};

mod ast;
mod codegen;
mod compile;
mod lexer;

fn main() {
    let file = std::fs::read_to_string("example/main.sl").unwrap();
    let mut tokens = lexer::TokenList::lex(file).unwrap();
    let sources = ast::parse_sources(&mut tokens).unwrap();
    println!("{:?}", sources);
    let mut compiler = Compiler::new();
    compiler.add_sources(sources);
    let print_function = ASTMember::Function(ASTFunction {
        body: None,
        name: ItemPath::single("print"),
        return_type: DataType::void(),
        parameters: vec![ASTFunctionParameter {
            name: "value".to_string(),
            data_type: DataType::make_simple(ItemPath::single("i64")),
        }],
    });
    compiler.add_sources(vec![print_function]);
    codegen::testrun(&compiler).unwrap();
}
