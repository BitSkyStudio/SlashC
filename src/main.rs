use crate::{
    ast::{ASTFunction, ASTFunctionParameter, ASTMember, DataType, ParameteredPath},
    compile::Compiler,
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
        name: "print".to_string(),
        return_type: DataType::void(),
        parameters: vec![ASTFunctionParameter {
            name: "value".to_string(),
            data_type: DataType::Simple(ParameteredPath::new("i64")),
        }],
    });
    compiler.add_sources(vec![print_function]);
    codegen::testrun(&compiler).unwrap();
    /*let mut vec = Vec::new();
    {
        let a = 5;
        vec.push(&a);
    }
    println!("{}", vec[0]);*/
}
