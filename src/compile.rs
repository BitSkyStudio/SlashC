use std::collections::HashMap;

use crate::{
    ast::{ASTBlock, ASTExpression, ASTLiteral, ASTMember, ASTStatement, DataType},
    lexer::Operator,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ItemPath(pub Vec<String>);
impl ItemPath {
    pub fn new() -> Self {
        ItemPath(Vec::new())
    }
    pub fn single(value: impl ToString) -> Self {
        ItemPath::new().extend(value.to_string())
    }
    pub fn extend(&self, value: String) -> Self {
        let mut new_path = self.clone();
        new_path.0.push(value);
        new_path
    }
}

pub struct Compiler {
    sources: HashMap<ItemPath, ASTMember>,
}
impl Compiler {
    pub fn new() -> Self {
        Compiler {
            sources: HashMap::new(),
        }
    }
    pub fn add_sources(&mut self, sources: Vec<ASTMember>) {
        for member in sources {
            if let Some(previous) = self.sources.insert(member.get_path().clone(), member) {
                println!("redefined member {:?}", previous.get_path());
            }
        }
    }
    pub fn compile_function(&self, function: ItemPath) -> CompiledFunction {
        let function = match self.sources.get(&function).unwrap() {
            ASTMember::Function(function) => function,
            _ => panic!(),
        };
        let mut context = FunctionCompileContext::new();
        context.stack_push();
        let block = CompiledBlock::compile(&function.body, &mut context, self);
        context.stack_pop();
        CompiledFunction {
            variables: context.variables,
            block,
        }
    }
}
pub struct FunctionCompileContext {
    variables: Vec<DataType>,
    variable_stack: Vec<HashMap<String, u32>>,
}
impl FunctionCompileContext {
    pub fn new() -> Self {
        FunctionCompileContext {
            variables: Vec::new(),
            variable_stack: Vec::new(),
        }
    }
    pub fn stack_push(&mut self) {
        self.variable_stack.push(HashMap::new());
    }
    pub fn stack_pop(&mut self) {
        self.variable_stack.pop();
    }
    pub fn set_variable(&mut self, name: &str, variable: DataType) -> u32 {
        let id = self.variables.len() as u32;
        self.variables.push(variable);
        self.variable_stack
            .last_mut()
            .unwrap()
            .insert(name.to_string(), id);
        id
    }
    pub fn get_variable_id(&self, name: &str) -> Option<u32> {
        for frame in self.variable_stack.iter().rev() {
            if let Some(variable) = frame.get(name) {
                return Some(*variable);
            }
        }
        None
    }
    pub fn get_variable_type(&self, id: u32) -> &DataType {
        &self.variables[id as usize]
    }
}
#[derive(Debug)]
pub struct CompiledFunction {
    pub variables: Vec<DataType>,
    pub block: CompiledBlock,
}
#[derive(Debug)]
pub struct CompiledBlock {
    pub statements: Vec<CompiledStatement>,
}
impl CompiledBlock {
    fn compile(
        block: &ASTBlock,
        context: &mut FunctionCompileContext,
        compiler: &Compiler,
    ) -> Self {
        let mut statements = Vec::new();
        for statement in &block.statements {
            match statement {
                ASTStatement::Expression(expression) => {
                    statements.push(Self::compile_expression(expression, context, compiler));
                }
                ASTStatement::Assign { left, right } => match left {
                    ASTExpression::VariableAccess { variable } => {
                        statements.push(CompiledStatement::StoreVariable {
                            variable: context.get_variable_id(&variable).unwrap(),
                            value: Box::new(Self::compile_expression(right, context, compiler)),
                        });
                    }
                    _ => unimplemented!(),
                },
                ASTStatement::InitializeAssign {
                    data_type,
                    variable,
                    expression,
                } => {
                    let expression = Self::compile_expression(expression, context, compiler);
                    let variable = context.set_variable(&variable, data_type.clone());
                    statements.push(CompiledStatement::StoreVariable {
                        variable,
                        value: Box::new(expression),
                    });
                }
            }
        }
        if let Some(expression) = &block.return_expression {
            statements.push(Self::compile_expression(expression, context, compiler));
        }
        CompiledBlock { statements }
    }
    fn compile_expression(
        expression: &ASTExpression,
        context: &mut FunctionCompileContext,
        compiler: &Compiler,
    ) -> CompiledStatement {
        match expression {
            ASTExpression::Literal(literal) => match literal {
                ASTLiteral::Integer(value) => CompiledStatement::IntegerLiteral { value: *value },
                ASTLiteral::Float(_) => todo!(),
                ASTLiteral::Bool(_) => todo!(),
            },
            ASTExpression::Operator {
                left,
                right,
                operator,
            } => {
                let left = Self::compile_expression(left, context, compiler);
                let right = Self::compile_expression(right, context, compiler);
                Self::make_function_call(
                    left.get_return_type(context, compiler)
                        .param_type
                        .path
                        .extend(format!("operator_{}", operator.get_name())),
                    vec![left, right],
                    context,
                    compiler,
                )
            }
            ASTExpression::FunctionCall {
                function,
                parameters,
            } => CompiledStatement::FunctionCall {
                path: function.clone(),
                arguments: parameters
                    .iter()
                    .map(|expression| Self::compile_expression(expression, context, compiler))
                    .collect(),
            },
            ASTExpression::VariableAccess { variable } => CompiledStatement::LoadVariable {
                variable: context.get_variable_id(&variable).unwrap(),
            },
        }
    }
    pub fn make_function_call(
        function: ItemPath,
        mut parameters: Vec<CompiledStatement>,
        context: &FunctionCompileContext,
        compiler: &Compiler,
    ) -> CompiledStatement {
        match function.0.join("::").as_str() {
            "i64::operator_add" => {
                let right = parameters.pop().unwrap();
                let left = parameters.pop().unwrap();
                CompiledStatement::IntegerOp {
                    a: Box::new(left),
                    b: Box::new(right),
                    op: Operator::Plus,
                }
            }
            "i64::operator_sub" => {
                let right = parameters.pop().unwrap();
                let left = parameters.pop().unwrap();
                CompiledStatement::IntegerOp {
                    a: Box::new(left),
                    b: Box::new(right),
                    op: Operator::Minus,
                }
            }
            _ => unimplemented!(),
        }
    }
}
#[derive(Debug)]
pub enum CompiledStatement {
    IntegerLiteral {
        value: i64,
    },
    LoadVariable {
        variable: u32,
    },
    StoreVariable {
        variable: u32,
        value: Box<CompiledStatement>,
    },
    IntegerOp {
        a: Box<CompiledStatement>,
        b: Box<CompiledStatement>,
        op: Operator,
    },
    FunctionCall {
        path: ItemPath,
        arguments: Vec<CompiledStatement>,
    },
}
impl CompiledStatement {
    pub fn get_return_type(
        &self,
        context: &FunctionCompileContext,
        compiler: &Compiler,
    ) -> DataType {
        match self {
            CompiledStatement::IntegerLiteral { value } => {
                DataType::make_simple(ItemPath::single("i64"))
            }
            CompiledStatement::LoadVariable { variable } => {
                context.get_variable_type(*variable).clone()
            }
            CompiledStatement::StoreVariable { variable, value } => DataType::void(),
            CompiledStatement::IntegerOp { a, b, op } => match op {
                Operator::Plus
                | Operator::Minus
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
                | Operator::Negate => DataType::make_simple(ItemPath::single("i64")),
                Operator::Comparison(_) => DataType::make_simple(ItemPath::single("bool")),
                Operator::And | Operator::Or | Operator::Xor => todo!(),
            },
            CompiledStatement::FunctionCall { path, arguments } => todo!(),
        }
    }
}
