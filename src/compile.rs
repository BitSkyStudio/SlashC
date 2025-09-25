use std::collections::HashMap;

use crate::{
    ast::{
        ASTBlock, ASTExpression, ASTFunctionParameter, ASTLiteral, ASTMember, ASTStatement,
        DataType, Mutability,
    },
    lexer::{Comparison, Operator, UnaryOperator},
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
    pub sources: HashMap<ItemPath, ASTMember>,
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
        let mut context = FunctionCompileContext::new(&function.parameters);
        let block = CompiledBlock::compile(function.body.as_ref().unwrap(), &mut context, self);
        CompiledFunction {
            variables: context.variables,
            block,
            parameters: function
                .parameters
                .iter()
                .map(|param| param.data_type.clone())
                .collect(),
            return_type: function.return_type.clone(),
        }
    }
}
pub struct FunctionCompileContext {
    variables: Vec<DataType>,
    variable_stack: Vec<HashMap<String, u32>>,
}
impl FunctionCompileContext {
    pub fn new(parameters: &Vec<ASTFunctionParameter>) -> Self {
        let mut context = FunctionCompileContext {
            variables: Vec::new(),
            variable_stack: Vec::new(),
        };
        context.stack_push();
        for param in parameters {
            context.set_variable(&param.name, param.data_type.clone());
        }
        context
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
    pub parameters: Vec<DataType>,
    pub return_type: DataType,
}
#[derive(Debug)]
pub struct CompiledBlock {
    pub statements: Vec<CompiledStatement>,
    pub returns: bool,
}
impl CompiledBlock {
    fn compile(
        block: &ASTBlock,
        context: &mut FunctionCompileContext,
        compiler: &Compiler,
    ) -> Self {
        context.stack_push();
        let mut statements = Vec::new();
        for statement in &block.statements {
            match statement {
                ASTStatement::Expression(expression) => {
                    statements.push(Self::compile_expression(expression, context, compiler));
                }
                ASTStatement::Assign {
                    left,
                    right,
                    reference_count,
                } => {
                    let left = Self::compile_expression(left, context, compiler);
                    let right = Self::compile_expression(right, context, compiler);
                    let left_deref_count = left.get_return_type(compiler).references.len() as i32
                        - *reference_count as i32
                        - 1;
                    let right_deref_count = right.get_return_type(compiler).references.len() as i32
                        - *reference_count as i32;
                    if left_deref_count < 0 || right_deref_count < 0 {
                        panic!();
                    }

                    statements.push(CompiledStatement::Store {
                        target: Box::new(left.deref_n_times(left_deref_count as u32, compiler)),
                        value: Box::new(right.deref_n_times(right_deref_count as u32, compiler)),
                    });
                }
                ASTStatement::InitializeAssign {
                    data_type,
                    variable,
                    expression,
                } => {
                    let expression = Self::compile_expression(expression, context, compiler);
                    let variable = context.set_variable(&variable, data_type.clone());
                    statements.push(CompiledStatement::Store {
                        target: Box::new(CompiledStatement::GetVariable {
                            variable: variable,
                            data_type: data_type.clone(), //needs to be a reference
                        }),
                        value: Box::new(expression),
                    });
                }
            }
        }
        if let Some(expression) = &block.return_expression {
            statements.push(Self::compile_expression(expression, context, compiler));
        }
        context.stack_pop();
        CompiledBlock {
            statements,
            returns: block.return_expression.is_some(),
        }
    }
    fn compile_expression(
        expression: &ASTExpression,
        context: &mut FunctionCompileContext,
        compiler: &Compiler,
    ) -> CompiledStatement {
        match expression {
            ASTExpression::Literal(literal) => match literal {
                ASTLiteral::Integer(value) => CompiledStatement::IntegerLiteral {
                    value: *value,
                    data_type: ItemPath::single("i64"),
                },
                ASTLiteral::Float(_) => todo!(),
                ASTLiteral::Bool(value) => CompiledStatement::IntegerLiteral {
                    value: if *value { 1 } else { 0 },
                    data_type: ItemPath::single("bool"),
                },
            },
            ASTExpression::Operator {
                left,
                right,
                operator,
            } => {
                let left = Self::compile_expression(left, context, compiler);
                let right = Self::compile_expression(right, context, compiler);
                Self::make_function_call(
                    left.get_return_type(compiler)
                        .param_type
                        .path
                        .extend(format!("operator_{}", operator.get_name())),
                    vec![left, right],
                    context,
                    compiler,
                )
            }
            ASTExpression::UnaryOperator {
                expression,
                operator,
            } => {
                let expression = Self::compile_expression(expression, context, compiler);
                Self::make_function_call(
                    expression
                        .get_return_type(compiler)
                        .param_type
                        .path
                        .extend(format!("operator_{}", operator.get_name())),
                    vec![expression],
                    context,
                    compiler,
                )
            }
            ASTExpression::FunctionCall {
                function,
                parameters,
            } => Self::make_function_call(
                function.clone(),
                parameters
                    .iter()
                    .map(|expression| Self::compile_expression(expression, context, compiler))
                    .collect(),
                &context,
                compiler,
            ),
            ASTExpression::VariableAccess { variable } => {
                let variable = context.get_variable_id(&variable).unwrap();
                CompiledStatement::GetVariable {
                    variable,
                    data_type: context.get_variable_type(variable).clone(),
                }
            }
            ASTExpression::IfConditional {
                condition,
                then,
                alt,
            } => {
                let then = Box::new(Self::compile(&then, context, compiler));
                CompiledStatement::IfConditional {
                    condition: Box::new(Self::compile_expression(&condition, context, compiler)),
                    returned_type: then.get_return_type(context, compiler), //check else
                    then,
                    alt: alt
                        .as_ref()
                        .map(|alt| Box::new(Self::compile(&alt, context, compiler))),
                }
            }
            ASTExpression::WhileLoop { condition, body } => CompiledStatement::WhileLoop {
                condition: Box::new(Self::compile_expression(&condition, context, compiler)),
                body: Box::new(Self::compile(&body, context, compiler)),
            },
            ASTExpression::MemberAccess { expression, member } => {
                let parent = Self::compile_expression(&expression, context, compiler);
                let member = match compiler
                    .sources
                    .get(&parent.get_return_type(compiler).param_type.path)
                    .unwrap()
                {
                    ASTMember::Function(astfunction) => unreachable!(),
                    ASTMember::Struct(aststruct) => aststruct
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, field)| field.name == *member),
                }
                .unwrap();
                CompiledStatement::MemberAccess {
                    parent: Box::new(parent),
                    member: member.0 as u32,
                    returned_type: member.1.data_type.clone(),
                }
            }
        }
    }
    pub fn make_function_call(
        function: ItemPath,
        mut parameters: Vec<CompiledStatement>,
        context: &FunctionCompileContext,
        compiler: &Compiler,
    ) -> CompiledStatement {
        let name = function.0.join("::");
        for (base_int_type, number) in [("i64", true), ("bool", false)] {
            if name.starts_with(base_int_type)
                && name[base_int_type.len()..].starts_with("::operator_")
            {
                let operator = &name["::operator_".len() + base_int_type.len()..];
                let operators = if number {
                    &[
                        Operator::Plus,
                        Operator::Minus,
                        Operator::Multiply,
                        Operator::Divide,
                        Operator::Modulo,
                        Operator::And,
                        Operator::Or,
                        Operator::Xor,
                        Operator::Comparison(Comparison::Equal),
                        Operator::Comparison(Comparison::NotEqual),
                        Operator::Comparison(Comparison::Greater),
                        Operator::Comparison(Comparison::GreaterEqual),
                        Operator::Comparison(Comparison::Less),
                        Operator::Comparison(Comparison::LessEqual),
                    ][..]
                } else {
                    &[Operator::And, Operator::Or, Operator::Xor][..]
                };
                let unary_operators = if number {
                    &[UnaryOperator::Negate][..]
                } else {
                    &[UnaryOperator::Not][..]
                };
                for op in operators {
                    if op.get_name() == operator {
                        let right = parameters.pop().unwrap();
                        let left = parameters.pop().unwrap();
                        let left_deref_cnt = left.get_return_type(compiler).references.len() as u32;
                        let right_deref_cnt =
                            right.get_return_type(compiler).references.len() as u32;
                        return CompiledStatement::IntegerOp {
                            a: Box::new(left.deref_n_times(left_deref_cnt, compiler)),
                            b: Box::new(right.deref_n_times(right_deref_cnt, compiler)),
                            op: *op,
                        };
                    }
                }
                for op in unary_operators {
                    if op.get_name() == operator {
                        let expression = parameters.pop().unwrap();
                        let expression_deref_count =
                            expression.get_return_type(compiler).references.len() as u32;
                        return CompiledStatement::IntegerUnaryOp {
                            a: Box::new(expression.deref_n_times(expression_deref_count, compiler)),
                            op: *op,
                        };
                    }
                }
            }
        }
        let ast_function = match compiler.sources.get(&function).unwrap() {
            ASTMember::Function(astfunction) => astfunction,
            ASTMember::Struct(aststruct) => unreachable!(),
        };
        CompiledStatement::FunctionCall {
            path: function,
            arguments: parameters
                .into_iter()
                .enumerate()
                .map(|(i, parameter)| {
                    let target_reference_count =
                        ast_function.parameters[i].data_type.references.len() as u32;
                    let param_reference_count =
                        parameter.get_return_type(compiler).references.len() as u32;
                    if target_reference_count > param_reference_count {
                        panic!();
                    }
                    parameter
                        .deref_n_times(param_reference_count - target_reference_count, compiler)
                })
                .collect(),
        }
    }
    pub fn get_return_type(
        &self,
        context: &FunctionCompileContext,
        compiler: &Compiler,
    ) -> DataType {
        if self.returns {
            self.statements
                .last()
                .map(|statement| statement.get_return_type(compiler))
                .unwrap_or(DataType::void())
        } else {
            DataType::void()
        }
    }
}
#[derive(Debug)]
pub enum CompiledStatement {
    IntegerLiteral {
        data_type: ItemPath,
        value: i64,
    },
    GetVariable {
        variable: u32,
        data_type: DataType,
    },
    Store {
        target: Box<CompiledStatement>,
        value: Box<CompiledStatement>,
    },
    IntegerOp {
        a: Box<CompiledStatement>,
        b: Box<CompiledStatement>,
        op: Operator,
    },
    IntegerUnaryOp {
        a: Box<CompiledStatement>,
        op: UnaryOperator,
    },
    FunctionCall {
        path: ItemPath,
        arguments: Vec<CompiledStatement>,
    },
    IfConditional {
        condition: Box<CompiledStatement>,
        then: Box<CompiledBlock>,
        alt: Option<Box<CompiledBlock>>,
        returned_type: DataType,
    },
    WhileLoop {
        condition: Box<CompiledStatement>,
        body: Box<CompiledBlock>,
    },
    MemberAccess {
        parent: Box<CompiledStatement>,
        member: u32,
        returned_type: DataType,
    },
    Dereference {
        parent: Box<CompiledStatement>,
        returned_type: DataType,
    },
}
impl CompiledStatement {
    pub fn get_return_type(&self, compiler: &Compiler) -> DataType {
        match self {
            CompiledStatement::IntegerLiteral { value, data_type } => {
                DataType::make_simple(data_type.clone())
            }
            CompiledStatement::GetVariable {
                variable,
                data_type,
            } => {
                let mut data_type = data_type.clone();
                data_type.references.insert(0, Mutability::Immutable);
                data_type
            }
            CompiledStatement::Store { target, value } => DataType::void(),
            CompiledStatement::IntegerOp { a, b, op } => match op {
                Operator::Plus
                | Operator::Minus
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
                | Operator::And
                | Operator::Or
                | Operator::Xor => {
                    let mut returned_type = a.get_return_type(compiler);
                    returned_type.references.clear();
                    returned_type
                }
                Operator::Comparison(_) => DataType::make_simple(ItemPath::single("bool")),
            },
            CompiledStatement::IntegerUnaryOp { a, op } => {
                let mut returned_type = a.get_return_type(compiler);
                returned_type.references.clear();
                returned_type
            }
            CompiledStatement::FunctionCall { path, arguments } => {
                match compiler.sources.get(path).unwrap() {
                    ASTMember::Function(function) => function.return_type.clone(),
                    _ => panic!(),
                }
            }
            CompiledStatement::IfConditional { returned_type, .. } => returned_type.clone(),
            CompiledStatement::WhileLoop { condition, body } => DataType::void(),
            CompiledStatement::MemberAccess { returned_type, .. } => returned_type.clone(),
            CompiledStatement::Dereference { returned_type, .. } => returned_type.clone(),
        }
    }
    pub fn deref_n_times(mut self, n: u32, compiler: &Compiler) -> CompiledStatement {
        for _ in 0..n {
            let mut new_type = self.get_return_type(compiler);
            new_type.references.remove(0);
            self = CompiledStatement::Dereference {
                parent: Box::new(self),
                returned_type: new_type,
            };
        }
        self
    }
}
