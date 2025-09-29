use std::collections::HashMap;

use crate::{
    ast::{
        ASTBlock, ASTExpression, ASTFunctionParameter, ASTLiteral, ASTMember, ASTStatement,
        DataType, Mutability, ParameteredPath,
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
    pub fn extend(&self, value: impl ToString) -> Self {
        let mut new_path = self.clone();
        new_path.0.push(value.to_string());
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
                    //todo: handle reference count
                    let left = Self::compile_expression(left, context, compiler);
                    let right = Self::compile_expression(right, context, compiler);

                    let right_type =
                        DataType::Simple(right.get_return_type(compiler).get_base_path().clone());

                    statements.push(CompiledStatement::Store {
                        target: Box::new(
                            left.implicit_cast_to(
                                &DataType::Reference(
                                    Mutability::Immutable,
                                    Box::new(right_type.clone()),
                                ),
                                compiler,
                            )
                            .unwrap(),
                        ),
                        value: Box::new(right.implicit_cast_to(&right_type, compiler).unwrap()),
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
                    ParameteredPath::new(
                        left.get_return_type(compiler)
                            .get_base_path()
                            .path
                            .extend(format!("operator_{}", operator.get_name())),
                    ),
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
                    ParameteredPath::new(
                        expression
                            .get_return_type(compiler)
                            .get_base_path()
                            .path
                            .extend(format!("operator_{}", operator.get_name())),
                    ),
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
                let variable = context.get_variable_id(&variable).expect(variable.as_str());
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
                let mut parent = Self::compile_expression(&expression, context, compiler);
                let member = match compiler
                    .sources
                    .get(&parent.get_return_type(compiler).get_base_path().path)
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
                let parent_type = parent.get_return_type(compiler);
                match parent_type {
                    DataType::Simple(path) => CompiledStatement::MemberAccess {
                        struct_type: parent.get_return_type(compiler),
                        parent: Box::new(parent),
                        member: member.0 as u32,
                        is_reference: false,
                    },
                    _ => CompiledStatement::MemberAccess {
                        struct_type: parent.get_return_type(compiler),
                        parent: Box::new(
                            parent
                                .implicit_cast_to(
                                    &DataType::Reference(
                                        Mutability::Immutable,
                                        Box::new(DataType::Simple(
                                            parent_type.get_base_path().clone(),
                                        )),
                                    ),
                                    compiler,
                                )
                                .unwrap(),
                        ),
                        member: member.0 as u32,
                        is_reference: true,
                    },
                }
            }
            ASTExpression::MethodCall {
                expression,
                method,
                parameters,
            } => {
                let compiled_expression = Self::compile_expression(&expression, context, compiler);
                let target_method = compiled_expression
                    .get_return_type(compiler)
                    .get_base_path()
                    .path
                    .extend(method);
                let mut method_params = Vec::new();
                method_params.push(compiled_expression);
                for parameter in parameters {
                    method_params.push(Self::compile_expression(parameter, context, compiler));
                }
                Self::make_function_call(
                    ParameteredPath::new(target_method),
                    method_params,
                    context,
                    compiler,
                )
            }
        }
    }
    pub fn make_function_call(
        function: ParameteredPath,
        mut parameters: Vec<CompiledStatement>,
        context: &FunctionCompileContext,
        compiler: &Compiler,
    ) -> CompiledStatement {
        let name = function.path.0.join("::");
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
                        let target_type = DataType::Simple(
                            left.get_return_type(compiler).get_base_path().clone(),
                        );
                        return CompiledStatement::IntegerOp {
                            a: Box::new(left.implicit_cast_to(&target_type, compiler).unwrap()),
                            b: Box::new(right.implicit_cast_to(&target_type, compiler).unwrap()),
                            op: *op,
                        };
                    }
                }
                for op in unary_operators {
                    if op.get_name() == operator {
                        let expression = parameters.pop().unwrap();
                        let target_type = DataType::Simple(
                            expression.get_return_type(compiler).get_base_path().clone(),
                        );
                        return CompiledStatement::IntegerUnaryOp {
                            a: Box::new(
                                expression.implicit_cast_to(&target_type, compiler).unwrap(),
                            ),
                            op: *op,
                        };
                    }
                }
            }
        }
        match compiler
            .sources
            .get(&function.path)
            .expect(&function.path.0.join("::"))
        {
            ASTMember::Function(ast_function) => CompiledStatement::FunctionCall {
                path: function,
                arguments: parameters
                    .into_iter()
                    .enumerate()
                    .map(|(i, parameter)| {
                        parameter
                            .implicit_cast_to(&ast_function.parameters[i].data_type, compiler)
                            .unwrap()
                    })
                    .collect(),
            },
            ASTMember::Struct(ast_struct) => CompiledStatement::Initialize {
                data_type: function,
                fields: parameters
                    .into_iter()
                    .enumerate()
                    .map(|(i, parameter)| {
                        parameter
                            .implicit_cast_to(&ast_struct.fields[i].data_type, compiler)
                            .unwrap()
                    })
                    .collect(),
            },
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
        path: ParameteredPath,
        arguments: Vec<CompiledStatement>,
    },
    Initialize {
        data_type: ParameteredPath,
        fields: Vec<CompiledStatement>,
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
        struct_type: DataType,
        is_reference: bool,
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
                DataType::Simple(ParameteredPath::new(data_type.clone()))
            }
            CompiledStatement::GetVariable {
                variable,
                data_type,
            } => DataType::Reference(Mutability::Immutable, Box::new(data_type.clone())),
            CompiledStatement::Store { target, value } => DataType::void(),
            CompiledStatement::IntegerOp { a, b, op } => match op {
                Operator::Plus
                | Operator::Minus
                | Operator::Multiply
                | Operator::Divide
                | Operator::Modulo
                | Operator::And
                | Operator::Or
                | Operator::Xor => a.get_return_type(compiler),
                Operator::Comparison(_) => {
                    DataType::Simple(ParameteredPath::new(ItemPath::single("bool")))
                }
            },
            CompiledStatement::IntegerUnaryOp { a, op } => a.get_return_type(compiler),
            CompiledStatement::FunctionCall { path, arguments } => {
                match compiler.sources.get(&path.path).unwrap() {
                    ASTMember::Function(function) => function.return_type.clone(),
                    _ => panic!(),
                }
            }
            CompiledStatement::IfConditional { returned_type, .. } => returned_type.clone(),
            CompiledStatement::WhileLoop { condition, body } => DataType::void(),
            CompiledStatement::MemberAccess {
                struct_type,
                member,
                is_reference,
                ..
            } => match compiler
                .sources
                .get(&struct_type.get_base_path().path)
                .unwrap()
            {
                ASTMember::Function(astfunction) => unreachable!(),
                ASTMember::Struct(aststruct) => {
                    let mut field_type = aststruct.fields[*member as usize].data_type.clone();
                    if *is_reference {
                        DataType::Reference(Mutability::Immutable, Box::new(field_type.clone()))
                    } else {
                        field_type
                    }
                }
            },
            CompiledStatement::Dereference { returned_type, .. } => returned_type.clone(),
            CompiledStatement::Initialize { data_type, fields } => {
                DataType::Simple(data_type.clone())
            }
        }
    }
    pub fn implicit_cast_to(self, to: &DataType, compiler: &Compiler) -> Option<CompiledStatement> {
        Self::implicit_cast_type_to(self.get_return_type(compiler), to, self)
    }
    pub fn implicit_cast_type_to(
        from: DataType,
        to: &DataType,
        statement: CompiledStatement,
    ) -> Option<CompiledStatement> {
        println!("from: {:?} to: {:?}", from, to);
        if from == *to {
            return Some(statement);
        }
        match from {
            DataType::Simple(parametered_path) => None,
            DataType::Reference(mutability, data_type) => {
                Self::implicit_cast_type_to((*data_type).clone(), to, statement).map(|statement| {
                    CompiledStatement::Dereference {
                        parent: Box::new(statement),
                        returned_type: (*data_type).clone(),
                    }
                })
            }
            DataType::Pointer(mutability, _, data_type) => todo!(),
            DataType::Lock(data_type) => todo!(),
        }
    }
}
