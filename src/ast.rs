use crate::{
    compile::{CompiledStatement, ItemPath},
    lexer::{Comparison, Operator, Token, TokenList, UnaryOperator},
};
use anyhow::{Result, anyhow};

pub fn parse_item_path(tokens: &mut TokenList) -> Result<ItemPath> {
    Ok(ItemPath::single(tokens.expect_identifier()?.0))
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ParamType {
    pub path: ItemPath,
    pub template_args: Vec<ParamType>,
}
pub fn parse_param_type(tokens: &mut TokenList) -> Result<ParamType> {
    Ok(ParamType {
        path: parse_item_path(tokens)?,
        template_args: {
            let mut template_args = Vec::new();
            if tokens
                .is_expected_and_take(Token::Operator(Operator::Comparison(Comparison::Less)))?
                .0
            {
                todo!()
            }
            template_args
        },
    })
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DataType {
    pub param_type: ParamType,
    pub references: Vec<Mutability>,
}
impl DataType {
    pub fn make_simple(path: ItemPath) -> Self {
        DataType {
            param_type: ParamType {
                path,
                template_args: Vec::new(),
            },
            references: Vec::new(),
        }
    }
    pub fn void() -> Self {
        DataType::make_simple(ItemPath::single("void"))
    }
}
pub fn parse_data_type(tokens: &mut TokenList) -> Result<DataType> {
    let mut references = Vec::new();
    while tokens.is_expected_and_take(Token::Reference)?.0 {
        references.push(match tokens.is_expected_and_take(Token::Mut)?.0 {
            true => Mutability::Mutable,
            false => Mutability::Immutable,
        });
    }
    Ok(DataType {
        references,
        param_type: parse_param_type(tokens)?,
    })
}
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Mutability {
    Immutable,
    Mutable,
}
#[derive(Debug)]
pub struct ASTFunction {
    pub name: ItemPath,
    pub return_type: DataType,
    pub parameters: Vec<ASTFunctionParameter>,
    pub body: Option<ASTBlock>,
}
pub fn parse_function(
    tokens: &mut TokenList,
    base_path: ItemPath,
    this_type: Option<ItemPath>,
) -> Result<ASTFunction> {
    Ok(ASTFunction {
        return_type: parse_data_type(tokens)?,
        name: base_path.extend(tokens.expect_identifier()?.0),
        parameters: {
            let mut parameters = Vec::new();
            tokens.expect_token(Token::LParen)?;
            let mut valid_this = this_type.is_some();
            loop {
                if tokens.is_expected_and_take(Token::RParen)?.0 {
                    break;
                }
                let mut data_type = parse_data_type(tokens)?;
                if valid_this && data_type.param_type.path == ItemPath::single("this") {
                    data_type.param_type.path = this_type.clone().unwrap();
                    parameters.push(ASTFunctionParameter {
                        name: "this".to_string(),
                        data_type,
                    });
                } else {
                    let name = tokens.expect_identifier()?.0;
                    parameters.push(ASTFunctionParameter { name, data_type });
                }
                if !tokens.is_expected_and_take(Token::Comma)?.0 {
                    tokens.expect_token(Token::RParen)?;
                    break;
                }
                valid_this = false;
            }
            parameters
        },
        body: Some(parse_block(tokens)?),
    })
}
#[derive(Debug)]
pub struct ASTFunctionParameter {
    pub name: String,
    pub data_type: DataType,
}
#[derive(Clone, Debug)]
pub struct ASTBlock {
    pub statements: Vec<ASTStatement>,
    pub return_expression: Option<ASTExpression>,
}
pub fn parse_block(tokens: &mut TokenList) -> Result<ASTBlock> {
    tokens.expect_token(Token::LBrace)?;
    let mut statements = Vec::new();
    loop {
        if tokens.is_expected_and_take(Token::RBrace)?.0 {
            break;
        } else if let Some((data_type, variable)) = tokens.try_parse(|tokens| {
            let data_type = parse_data_type(tokens)?;
            let variable = tokens.expect_identifier()?.0;
            tokens.expect_token(Token::Assign(None))?;
            Ok((data_type, variable))
        }) {
            let expression = parse_expression(tokens)?;
            tokens.expect_token(Token::Semicolon)?;
            statements.push(ASTStatement::InitializeAssign {
                data_type,
                variable,
                expression,
            });
        } else {
            let left = parse_expression(tokens)?;
            if left.no_semicolon_required() {
                if tokens.is_expected_and_take(Token::RBrack)?.0 {
                    return Ok(ASTBlock {
                        statements,
                        return_expression: Some(left),
                    });
                }
            }
            match tokens.peek()?.0.clone() {
                Token::Assign(operator) => {
                    tokens.take().unwrap();
                    let mut reference_count = 0;
                    while tokens.is_expected_and_take(Token::Reference)?.0 {
                        reference_count += 1;
                    }
                    let right = parse_expression(tokens)?;
                    tokens.expect_token(Token::Semicolon)?;
                    statements.push(ASTStatement::Assign {
                        right: match operator {
                            Some(operator) => ASTExpression::Operator {
                                left: Box::new(left.clone()),
                                right: Box::new(right),
                                operator,
                            },
                            None => right,
                        },
                        left,
                        reference_count,
                    });
                }
                _ => {
                    if tokens.is_expected_and_take(Token::Semicolon)?.0
                        || left.no_semicolon_required()
                    {
                        statements.push(ASTStatement::Expression(left));
                    } else {
                        tokens.expect_token(Token::RBrace)?;
                        return Ok(ASTBlock {
                            statements,
                            return_expression: Some(left),
                        });
                    }
                }
            }
        }
    }
    Ok(ASTBlock {
        statements,
        return_expression: None,
    })
}
#[derive(Clone, Debug)]
pub enum ASTStatement {
    Expression(ASTExpression),
    Assign {
        left: ASTExpression,
        right: ASTExpression,
        reference_count: u32,
    },
    InitializeAssign {
        data_type: DataType,
        variable: String,
        expression: ASTExpression,
    },
}
pub fn parse_expression(tokens: &mut TokenList) -> Result<ASTExpression> {
    let left = parse_expression_primary(tokens)?;
    parse_expression_biops(tokens, left, 0)
}
fn parse_expression_biops(
    tokens: &mut TokenList,
    mut left: ASTExpression,
    min_precedence: u8,
) -> Result<ASTExpression> {
    let mut lookahead = tokens.peek()?.0.clone();
    while match lookahead {
        Token::Operator(operator) => operator.precedence() >= min_precedence,
        _ => false,
    } {
        let operator = match tokens.take()?.0 {
            Token::Operator(operator) => operator,
            _ => unreachable!(),
        };
        let mut right = parse_expression_primary(tokens)?;
        lookahead = tokens.peek()?.0.clone();
        while match lookahead {
            Token::Operator(next_operator) => next_operator.precedence() > operator.precedence(),
            _ => false,
        } {
            right = parse_expression_biops(
                tokens,
                right,
                match lookahead {
                    Token::Operator(operator) => operator,
                    _ => unreachable!(),
                }
                .precedence(),
            )?;
        }
        left = ASTExpression::Operator {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        };
    }
    Ok(left)
}
pub fn parse_expression_primary(tokens: &mut TokenList) -> Result<ASTExpression> {
    let unary_operator = match tokens.peek()?.0 {
        Token::Not => {
            tokens.take().unwrap();
            Some(UnaryOperator::Not)
        }
        Token::Operator(Operator::Minus) => {
            tokens.take().unwrap();
            Some(UnaryOperator::Negate)
        }
        _ => None,
    };
    let mut expression = match tokens.take()?.0 {
        Token::Integer(value) => ASTExpression::Literal(ASTLiteral::Integer(value)),
        Token::Number(value) => ASTExpression::Literal(ASTLiteral::Float(value)),
        Token::Bool(value) => ASTExpression::Literal(ASTLiteral::Bool(value)),
        Token::Identifier(identifier) => {
            let mut function = ItemPath::single(&identifier);
            while tokens.is_expected_and_take(Token::DoubleColon)?.0 {
                function = function.extend(tokens.expect_identifier()?.0.as_str());
            }
            if tokens.is_expected_and_take(Token::LParen)?.0 {
                let mut parameters = Vec::new();
                while match tokens.peek()?.0 {
                    Token::RParen => false,
                    _ => true,
                } {
                    parameters.push(parse_expression(tokens)?);
                    if !tokens.is_expected_and_take(Token::Comma)?.0 {
                        break;
                    }
                }
                tokens.expect_token(Token::RParen)?;
                ASTExpression::FunctionCall {
                    function,
                    parameters,
                }
            } else {
                ASTExpression::VariableAccess {
                    variable: identifier,
                }
            }
        }
        Token::LParen => {
            let expression = parse_expression(tokens)?;
            tokens.expect_token(Token::RParen)?;
            expression
        }
        Token::If => {
            let condition = parse_expression(tokens)?;
            let then = parse_block(tokens)?;
            let alt = if tokens.is_expected_and_take(Token::Else)?.0 {
                Some(parse_block(tokens)?)
            } else {
                None
            };
            ASTExpression::IfConditional {
                condition: Box::new(condition),
                then: Box::new(then),
                alt: alt.map(|alt| Box::new(alt)),
            }
        }
        Token::While => {
            let condition = parse_expression(tokens)?;
            let body = parse_block(tokens)?;
            ASTExpression::WhileLoop {
                condition: Box::new(condition),
                body: Box::new(body),
            }
        }
        token => return Err(anyhow::anyhow!("invalid token {token:?}")),
    };
    match tokens.peek()?.0 {
        Token::Dot => {
            tokens.take().unwrap();
            let member = tokens.expect_identifier()?.0;
            if tokens.is_expected_and_take(Token::LParen)?.0 {
                let mut parameters = Vec::new();
                while match tokens.peek()?.0 {
                    Token::RParen => false,
                    _ => true,
                } {
                    parameters.push(parse_expression(tokens)?);
                    if !tokens.is_expected_and_take(Token::Comma)?.0 {
                        break;
                    }
                }
                tokens.expect_token(Token::RParen)?;
                expression = ASTExpression::MethodCall {
                    expression: Box::new(expression),
                    method: member,
                    parameters,
                };
            } else {
                expression = ASTExpression::MemberAccess {
                    expression: Box::new(expression),
                    member,
                };
            }
        }
        _ => {}
    }
    Ok(match unary_operator {
        Some(operator) => ASTExpression::UnaryOperator {
            expression: Box::new(expression),
            operator,
        },
        None => expression,
    })
}
#[derive(Clone, Debug)]
pub enum ASTExpression {
    Literal(ASTLiteral),
    Operator {
        left: Box<ASTExpression>,
        right: Box<ASTExpression>,
        operator: Operator,
    },
    UnaryOperator {
        expression: Box<ASTExpression>,
        operator: UnaryOperator,
    },
    FunctionCall {
        function: ItemPath,
        parameters: Vec<ASTExpression>,
    },
    VariableAccess {
        variable: String,
    },
    IfConditional {
        condition: Box<ASTExpression>,
        then: Box<ASTBlock>,
        alt: Option<Box<ASTBlock>>,
    },
    WhileLoop {
        condition: Box<ASTExpression>,
        body: Box<ASTBlock>,
    },
    MemberAccess {
        expression: Box<ASTExpression>,
        member: String,
    },
    MethodCall {
        expression: Box<ASTExpression>,
        method: String,
        parameters: Vec<ASTExpression>,
    },
}
impl ASTExpression {
    pub fn no_semicolon_required(&self) -> bool {
        match self {
            ASTExpression::IfConditional { .. } | ASTExpression::WhileLoop { .. } => true,
            _ => false,
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum ASTLiteral {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug)]
pub struct ASTStruct {
    pub name: ItemPath,
    pub fields: Vec<ASTStructField>,
}
#[derive(Debug)]
pub struct ASTStructField {
    pub name: String,
    pub data_type: DataType,
}

pub fn parse_struct(tokens: &mut TokenList, base_path: ItemPath) -> Result<Vec<ASTMember>> {
    let mut ast_members = Vec::new();
    let name = base_path.extend(tokens.expect_identifier()?.0);
    tokens.expect_token(Token::LBrace)?;
    let mut fields = Vec::new();
    while !tokens.is_expected_and_take(Token::RBrace)?.0 {
        match tokens.try_parse(|tokens| {
            let data_type = parse_data_type(tokens)?;
            let name = tokens.expect_identifier()?.0;
            tokens.expect_token(Token::Semicolon)?;
            Ok(ASTStructField { name, data_type })
        }) {
            Some(field) => fields.push(field),
            None => {
                ast_members.push(ASTMember::Function(parse_function(
                    tokens,
                    name.clone(),
                    Some(name.clone()),
                )?));
            }
        }
    }
    ast_members.push(ASTMember::Struct(ASTStruct { name, fields }));
    Ok(ast_members)
}
#[derive(Debug)]
pub enum ASTMember {
    Function(ASTFunction),
    Struct(ASTStruct),
}
impl ASTMember {
    pub fn get_path(&self) -> &ItemPath {
        match self {
            ASTMember::Function(function) => &function.name,
            ASTMember::Struct(structure) => &structure.name,
        }
    }
}
pub fn parse_sources(tokens: &mut TokenList) -> Result<Vec<ASTMember>> {
    let mut members = Vec::new();
    while !tokens.is_empty() {
        if tokens.is_expected_and_take(Token::Struct)?.0 {
            members.extend(parse_struct(tokens, ItemPath::new())?);
        } else {
            members.push(ASTMember::Function(parse_function(
                tokens,
                ItemPath::new(),
                None,
            )?));
        }
    }
    Ok(members)
}
