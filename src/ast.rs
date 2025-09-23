use crate::{
    compile::{CompiledStatement, ItemPath},
    lexer::{Comparison, Operator, Token, TokenList},
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
    pub reference: Option<DataTypeReference>,
}
impl DataType {
    pub fn make_simple(path: ItemPath) -> Self {
        DataType {
            param_type: ParamType {
                path,
                template_args: Vec::new(),
            },
            reference: None,
        }
    }
    pub fn void() -> Self {
        DataType::make_simple(ItemPath::single("void"))
    }
}
pub fn parse_data_type(tokens: &mut TokenList) -> Result<DataType> {
    Ok(DataType {
        reference: match tokens.is_expected_and_take(Token::Reference)?.0 {
            true => todo!(),
            false => None,
        },
        param_type: parse_param_type(tokens)?,
    })
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DataTypeReference {
    pub mutability: Mutability,
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
    pub body: ASTBlock,
}
pub fn parse_function(tokens: &mut TokenList, base_path: ItemPath) -> Result<ASTFunction> {
    Ok(ASTFunction {
        return_type: parse_data_type(tokens)?,
        name: base_path.extend(tokens.expect_identifier()?.0),
        parameters: {
            let mut parameters = Vec::new();
            tokens.expect_token(Token::LParen)?;
            loop {
                if tokens.is_expected_and_take(Token::RParen)?.0 {
                    break;
                }
                let data_type = parse_data_type(tokens)?;
                let name = tokens.expect_identifier()?.0;
                parameters.push(ASTFunctionParameter { name, data_type });
                if !tokens.is_expected_and_take(Token::Comma)?.0 {
                    tokens.expect_token(Token::RParen)?;
                    break;
                }
            }
            parameters
        },
        body: parse_block(tokens)?,
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
    let mut expression = match tokens.take()?.0 {
        Token::Integer(value) => ASTExpression::Literal(ASTLiteral::Integer(value)),
        Token::Number(value) => ASTExpression::Literal(ASTLiteral::Float(value)),
        Token::Bool(value) => ASTExpression::Literal(ASTLiteral::Bool(value)),
        Token::Identifier(identifier) => {
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
                    function: ItemPath::single(identifier),
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
    Ok(expression)
}
#[derive(Clone, Debug)]
pub enum ASTExpression {
    Literal(ASTLiteral),
    Operator {
        left: Box<ASTExpression>,
        right: Box<ASTExpression>,
        operator: Operator,
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

pub enum ASTMember {
    Function(ASTFunction),
}
impl ASTMember {
    pub fn get_path(&self) -> &ItemPath {
        match self {
            ASTMember::Function(function) => &function.name,
        }
    }
}
pub fn parse_sources(tokens: &mut TokenList) -> Result<Vec<ASTMember>> {
    let mut members = Vec::new();
    while !tokens.is_empty() {
        members.push(ASTMember::Function(parse_function(
            tokens,
            ItemPath::new(),
        )?));
    }
    Ok(members)
}
