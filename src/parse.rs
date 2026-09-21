use std::collections::HashMap;

use immutable_string::ImmutableString;

use crate::lex::{
    Lexer, ParseError, ParseResult,
    Token::{self, Identifier},
};
#[derive(Debug)]
pub struct ASTModule(HashMap<MemberSignature, ASTMember>);
#[derive(Debug)]
pub enum ClassKind {
    Class,
    Interface,
    Struct,
}
#[derive(Debug)]
pub struct ASTGenericBounds(Vec<(ASTDataType, ASTDataType)>);
#[derive(Debug)]
pub struct ASTClass {
    kind: ClassKind,
    members: HashMap<MemberSignature, ASTMember>,
    impls: Vec<(ASTDataType, ASTGenericBounds)>,
    deps: Vec<ASTDataType>,
    generics: Vec<ImmutableString>,
    bounds: ASTGenericBounds,
}
#[derive(Debug)]
pub struct ASTEnum {
    members: HashMap<MemberSignature, ASTMember>,
    cases: Vec<ASTDataType>,
    impls: Vec<ASTDataType>,
    generics: Vec<ImmutableString>,
    bounds: ASTGenericBounds,
}
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct MemberSignature {
    name: ImmutableString,
    argument_count: usize,
}
#[derive(Debug)]
pub enum ASTMember {
    Attribute {
        pre_generics: Vec<ASTDataType>,
        data_type: ASTDataType,
    },
    Function {
        arguments: Vec<(ImmutableString, ASTDataType)>,
        return_type: ASTDataType,
        pre_generics: Vec<ASTDataType>,
        post_generics: Vec<ImmutableString>,
        body: ASTBlock,
        bounds: ASTGenericBounds,
    },
    Class(ASTClass),
    Enum(ASTEnum),
}
#[derive(Clone, Debug)]
pub enum ASTDataType {
    Type(ImmutableString, Vec<ASTDataType>),
    Pointer(Box<ASTDataType>),
    UninitPointer(Box<ASTDataType>),
    DynPointer(Vec<ASTDataType>),
}
#[derive(Debug)]
pub struct ASTBlock(Vec<ASTStatement>);
#[derive(Debug)]
pub struct ASTStatement {
    left: Option<ASTAssignTarget>,
    right: ASTExpression,
}
#[derive(Debug)]
pub enum ASTAssignTarget {
    Expression(ASTExpression),
    Rip {
        members: HashMap<ImmutableString, ASTAssignTarget>,
        type_hint: Option<ASTDataType>,
    },
}
#[derive(Debug)]
pub enum ASTExpression {
    Void,
    Let {
        name: ImmutableString,
        type_hint: Option<ASTDataType>,
    },
    Identifier(ImmutableString),
    MemberCall {
        target: Box<ASTExpression>,
        member: ImmutableString,
        pre_generics: Vec<ASTDataType>,
        post_generics: Vec<ASTDataType>,
        arguments: ASTCallArguments,
    },
    StaticCall {
        name: ImmutableString,
        post_generics: Vec<ASTDataType>,
        arguments: ASTCallArguments,
    },
    If {
        condition: Box<ASTExpression>,
        pass: Box<ASTExpression>,
        fail: Box<ASTExpression>,
    },
    Loop(ASTBlock),
    Block(ASTBlock),
    Move(ImmutableString),
}
#[derive(Debug)]
pub enum ASTCallArguments {
    Function(Vec<ASTExpression>),
    Initializer(HashMap<ImmutableString, ASTExpression>),
}
mod idt {
    use immutable_string::ImmutableString;
    lazy_static::lazy_static! {
        pub static ref CLASS: ImmutableString = ImmutableString::from("class");
        pub static ref INTERFACE: ImmutableString = ImmutableString::from("interface");
        pub static ref STRUCT: ImmutableString = ImmutableString::from("struct");
        pub static ref ENUM: ImmutableString = ImmutableString::from("enum");
        pub static ref IF: ImmutableString = ImmutableString::from("if");
        pub static ref ELSE: ImmutableString = ImmutableString::from("else");
        pub static ref LOOP: ImmutableString = ImmutableString::from("loop");
        pub static ref MOVE: ImmutableString = ImmutableString::from("move");
        pub static ref CASE: ImmutableString = ImmutableString::from("case");
        pub static ref FN: ImmutableString = ImmutableString::from("fn");
        pub static ref RIP: ImmutableString = ImmutableString::from("rip");
        pub static ref UNINIT: ImmutableString = ImmutableString::from("uninit");
        pub static ref DYN: ImmutableString = ImmutableString::from("dyn");
        pub static ref LET: ImmutableString = ImmutableString::from("let");
        pub static ref IMPL: ImmutableString = ImmutableString::from("impl");
        pub static ref DEP: ImmutableString = ImmutableString::from("dep");
    }
}
struct MemberClassContext {
    class: ImmutableString,
    generics: Vec<ImmutableString>,
    is_enum: bool,
}
pub fn parse_module(lexer: &mut Lexer) -> ParseResult<ASTModule> {
    let members = parse_members(lexer, None)?;
    match lexer.peek() {
        Ok((token, position)) => Err(ParseError::Custom {
            message: format!("expected eof, got {:?}", token),
            position,
        }),
        Err(ParseError::EOF) => Ok(ASTModule(members)),
        Err(error) => Err(error),
    }
}
fn parse_members(
    lexer: &mut Lexer,
    class: Option<MemberClassContext>,
) -> ParseResult<HashMap<MemberSignature, ASTMember>> {
    let mut members = HashMap::new();
    loop {
        let (id, position) = match lexer.expect_identifier() {
            Ok(id) => id,
            Err(_) => break,
        };
        if id == *idt::CLASS || id == *idt::INTERFACE || id == *idt::STRUCT {
            let kind = if id == *idt::CLASS {
                ClassKind::Class
            } else if id == *idt::INTERFACE {
                ClassKind::Interface
            } else {
                ClassKind::Struct
            };
            let name = lexer.expect_identifier()?.0;
            let generics = parse_generics_names_if_any(lexer)?;
            let bounds = parse_generic_bounds_if_any(lexer)?;
            let mut impls = Vec::new();
            let mut deps = Vec::new();
            while let Some(_) = lexer.try_exec(|lexer| {
                let id = lexer.expect_identifier()?.0;
                if id == *idt::IMPL {
                    let dt = parse_type(lexer)?;
                    let bounds = parse_generic_bounds_if_any(lexer)?;
                    impls.push((dt, bounds));
                    Ok(())
                } else if id == *idt::DEP {
                    let dt = parse_type(lexer)?;
                    deps.push(dt);
                    Ok(())
                } else {
                    return Err(ParseError::EOF);
                }
            }) {}
            lexer.expect(Token::LBrace)?;
            let class_members = parse_members(
                lexer,
                Some(MemberClassContext {
                    class: match &class {
                        Some(parent_class) => format!("{}::{}", parent_class.class, name).into(),
                        None => name.clone(),
                    },
                    generics: generics.clone(),
                    is_enum: false,
                }),
            )?;
            lexer.expect(Token::RBrace)?;
            members.insert(
                MemberSignature {
                    name: name.clone(),
                    argument_count: 0,
                },
                ASTMember::Class(ASTClass {
                    kind,
                    members: class_members,
                    impls,
                    deps,
                    generics,
                    bounds,
                }),
            );
        } else if id == *idt::LET {
            let pre_generics = parse_generics_if_any(lexer)?;
            let name = lexer.expect_identifier()?.0;
            lexer.expect(Token::Colon)?;
            let dt = parse_type(lexer)?;
            lexer.expect(Token::Semi)?;
            members.insert(
                MemberSignature {
                    name: name.clone(),
                    argument_count: 1,
                },
                ASTMember::Attribute {
                    pre_generics,
                    data_type: dt,
                },
            );
        } else {
            return Err(ParseError::ExpectedToken {
                expect: vec![].into_boxed_slice(),
                got: Identifier(id),
                position,
            });
        }
    }
    Ok(members)
}
fn parse_assign_target(lexer: &mut Lexer) -> ParseResult<ASTAssignTarget> {
    match lexer.peek()?.0 {
        Token::Identifier(identifier) => {
            if identifier == *idt::RIP {
                lexer.pop()?;
                let type_hint = match lexer.expect(Token::LBrace) {
                    Ok(()) => None,
                    Err(ParseError::ExpectedToken { .. }) => {
                        let type_hint = parse_type(lexer)?;
                        lexer.expect(Token::LBrace)?;
                        Some(type_hint)
                    }
                    Err(err) => return Err(err),
                };
                let mut members = HashMap::new();
                loop {
                    let (token, position) = lexer.pop()?;
                    match token {
                        Token::Identifier(name) => {
                            lexer.expect(Token::Colon)?;
                            if members
                                .insert(name.clone(), parse_assign_target(lexer)?)
                                .is_some()
                            {
                                return Err(ParseError::Custom {
                                    message: format!("ripped attribute already moved {}", name),
                                    position,
                                });
                            }
                        }
                        Token::RBrace => {
                            break;
                        }
                        other => {
                            return Err(ParseError::ExpectedToken {
                                expect: vec![
                                    Token::RBrace,
                                    Token::Identifier(ImmutableString::from("")),
                                ]
                                .into_boxed_slice(),
                                got: other,
                                position,
                            });
                        }
                    }
                }
                return Ok(ASTAssignTarget::Rip { members, type_hint });
            }
        }
        _ => {}
    }
    Ok(ASTAssignTarget::Expression(parse_expression(lexer)?))
}
fn parse_statement(lexer: &mut Lexer) -> ParseResult<ASTStatement> {
    let target = parse_assign_target(lexer)?;
    match &target {
        ASTAssignTarget::Expression(_) => match lexer.peek()?.0 {
            Token::Assign => {
                lexer.pop()?;
                let expr = parse_expression(lexer)?;
                Ok(ASTStatement {
                    left: Some(target),
                    right: expr,
                })
            }
            _ => Ok(ASTStatement {
                left: None,
                right: match target {
                    ASTAssignTarget::Expression(expr) => expr,
                    _ => unreachable!(),
                },
            }),
        },
        ASTAssignTarget::Rip { .. } => {
            lexer.expect(Token::Assign)?;
            let expr = parse_expression(lexer)?;
            Ok(ASTStatement {
                left: Some(target),
                right: expr,
            })
        }
    }
}
fn parse_block(lexer: &mut Lexer) -> ParseResult<ASTBlock> {
    lexer.expect(Token::LBrace)?;
    let mut statements = Vec::new();
    loop {
        match lexer.peek()?.0 {
            Token::RBrace => break,
            Token::Semi => {
                statements.push(ASTStatement {
                    left: None,
                    right: ASTExpression::Void,
                });
            }
            _ => {}
        }
        statements.push(parse_statement(lexer)?);
    }
    if statements.is_empty() {
        statements.push(ASTStatement {
            left: None,
            right: ASTExpression::Void,
        });
    }
    Ok(ASTBlock(statements))
}
fn parse_expression(lexer: &mut Lexer) -> ParseResult<ASTExpression> {
    let mut expr = match lexer.expect_identifier() {
        Ok((identifier, _)) => {
            if identifier == *idt::LET {
                let (name, _) = lexer.expect_identifier()?;
                let type_hint = match lexer.expect(Token::Colon) {
                    Ok(_) => Some(parse_type(lexer)?),
                    Err(_) => None,
                };
                ASTExpression::Let { name, type_hint }
            } else if identifier == *idt::MOVE {
                ASTExpression::Move(lexer.expect_identifier()?.0)
            } else if identifier == *idt::IF {
                let condition = parse_expression(lexer)?;
                let pass = parse_expression(lexer)?;
                let fail = match lexer.pop()?.0 {
                    Token::Identifier(id) => {
                        if id == *idt::ELSE {
                            Some(parse_expression(lexer)?)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
                .unwrap_or(ASTExpression::Void);
                ASTExpression::If {
                    condition: Box::new(condition),
                    pass: Box::new(pass),
                    fail: Box::new(fail),
                }
            } else if identifier == *idt::LOOP {
                ASTExpression::Loop(parse_block(lexer)?)
            } else {
                let id = extend_identifier(lexer, identifier)?;
                let post_generics = parse_generics_if_any(lexer)?;
                let arguments = parse_call_arguments(lexer)?;
                ASTExpression::StaticCall {
                    name: id,
                    post_generics,
                    arguments,
                }
            }
        }
        Err(_) => {
            if let Some(block) = lexer.try_exec(|lexer| parse_block(lexer)) {
                ASTExpression::Block(block)
            } else {
                unimplemented!()
            }
        }
    };
    loop {
        if lexer.expect(Token::Dot).is_err() {
            break;
        }
        let pre_generics = parse_generics_if_any(lexer)?;
        let name = lexer.expect_identifier()?.0;
        let post_generics = parse_generics_if_any(lexer)?;
        let arguments = parse_call_arguments(lexer)?;
        let target = std::mem::replace(&mut expr, ASTExpression::Void);
        expr = ASTExpression::MemberCall {
            target: Box::new(target),
            member: name,
            pre_generics,
            post_generics,
            arguments,
        }
    }
    Ok(expr)
}
fn parse_call_arguments(lexer: &mut Lexer) -> ParseResult<ASTCallArguments> {
    if lexer.expect(Token::LBrace).is_ok() {
        let mut arguments = HashMap::new();
        if lexer.expect(Token::RBrace).is_err() {
            loop {
                let (name, position) = lexer.expect_identifier()?;
                lexer.expect(Token::Colon)?;
                let value = parse_expression(lexer)?;
                if arguments.insert(name.clone(), value).is_some() {
                    return Err(ParseError::Custom {
                        message: format!("redefined name inside initializer {}", name),
                        position,
                    });
                }
                match lexer.expect_n(&[Token::RBrace, Token::Comma])? {
                    Token::RBrace => break,
                    Token::Comma => {}
                    _ => unreachable!(),
                }
            }
        }
        return Ok(ASTCallArguments::Initializer(arguments));
    }
    let mut arguments = Vec::new();
    if lexer.expect(Token::LParen).is_ok() {
        if lexer.expect(Token::RParen).is_err() {
            loop {
                arguments.push(parse_expression(lexer)?);
                match lexer.expect_n(&[Token::RParen, Token::Comma])? {
                    Token::RParen => break,
                    Token::Comma => {}
                    _ => unreachable!(),
                }
            }
        }
    }
    Ok(ASTCallArguments::Function(arguments))
}
fn parse_type(lexer: &mut Lexer) -> ParseResult<ASTDataType> {
    let (token, position) = lexer.pop()?;
    match token {
        Token::Star => {
            let checkpoint = lexer.save();
            match lexer.expect_identifier() {
                Ok((id, _)) => {
                    if id == *idt::UNINIT {
                        return Ok(ASTDataType::UninitPointer(Box::new(parse_type(lexer)?)));
                    }
                    if id == *idt::DYN {
                        let mut implementing = Vec::new();
                        loop {
                            implementing.push(parse_type(lexer)?);
                            if lexer.expect(Token::Plus).is_err() {
                                break;
                            }
                        }
                        return Ok(ASTDataType::DynPointer(implementing));
                    }
                }
                Err(_) => {}
            }
            lexer.rollback(checkpoint);
            Ok(ASTDataType::Pointer(Box::new(parse_type(lexer)?)))
        }
        Token::Identifier(id) => {
            let id = extend_identifier(lexer, id)?;
            let generics = parse_generics_if_any(lexer)?;
            Ok(ASTDataType::Type(id, generics))
        }
        token => {
            return Err(ParseError::ExpectedToken {
                expect: vec![Token::Star, Token::Identifier("".into())].into_boxed_slice(),
                got: token,
                position,
            });
        }
    }
}
fn parse_generics_if_any(lexer: &mut Lexer) -> ParseResult<Vec<ASTDataType>> {
    let mut generics = Vec::new();
    if lexer.expect(Token::LAngle).is_err() {
        return Ok(vec![]);
    }
    loop {
        if lexer.expect(Token::RAngle).is_ok() {
            break;
        }
        generics.push(parse_type(lexer)?);
        match lexer.expect_n(&[Token::Comma, Token::RAngle])? {
            Token::Comma => {}
            Token::RAngle => break,
            _ => unreachable!(),
        }
    }
    Ok(generics)
}
fn parse_generics_names_if_any(lexer: &mut Lexer) -> ParseResult<Vec<ImmutableString>> {
    let mut generics = Vec::new();
    if lexer.expect(Token::LAngle).is_err() {
        return Ok(vec![]);
    }
    loop {
        if lexer.expect(Token::RAngle).is_ok() {
            break;
        }
        generics.push(lexer.expect_identifier()?.0);
        match lexer.expect_n(&[Token::Comma, Token::RAngle])? {
            Token::Comma => {}
            Token::RAngle => break,
            _ => unreachable!(),
        }
    }
    Ok(generics)
}
fn parse_generic_bounds_if_any(lexer: &mut Lexer) -> ParseResult<ASTGenericBounds> {
    let checkpoint = lexer.save();
    match lexer.expect_identifier() {
        Ok((id, _)) => {
            if id != *idt::IF {
                lexer.rollback(checkpoint);
                return Ok(ASTGenericBounds(Vec::new()));
            }
        }
        Err(_) => return Ok(ASTGenericBounds(Vec::new())),
    }
    let mut bounds = Vec::new();
    'outer: loop {
        let first = parse_type(lexer)?;
        lexer.expect(Token::Colon)?;
        loop {
            let second = parse_type(lexer)?;
            bounds.push((first.clone(), second));
            match lexer.peek()?.0 {
                Token::Plus => {}
                Token::Comma => break,
                _ => break 'outer,
            }
        }
    }
    Ok(ASTGenericBounds(bounds))
}
fn extend_identifier(lexer: &mut Lexer, id: ImmutableString) -> ParseResult<ImmutableString> {
    let mut full_id = id.to_string();
    loop {
        if lexer.expect(Token::DColon).is_err() {
            break;
        }
        full_id += &format!("::{}", lexer.expect_identifier()?.0);
    }
    Ok(full_id.into())
}
