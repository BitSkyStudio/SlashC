use std::fmt::Debug;

use immutable_string::ImmutableString;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Identifier(ImmutableString),
    Number(f64),
    Integer(i64),
    Dot,
    Comma,
    DColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Hash,
    Assign,
    Colon,
    Semi,
    Star,
    Plus,
}
impl Token {
    const SYMBOL_TABLE: [(&'static str, Token); 15] = [
        (".", Token::Dot),
        (",", Token::Comma),
        ("::", Token::DColon),
        ("(", Token::LParen),
        (")", Token::RParen),
        ("{", Token::LBrace),
        ("}", Token::RBrace),
        ("<", Token::LAngle),
        (">", Token::RAngle),
        ("#", Token::Hash),
        ("=", Token::Assign),
        (":", Token::Colon),
        (";", Token::Semi),
        ("*", Token::Star),
        ("+", Token::Plus),
    ];
}
#[derive(Clone, Copy)]
pub struct TokenPosition {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}
impl Debug for TokenPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}[{}]", self.line, self.column, self.length)
    }
}
impl TokenPosition {
    fn shift_back_by_length(self) -> TokenPosition {
        TokenPosition {
            line: self.line,
            column: self.column - self.length,
            length: self.length,
        }
    }
}
pub struct Lexer {
    cursor: usize,
    pub tokens: Vec<(Token, TokenPosition)>,
}
pub struct LexerCheckpoint(usize);
impl Lexer {
    pub fn save(&self) -> LexerCheckpoint {
        LexerCheckpoint(self.cursor)
    }
    pub fn rollback(&mut self, checkpoint: LexerCheckpoint) {
        self.cursor = checkpoint.0;
    }
    pub fn try_exec<T>(
        &mut self,
        callback: impl FnOnce(&mut Lexer) -> ParseResult<T>,
    ) -> Option<T> {
        let checkpoint = self.save();
        match callback(self) {
            Ok(res) => Some(res),
            Err(_) => {
                self.rollback(checkpoint);
                None
            }
        }
    }
    pub fn peek(&self) -> ParseResult<(Token, TokenPosition)> {
        if self.tokens.len() <= self.cursor {
            return Err(ParseError::EOF);
        }
        Ok(self.tokens[self.cursor].clone())
    }
    pub fn pop(&mut self) -> ParseResult<(Token, TokenPosition)> {
        if self.tokens.len() <= self.cursor {
            return Err(ParseError::EOF);
        }
        self.cursor += 1;
        Ok(self.tokens[self.cursor - 1].clone())
    }
    pub fn expect(&mut self, expect: Token) -> ParseResult<()> {
        let (token, position) = self.peek()?;
        if token == expect {
            self.pop().unwrap();
            Ok(())
        } else {
            Err(ParseError::ExpectedToken {
                expect: vec![expect].into_boxed_slice(),
                got: token,
                position,
            })
        }
    }
    pub fn expect_n(&mut self, expect: &[Token]) -> ParseResult<Token> {
        let (token, position) = self.peek()?;
        if expect.contains(&token) {
            self.pop().unwrap();
            Ok(token)
        } else {
            Err(ParseError::ExpectedToken {
                expect: expect.to_vec().into_boxed_slice(),
                got: token,
                position,
            })
        }
    }
    pub fn expect_identifier(&mut self) -> ParseResult<(ImmutableString, TokenPosition)> {
        let (token, position) = self.peek()?;
        match token {
            Token::Identifier(text) => {
                self.pop().unwrap();
                Ok((text, position))
            }
            got => Err(ParseError::ExpectedToken {
                expect: vec![Token::Identifier("".into())].into_boxed_slice(),
                got,
                position,
            }),
        }
    }
}
pub type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub enum ParseError {
    EOF,
    ExpectedToken {
        expect: Box<[Token]>,
        got: Token,
        position: TokenPosition,
    },
    Custom {
        message: String,
        position: TokenPosition,
    },
}
impl Lexer {
    pub fn lex(source: &str) -> Result<Lexer, LexError> {
        let mut column = 0;
        let mut line = 0;
        let mut i = 0;
        let mut tokens = Vec::new();
        'outer: while i < source.len() {
            let slice = source.as_bytes();
            let first_character = slice[i] as char;
            if first_character.is_whitespace() {
                i += 1;
                column += 1;
                if first_character == '\n' {
                    line += 1;
                    column = 0;
                }
                continue;
            }
            for (s, t) in &Token::SYMBOL_TABLE {
                if source[i..].starts_with(s) {
                    tokens.push((
                        t.clone(),
                        TokenPosition {
                            line,
                            column,
                            length: s.len(),
                        },
                    ));
                    i += s.len();
                    column += s.len();
                    continue 'outer;
                }
            }
            if first_character.is_ascii_digit() || first_character == '-' {
                let start = i;
                i += 1;
                column += 1;
                let mut had_dot = false;
                while i < source.len() {
                    let char = slice[i] as char;
                    if char == '.' && !had_dot {
                        had_dot = true;
                        i += 1;
                        column += 1;
                    } else if char.is_ascii_digit() {
                        i += 1;
                        column += 1;
                    } else {
                        break;
                    }
                }
                tokens.push((
                    if had_dot {
                        Token::Number(source[start..i].parse::<f64>().unwrap())
                    } else {
                        Token::Integer(source[start..i].parse::<i64>().unwrap())
                    },
                    TokenPosition {
                        line,
                        column,
                        length: i - start + 1,
                    }
                    .shift_back_by_length(),
                ));
                continue;
            }
            if Self::is_valid_identifier_character(first_character, true) {
                let start = i;
                i += 1;
                while Self::is_valid_identifier_character(
                    {
                        let Some(c) = slice.get(i) else {
                            break 'outer;
                        };
                        *c as char
                    },
                    false,
                ) {
                    i += 1;
                    column += 1;
                }
                tokens.push((
                    Token::Identifier(source[start..i].into()),
                    TokenPosition {
                        length: i - start + 1,
                        column,
                        line,
                    },
                ));
                continue;
            }
            return Err(LexError::InvalidCharacter(
                first_character,
                TokenPosition {
                    line,
                    column,
                    length: 1,
                },
            ));
        }
        Ok(Lexer { tokens, cursor: 0 })
    }
    fn is_valid_identifier_character(character: char, first: bool) -> bool {
        character.is_ascii_alphabetic() || character == '_' || (character.is_numeric() && !first)
    }
}
#[derive(Debug)]
pub enum LexError {
    InvalidCharacter(char, TokenPosition),
}
