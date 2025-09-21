use anyhow::anyhow;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    String(String),
    Number(f64),
    Integer(i64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    DoubleColon,
    Colon,
    Comma,
    Fn,
    Class,
    Interface,
    Namespace,
    Struct,
    Operator(Operator),
    Arrow,
    Let,
    Assign(Option<Operator>),
    Semicolon,
    Dot,
    Bool(bool),
    Reference,
    If,
    While,
    Else,
    Implements,
    Depends,
    Override,
    New,
    Cast,
    DynamicCast,
    Enum,
    Mut,
    Match,
}
#[derive(Debug, Clone)]
pub struct TokenPosition {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}
impl TokenPosition {
    pub fn internal() -> TokenPosition {
        TokenPosition {
            length: 0,
            column: 0,
            line: 0,
        }
    }
}
#[derive(Default)]
struct LexerConfig {
    keywords: Vec<(&'static str, Token)>,
}
pub fn lex(input: String) -> anyhow::Result<Vec<(Token, TokenPosition)>> {
    let mut config = LexerConfig::default();
    config
        .keywords
        .push(("+=", Token::Assign(Some(Operator::Plus))));
    config
        .keywords
        .push(("-=", Token::Assign(Some(Operator::Minus))));
    config
        .keywords
        .push(("*=", Token::Assign(Some(Operator::Multiply))));
    config
        .keywords
        .push(("/=", Token::Assign(Some(Operator::Divide))));
    config
        .keywords
        .push(("%=", Token::Assign(Some(Operator::Modulo))));
    config.keywords.push(("(", Token::LParen));
    config.keywords.push((")", Token::RParen));
    config.keywords.push(("{", Token::LBrace));
    config.keywords.push(("}", Token::RBrace));
    config.keywords.push(("[", Token::LBrack));
    config.keywords.push(("]", Token::RBrack));
    config.keywords.push(("::", Token::DoubleColon));
    config.keywords.push((":", Token::Colon));
    config.keywords.push((",", Token::Comma));
    config.keywords.push(("->", Token::Arrow));
    config.keywords.push(("+", Token::Operator(Operator::Plus)));
    config
        .keywords
        .push(("-", Token::Operator(Operator::Minus)));
    config
        .keywords
        .push(("*", Token::Operator(Operator::Multiply)));
    config
        .keywords
        .push(("/", Token::Operator(Operator::Divide)));
    config
        .keywords
        .push(("%", Token::Operator(Operator::Modulo)));
    config.keywords.push(("&&", Token::Operator(Operator::And)));
    config.keywords.push(("||", Token::Operator(Operator::Or)));
    config.keywords.push(("^", Token::Operator(Operator::Xor)));
    config.keywords.push((
        "==",
        Token::Operator(Operator::Comparison(Comparison::Equal)),
    ));
    config.keywords.push((
        "!=",
        Token::Operator(Operator::Comparison(Comparison::NotEqual)),
    ));
    config.keywords.push((
        ">=",
        Token::Operator(Operator::Comparison(Comparison::GreaterEqual)),
    ));
    config.keywords.push((
        ">",
        Token::Operator(Operator::Comparison(Comparison::Greater)),
    ));
    config.keywords.push((
        "<=",
        Token::Operator(Operator::Comparison(Comparison::LessEqual)),
    ));
    config
        .keywords
        .push(("<", Token::Operator(Operator::Comparison(Comparison::Less))));
    config.keywords.push(("fn", Token::Fn));
    config.keywords.push(("class", Token::Class));
    config.keywords.push(("namespace", Token::Namespace));
    config.keywords.push(("struct", Token::Struct));
    config.keywords.push(("let", Token::Let));
    config.keywords.push((";", Token::Semicolon));
    config
        .keywords
        .push(("!", Token::Operator(Operator::Negate)));
    config.keywords.push((".", Token::Dot));
    config.keywords.push(("=>", Token::Cast));
    config.keywords.push(("=?>", Token::DynamicCast));
    config.keywords.push(("=", Token::Assign(None)));
    config.keywords.push(("true", Token::Bool(true)));
    config.keywords.push(("false", Token::Bool(false)));
    config.keywords.push(("&", Token::Reference));
    config.keywords.push(("if", Token::If));
    config.keywords.push(("else", Token::Else));
    config.keywords.push(("while", Token::While));
    config.keywords.push(("implements", Token::Implements));
    config.keywords.push(("depends", Token::Depends));
    config.keywords.push(("interface", Token::Interface));
    config.keywords.push(("override", Token::Override));
    config.keywords.push(("new", Token::New));
    config.keywords.push(("enum", Token::Enum));
    config.keywords.push(("mut", Token::Mut));
    config.keywords.push(("match", Token::Match));

    let mut tokens = Vec::new();

    let mut i = 0;
    let mut line = 1;
    let mut column = 0;
    loop {
        let char = match input.chars().nth(i) {
            Some(c) => c,
            None => break,
        };
        i += 1;
        column += 1;
        if char == '\n' {
            line += 1;
            column = 0;
            continue;
        }
        if char == ' ' || char == '\t' {
            continue;
        }
        if char == '/' && input.chars().nth(i) == Some('/') {
            i += 1;
            while input.chars().nth(i) != Some('\n') {
                i += 1;
            }
            i += 1;
            column = 0;
            line += 1;
            continue;
        }
        if char == '/' && input.chars().nth(i) == Some('*') {
            while input.chars().nth(i).unwrap() != '*'
                || match input.chars().nth(i + 1) {
                    Some(ch) => ch,
                    None => return Err(anyhow!("unexpected EOF")),
                } != '/'
            {
                i += 1;
                column += 1;
                if input.chars().nth(i - 1).unwrap() == '\n' {
                    line += 1;
                    column = 0;
                }
            }
            i += 2;
            column += 2;
            continue;
        }
        {
            let mut found = false;
            for (chars, token) in &config.keywords {
                if input[i - 1..].starts_with(chars) {
                    tokens.push((
                        token.clone(),
                        TokenPosition {
                            line,
                            column,
                            length: chars.len(),
                        },
                    ));
                    i += chars.len() - 1;
                    column += chars.len() - 1;
                    found = true;
                    break;
                }
            }
            if found {
                continue;
            }
        }
        if char.is_digit(10) {
            let mut has_dot = false;
            let mut number = String::new();
            number.push(char);
            loop {
                let next_char = match input.chars().nth(i) {
                    Some(c) => c,
                    None => break,
                };
                if next_char.is_digit(10) {
                    number.push(next_char);
                } else if next_char == '.' && !has_dot {
                    has_dot = true;
                    number.push('.');
                } else {
                    break;
                }
                i += 1;
                column += 1;
            }
            let length = number.len();
            if has_dot {
                tokens.push((
                    Token::Number(number.parse::<f64>().unwrap()),
                    TokenPosition {
                        line,
                        column: column - length,
                        length,
                    },
                ));
            } else {
                tokens.push((
                    Token::Integer(number.parse::<i64>().unwrap()),
                    TokenPosition {
                        line,
                        column: column - length,
                        length,
                    },
                ));
            }
            continue;
        }
        fn is_valid_identifier(character: char, first: bool) -> bool {
            character.is_alphabetic() || character == '_' || (!first && character.is_digit(10))
        }
        if is_valid_identifier(char, true) {
            let mut identifier = String::new();
            identifier.push(char);
            loop {
                let next_char = match input.chars().nth(i) {
                    Some(c) => c,
                    None => break,
                };
                if is_valid_identifier(next_char, false) {
                    identifier.push(next_char);
                } else {
                    break;
                }
                i += 1;
                column += 1;
            }
            let length = identifier.len();
            tokens.push((
                Token::Identifier(identifier),
                TokenPosition {
                    line,
                    column: column - length,
                    length,
                },
            ));
            continue;
        }
        if char == '"' {
            let mut string = String::new();
            loop {
                let next_char = match input.chars().nth(i) {
                    Some(ch) => ch,
                    None => return Err(anyhow!("unexpected EOF")),
                };
                i += 1;
                column += 1;
                if next_char == '\n' {
                    return Err(anyhow!("unexpected newline"));
                }
                if next_char == '"' {
                    break;
                }
                string.push(next_char);
            }
            let length = string.len();
            tokens.push((
                Token::String(string),
                TokenPosition {
                    line,
                    column: column - length,
                    length,
                },
            ));
            continue;
        }
        return Err(anyhow!("unexpected character {}", char));
    }
    Ok(tokens)
}
#[derive(Clone)]
pub struct TokenList {
    tokens: Vec<(Token, TokenPosition)>,
}
impl TokenList {
    pub fn lex(text: String) -> anyhow::Result<TokenList> {
        let mut tokens = lex(text)?;
        tokens.reverse();
        Ok(TokenList { tokens })
    }
    pub fn peek(&self) -> anyhow::Result<&(Token, TokenPosition)> {
        self.tokens.last().ok_or_else(|| anyhow!("eof"))
    }
    pub fn peek_nth(&self, n: usize) -> anyhow::Result<&(Token, TokenPosition)> {
        self.tokens
            .get(self.tokens.len() - 1 - n)
            .ok_or_else(|| anyhow!("eof"))
    }
    pub fn take(&mut self) -> anyhow::Result<(Token, TokenPosition)> {
        self.tokens.pop().ok_or_else(|| anyhow!("eof"))
    }
    pub fn expect_token(&mut self, expected: Token) -> anyhow::Result<TokenPosition> {
        let token = self.take()?;
        match token.0 == expected {
            true => Ok(token.1),
            false => Err(anyhow!(
                "expected {:?} but got {:?} at {:?}",
                expected,
                token.0,
                token.1
            )),
        }
    }
    pub fn is_expected_and_take(
        &mut self,
        expected: Token,
    ) -> anyhow::Result<(bool, TokenPosition)> {
        let token = match self.peek() {
            Ok(token) => token,
            Err(_) => return Err(anyhow!("unexpected eof")),
        };
        let position = token.1.clone();
        let mut same = token.0 == expected;
        if same {
            self.take().unwrap();
        }
        Ok((same, position))
    }
    pub fn expect_identifier(&mut self) -> anyhow::Result<(String, TokenPosition)> {
        match self.take()? {
            (Token::Identifier(name), position) => Ok((name, position)),
            (position, token) => Err(anyhow!(
                "expected identifier, but got {:?} at {:?}",
                position,
                token
            )),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
    pub fn try_parse<T>(
        &mut self,
        function: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> Option<T> {
        let rollback = self.clone();
        match function(self) {
            Ok(value) => Some(value),
            Err(_) => {
                *self = rollback;
                None
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Negate,
    And,
    Or,
    Xor,
    Comparison(Comparison),
}
impl Operator {
    pub fn get_name(self) -> &'static str {
        match self {
            Operator::Plus => "add",
            Operator::Minus => "sub",
            Operator::Multiply => "mul",
            Operator::Divide => "div",
            Operator::Modulo => "mod",
            Operator::Negate => "neg",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Xor => "xor",
            Operator::Comparison(_) => "cmp",
        }
    }
    pub fn precedence(self) -> u8 {
        match self {
            Operator::Negate => 0,
            Operator::Plus | Operator::Minus => 1,
            Operator::Multiply | Operator::Divide | Operator::Modulo => 2,
            Operator::Comparison(_) => 3,
            Operator::Or | Operator::Xor => 4,
            Operator::And => 5,
        }
    }
}
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Comparison {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}
impl Operator {
    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Minus | Operator::Negate => true,
            _ => false,
        }
    }
    pub fn is_middle_op(&self) -> bool {
        match self {
            Operator::Negate => false,
            _ => true,
        }
    }
}
