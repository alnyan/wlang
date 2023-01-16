use std::str::FromStr;

pub trait FromChar: Sized {
    fn from_char(s: char) -> Option<Self>;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Static,
    Let,
    Const,
    Operator,
    Extern,
    Fn,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BasicOperator {
    Assign,             // =
    FatArrow,           // =>
    Arrow,              // ->
    Add,                // +
    Sub,                // -
    Mul,                // *
    Div,                // /
    Mod,                // %
    Colon,              // :
    DoubleColon,        // ::
    Dot,                // .
    Question,           // ?
    Lt,                 // <
    Gt,                 // >
    Le,                 // <=
    Ge,                 // >=
    Comment,            // //
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    BasicOperator(BasicOperator),
    CustomOperator(String),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Ident(String),
    IntegerLiteral(u64),
}

impl Token {
    pub const fn is_operator(&self) -> bool {
        matches!(self, Self::BasicOperator(_) | Self::CustomOperator(_))
    }
}

impl FromChar for Punctuation {
    fn from_char(s: char) -> Option<Self> {
        match s {
            ';' => Some(Punctuation::Semicolon),
            ',' => Some(Punctuation::Comma),
            '(' => Some(Punctuation::LParen),
            ')' => Some(Punctuation::RParen),
            '[' => Some(Punctuation::LBracket),
            ']' => Some(Punctuation::RBracket),
            '{' => Some(Punctuation::LBrace),
            '}' => Some(Punctuation::RBrace),
            _ => None
        }
    }
}

impl FromStr for BasicOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "=" => Ok(Self::Assign),
            "=>" => Ok(Self::FatArrow),
            "->" => Ok(Self::Arrow),
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mul),
            "/" => Ok(Self::Div),
            "%" => Ok(Self::Mod),
            ":" => Ok(Self::Colon),
            "::" => Ok(Self::DoubleColon),
            "?" => Ok(Self::Question),
            "." => Ok(Self::Dot),
            ">" => Ok(Self::Gt),
            "<" => Ok(Self::Lt),
            ">=" => Ok(Self::Ge),
            "<=" => Ok(Self::Le),
            "//" => Ok(Self::Comment),
            _ => Err(())
        }
    }
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "operator" => Ok(Self::Operator),
            "static" => Ok(Self::Static),
            "fn" => Ok(Self::Fn),
            "let" => Ok(Self::Let),
            "const" => Ok(Self::Const),
            "extern" => Ok(Self::Extern),
            _ => Err(())
        }
    }
}
