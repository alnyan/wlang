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
    RBracket,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Static,
    Let,
    Const,
    Operator,
    Extern,
    Fn,
    If,
    Else,
    While,
    Loop,
    Break,
    Continue,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BasicOperator {
    Assign,      // =
    FatArrow,    // =>
    Arrow,       // ->
    Add,         // +
    Sub,         // -
    Mul,         // *
    Div,         // /
    Mod,         // %
    Colon,       // :
    DoubleColon, // ::
    Dot,         // .
    Question,    // ?
    Lt,          // <
    Gt,          // >
    Le,          // <=
    Ge,          // >=
    Eq,          // ==
    Ne,          // !=
    Comment,     // //
    And,         // &&
    Or,          // ||
    BitAnd,      // &
    BitOr,       // |
    Shl,         // <<
    Shr,         // >>
    As,          // as
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    BasicOperator(BasicOperator),
    CustomOperator(String),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Ident(String),
    IntegerLiteral(u64, String),
    StringLiteral(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourcePosition {
    pub line: u64,
    pub column: u64
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub position: SourcePosition,
    pub value: TokenValue
}

impl TokenValue {
    pub const fn is_operator(&self) -> bool {
        matches!(self, Self::BasicOperator(_) | Self::CustomOperator(_))
    }

    pub const fn to_str(&self) -> &'static str {
        match self {
            TokenValue::Punctuation(p) => match p {
                Punctuation::Comma => "`,'",
                Punctuation::Semicolon => "`;'",
                Punctuation::LParen => "`('",
                Punctuation::LBrace => "`{'",
                Punctuation::LBracket => "`['",
                Punctuation::RParen => "`)'",
                Punctuation::RBrace => "`}'",
                Punctuation::RBracket => "`]'",
            },
            _ => todo!()
        }
    }
}

impl BasicOperator {
    pub const fn is_comparison(&self) -> bool {
        matches!(
            self,
            Self::Ne | Self::Eq | Self::Gt | Self::Lt | Self::Ge | Self::Le
        )
    }

    pub const fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Mod | Self::BitAnd | Self::BitOr
        )
    }

    pub const fn is_logic(&self) -> bool {
        matches!(self, Self::And | Self::Or)
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
            _ => None,
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
            "==" => Ok(Self::Eq),
            "!=" => Ok(Self::Ne),
            "//" => Ok(Self::Comment),
            "&&" => Ok(Self::And),
            "||" => Ok(Self::Or),
            "&" => Ok(Self::BitAnd),
            "|" => Ok(Self::BitOr),
            "<<" => Ok(Self::Shl),
            ">>" => Ok(Self::Shr),
            "as" => Ok(Self::As),
            _ => Err(()),
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
            "return" => Ok(Self::Return),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
            "while" => Ok(Self::While),
            "break" => Ok(Self::Break),
            _ => Err(()),
        }
    }
}
