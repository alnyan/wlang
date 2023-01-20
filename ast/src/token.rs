use macros::ToFromEnum;
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, ToFromEnum)]
pub enum Punctuation {
    #[to_from(';')]
    Semicolon,
    #[to_from(',')]
    Comma,
    #[to_from('(')]
    LParen,
    #[to_from(')')]
    RParen,
    #[to_from('{')]
    LBrace,
    #[to_from('}')]
    RBrace,
    #[to_from('[')]
    LBracket,
    #[to_from(']')]
    RBracket,
}

#[derive(Debug, Clone, Copy, PartialEq, ToFromEnum)]
pub enum Keyword {
    #[to_from("static")]
    Static,
    #[to_from("let")]
    Let,
    #[to_from("const")]
    Const,
    #[to_from("operator")]
    Operator,
    #[to_from("extern")]
    Extern,
    #[to_from("fn")]
    Fn,
    #[to_from("if")]
    If,
    #[to_from("else")]
    Else,
    #[to_from("while")]
    While,
    #[to_from("loop")]
    Loop,
    #[to_from("break")]
    Break,
    #[to_from("continue")]
    Continue,
    #[to_from("return")]
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, ToFromEnum)]
pub enum BasicOperator {
    #[to_from("=")]
    Assign, // =
    #[to_from("=>")]
    FatArrow, // =>
    #[to_from("->")]
    Arrow, // ->
    #[to_from("+")]
    Add, // +
    #[to_from("-")]
    Sub, // -
    #[to_from("*")]
    Mul, // *
    #[to_from("/")]
    Div, // /
    #[to_from("%")]
    Mod, // %
    #[to_from(":")]
    Colon, // :
    #[to_from("::")]
    DoubleColon, // ::
    #[to_from(".")]
    Dot, // .
    #[to_from("?")]
    Question, // ?
    #[to_from("<")]
    Lt, // <
    #[to_from(">")]
    Gt, // >
    #[to_from("<=")]
    Le, // <=
    #[to_from(">=")]
    Ge, // >=
    #[to_from("==")]
    Eq, // ==
    #[to_from("!=")]
    Ne, // !=
    #[to_from("//")]
    Comment, // //
    #[to_from("&&")]
    And, // &&
    #[to_from("||")]
    Or, // ||
    #[to_from("&")]
    BitAnd, // &
    #[to_from("|")]
    BitOr, // |
    #[to_from("<<")]
    Shl, // <<
    #[to_from(">>")]
    Shr, // >>
    #[to_from("as")]
    As, // as
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
    pub column: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub position: SourcePosition,
    pub value: TokenValue,
}

impl TokenValue {
    pub const fn is_operator(&self) -> bool {
        matches!(self, Self::BasicOperator(_) | Self::CustomOperator(_))
    }
}

impl From<&TokenValue> for String {
    fn from(value: &TokenValue) -> Self {
        match value {
            TokenValue::Punctuation(p) => {
                format!("punctuation `{}'", <Punctuation as Into<char>>::into(*p))
            }
            TokenValue::Keyword(p) => {
                format!("keyword `{}'", <Keyword as Into<&'static str>>::into(*p))
            }
            TokenValue::BasicOperator(p) => format!(
                "operator `{}'",
                <BasicOperator as Into<&'static str>>::into(*p)
            ),
            TokenValue::Ident(name) => format!("identifier `{name}'"),
            TokenValue::CustomOperator(name) => format!("operator `{name}'"),
            TokenValue::IntegerLiteral(value, suffix) => {
                format!("integer literal `{value}{suffix}'")
            }
            TokenValue::StringLiteral(value) => format!("string literal {value:?}"),
        }
    }
}

impl From<TokenValue> for String {
    fn from(value: TokenValue) -> Self {
        String::from(&value)
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(self))
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
