use std::rc::Rc;

use crate::lexer::{token::Token, LexerError};

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub ret_type: Option<Rc<Node>>,
    pub args: Vec<(Rc<Node>, Rc<Node>)>,
    pub body: Rc<Node>
}

#[derive(Debug, Clone, PartialEq)]
pub struct GlobalDefinition {
    pub is_const: bool,
    pub name: String,
    pub ty: Rc<Node>,
    pub value: Rc<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Ident(String),
    IntegerLiteral(u64),
    Binary(Token, Rc<Node>, Rc<Node>),
    Type(String),
    Call(Rc<Node>, Vec<Rc<Node>>),
    Function(Function),
    Block(Vec<Rc<Node>>),
    Statement(Rc<Node>),
    GlobalDefinition(GlobalDefinition)
}

macro_rules! expect {
    ($input:expr, $pattern:pat) => {
        let Some(_token) = $crate::input::Input::next($input)? else {
                            return Err($crate::parser::ParserError::UnexpectedEof);
                        };

        let $pattern = _token else {
                            return Err($crate::parser::ParserError::UnexpectedToken(_token));
                        };
    };
}

#[macro_export]
macro_rules! def_parser {
    ($vis:vis $func_name:ident<$input_ty:ident $(, $extra_generic:ident $(: $constraint:path)?)*>
        ($($arg:ident: $arg_ty:ty),+) -> $res_ty:ty $body:block) =>
    (
        $vis fn $func_name<$input_ty: $crate::input::Input<$crate::lexer::token::Token> $(, $extra_generic $(: $constraint)?)*>
            ($($arg: $arg_ty),+) -> Result<$res_ty, $crate::parser::ParserError>
            where $crate::parser::ParserError: From<<$input_ty as $crate::input::Input<$crate::lexer::token::Token>>::Error>
            $body
    )
}

impl From<()> for ParserError {
    fn from(_: ()) -> Self {
        todo!()
    }
}

impl From<LexerError> for ParserError {
    fn from(_: LexerError) -> Self {
        todo!()
    }
}

pub mod combinator;
pub mod expr;
pub mod program;
