use ast::Token;

use crate::lexer::LexerError;

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(Token, String),
}

macro_rules! expect {
    ($input:expr, $pattern:pat, $msg:expr) => {
        let Some(_token) = $crate::input::Input::next($input)? else {
                            return Err($crate::parser::ParserError::UnexpectedEof);
                        };

        let $pattern = _token else {
                            return Err($crate::parser::ParserError::UnexpectedToken(_token, $msg));
                        };
    };
}

#[macro_export]
macro_rules! def_parser {
    ($vis:vis $func_name:ident<$input_ty:ident $(, $extra_generic:ident $(: $constraint:path)?)*>
        ($($arg:ident: $arg_ty:ty),+) -> $res_ty:ty $body:block) =>
    (
        $vis fn $func_name<$input_ty: $crate::input::Input<$crate::token::Token> $(, $extra_generic $(: $constraint)?)*>
            ($($arg: $arg_ty),+) -> Result<$res_ty, $crate::parser::ParserError>
            where $crate::parser::ParserError: From<<$input_ty as $crate::input::Input<$crate::token::Token>>::Error>
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
