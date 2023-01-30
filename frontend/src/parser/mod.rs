use ast::Token;

use crate::lexer::LexerError;

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(Token, Vec<String>),
    LexerError(LexerError),
}

macro_rules! expect_match {
    ($input:expr, $pattern:pat, $msg:expr) => {
        let Some(_token) = $crate::input::Input::next($input)? else {
                            return Err($crate::parser::ParserError::UnexpectedEof);
                        };

        let $pattern = _token.value else {
                            return Err($crate::parser::ParserError::unexpected_token(_token, $msg));
                        };
    };
}

macro_rules! expect {
    ($input:expr, $value:expr) => {
        let Some(_token) = $crate::input::Input::next($input)? else {
                                            return Err($crate::parser::ParserError::UnexpectedEof);
                                        };

        if _token.value != $value {
            return Err($crate::parser::ParserError::unexpected_token(
                _token,
                vec![$value],
            ));
        }
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

impl ParserError {
    pub fn unexpected_token<S, I: IntoIterator<Item = S>>(token: Token, it: I) -> Self
    where
        String: From<S>,
    {
        Self::UnexpectedToken(token, it.into_iter().map(String::from).collect())
    }
}

impl From<()> for ParserError {
    fn from(_: ()) -> Self {
        todo!()
    }
}

impl From<LexerError> for ParserError {
    fn from(e: LexerError) -> Self {
        Self::LexerError(e)
    }
}

pub mod combinator;
pub mod expr;
pub mod program;
