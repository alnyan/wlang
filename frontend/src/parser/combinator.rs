use ast::token::TokenValue;

use super::ParserError;

def_parser!(
    pub parse_delimited<S, T, P1: Fn(&mut S) -> Result<T, ParserError>>
        (input: &mut S, p: P1, end: TokenValue, delim: TokenValue) -> Vec<T> {
        let mut res = vec![];

        loop {
            let Some(token) = input.peek()? else {
                return Err(ParserError::UnexpectedEof);
            };

            if token.value == end {
                input.next()?.unwrap();
                break;
            } else {
                res.push(p(input)?);
            }

            let Some(token) = input.next()? else {
                return Err(ParserError::UnexpectedEof)
            };

            if token.value == end {
                break;
            } else if token.value != delim {
                return Err(ParserError::unexpected_token(token, vec![delim, end]));
            }
        }

        Ok(res)
    }
);

def_parser!(
    pub parse_many0<S, T, P1: Fn(&mut S) -> Result<T, ParserError>>
        (input: &mut S, p: P1, end: TokenValue) -> Vec<T> {
        let mut res = vec![];

        loop {
            let Some(token) = input.peek()? else {
                return Err(ParserError::UnexpectedEof);
            };

            if token.value == end {
                input.next()?.unwrap();
                break;
            } else {
                res.push(p(input)?);
            }
        }

        Ok(res)
    }
);
