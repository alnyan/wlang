use ast::Token;

use super::ParserError;

def_parser!(
    pub parse_delimited<S, T, P1: Fn(&mut S) -> Result<T, ParserError>>
        (input: &mut S, p: P1, end: Token, delim: Token) -> Vec<T> {
        let mut res = vec![];

        loop {
            let Some(token) = input.peek()? else {
                return Err(ParserError::UnexpectedEof);
            };

            if token == end {
                input.next()?.unwrap();
                break;
            } else {
                res.push(p(input)?);
            }

            let Some(token) = input.next()? else {
                return Err(ParserError::UnexpectedEof)
            };

            if token == end {
                break;
            } else if token != delim {
                return Err(ParserError::UnexpectedToken(token, format!("{delim:?}")));
            }
        }

        Ok(res)
    }
);

def_parser!(
    pub parse_many0<S, T, P1: Fn(&mut S) -> Result<T, ParserError>>
        (input: &mut S, p: P1, end: Token) -> Vec<T> {
        let mut res = vec![];

        loop {
            let Some(token) = input.peek()? else {
                return Err(ParserError::UnexpectedEof);
            };

            if token == end {
                input.next()?.unwrap();
                break;
            } else {
                res.push(p(input)?);
            }
        }

        Ok(res)
    }
);
