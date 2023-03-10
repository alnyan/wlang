use ast::{
    token::{BasicOperator, Keyword, Punctuation, TokenValue},
    Token,
};

use crate::input::{Input, InputPosition};

pub struct Lexer<S: Input<char> + InputPosition> {
    input: S,
}

pub struct LexerInput<S: Input<char> + InputPosition> {
    lexer: Lexer<S>,
    current: Option<Token>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    Input(String),
    UnknownEscapeCharacter(char),
    Eof,
}

#[derive(Clone, Copy, PartialEq)]
pub enum EscapedCharacter {
    Newline,
}

pub enum StringElement {
    Character(char),
    EscapedCharacter(EscapedCharacter),
    Terminator,
}

impl TryFrom<char> for EscapedCharacter {
    type Error = LexerError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'n' => Ok(EscapedCharacter::Newline),
            _ => Err(LexerError::UnknownEscapeCharacter(value)),
        }
    }
}

impl From<EscapedCharacter> for char {
    fn from(value: EscapedCharacter) -> Self {
        match value {
            EscapedCharacter::Newline => '\n',
        }
    }
}

impl<S: Input<char> + InputPosition> Lexer<S>
where
    LexerError: From<S::Error>,
{
    const PUNCTUATION: &[char] = &[';', ',', '{', '}', '(', ')', '[', ']'];
    const OPERATOR: &[char] = &[
        '=', ':', '.', '?', '>', '<', '!', '+', '-', '*', '/', '%', '&', '|',
    ];

    pub fn new(input: S) -> Self {
        Self { input }
    }

    fn lex_number_extra(&mut self) -> Result<String, LexerError> {
        let mut extra = String::new();

        loop {
            let Some(c) = self.input.peek()? else {
                break;
            };

            if c.is_alphanumeric() {
                self.input.next()?.unwrap();
                extra.push(c);
            } else {
                break;
            }
        }

        Ok(extra)
    }

    fn lex_number(&mut self) -> Result<Token, LexerError> {
        let position = self.input.pos();
        let mut value = 0;
        let mut extra = String::new();

        loop {
            let Some(c) = self.input.peek()? else {
                break;
            };

            if c.is_alphabetic() {
                extra = self.lex_number_extra()?;
                break;
            } else if c.is_ascii_digit() {
                self.input.next().unwrap();
                value *= 10;
                value += (c as u8 - b'0') as u64;
            } else {
                break;
            }
        }

        Ok(Token {
            position,
            value: TokenValue::IntegerLiteral(value, extra),
        })
    }

    fn lex_operator(&mut self) -> Result<Token, LexerError> {
        let position = self.input.pos();
        let mut buf = String::new();

        loop {
            let Some(c) = self.input.peek()? else {
                break;
            };

            if Self::OPERATOR.contains(&c) {
                self.input.next()?.unwrap();
                buf.push(c);
            } else {
                break;
            }

            // Special case: `*', mul or pointer derefence
            // TODO somehow support both **x and `**' operator?
            if buf == "*" {
                break;
            }

            // Special case: comment
            if buf == "//" {
                break;
            }
        }

        Ok(Token {
            position,
            value: if let Ok(basic) = BasicOperator::try_from(buf.as_str()) {
                TokenValue::BasicOperator(basic)
            } else {
                TokenValue::CustomOperator(buf)
            },
        })
    }

    fn skip_whitespace(&mut self) -> Result<(), LexerError> {
        loop {
            let Some(c) = self.input.peek()? else {
                return Ok(());
            };

            if !c.is_whitespace() {
                return Ok(());
            }

            self.input.next()?.unwrap();
        }
    }

    fn skip_comment(&mut self) -> Result<(), LexerError> {
        loop {
            let Some(c) = self.input.next()? else {
                return Ok(());
            };

            if c == '\n' {
                return Ok(());
            }
        }
    }

    fn lex_keyword_or_ident(&mut self) -> Result<Token, LexerError> {
        let position = self.input.pos();
        let mut buf = String::new();

        loop {
            let Some(c) = self.input.peek()? else {
                break;
            };

            if c.is_alphanumeric() || c == '_' {
                self.input.next()?.unwrap();
                buf.push(c);
            } else {
                break;
            }
        }

        Ok(Token {
            position,
            value: if let Ok(op) = BasicOperator::try_from(buf.as_str()) {
                TokenValue::BasicOperator(op)
            } else if let Ok(kw) = Keyword::try_from(buf.as_str()) {
                TokenValue::Keyword(kw)
            } else {
                TokenValue::Ident(buf)
            },
        })
    }

    fn lex_string_element(&mut self) -> Result<StringElement, LexerError> {
        let Some(c) = self.input.next()? else {
            return Err(LexerError::Eof);
        };

        match c {
            '\\' => {
                let Some(c) = self.input.next()? else {
                    return Err(LexerError::Eof);
                };

                EscapedCharacter::try_from(c).map(StringElement::EscapedCharacter)
            }
            '"' => Ok(StringElement::Terminator),
            _ => Ok(StringElement::Character(c)),
        }
    }

    fn lex_string_literal(&mut self) -> Result<Token, LexerError> {
        let position = self.input.pos();
        let mut buf = String::new();

        loop {
            let e = self.lex_string_element()?;

            match e {
                StringElement::Character(c) => buf.push(c),
                StringElement::EscapedCharacter(c) => buf.push(char::from(c)),
                StringElement::Terminator => break,
            }
        }

        Ok(Token {
            position,
            value: TokenValue::StringLiteral(buf),
        })
    }

    pub fn lex_token(&mut self) -> Result<Option<Token>, LexerError> {
        let c = loop {
            self.skip_whitespace()?;

            let Some(c) = self.input.peek()? else {
                return Ok(None);
            };

            if Self::OPERATOR.contains(&c) {
                let res = self.lex_operator()?;

                if res.value == TokenValue::BasicOperator(BasicOperator::Comment) {
                    self.skip_comment()?;
                } else {
                    return Ok(Some(res));
                }
            } else {
                break c;
            }
        };

        if c.is_ascii_digit() {
            self.lex_number()
        } else if c.is_alphabetic() || c == '_' {
            self.lex_keyword_or_ident()
        } else {
            let position = self.input.pos();
            self.input.next().unwrap();

            if c == '"' {
                self.lex_string_literal()
            } else if Self::PUNCTUATION.contains(&c) {
                Ok(Token {
                    position,
                    value: TokenValue::Punctuation(Punctuation::try_from(c).unwrap()),
                })
            } else {
                todo!()
            }
        }
        .map(Some)
    }

    pub fn lex_all(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut res = vec![];

        while let Some(token) = self.lex_token()? {
            res.push(token);
        }

        Ok(res)
    }
}

impl<S: Input<char> + InputPosition> LexerInput<S> {
    pub fn new(lexer: Lexer<S>) -> Self {
        Self {
            lexer,
            current: None,
        }
    }
}

impl<S: Input<char> + InputPosition> Input<Token> for LexerInput<S>
where
    LexerError: From<<S as Input<char>>::Error>,
{
    type Error = LexerError;

    fn next(&mut self) -> Result<Option<Token>, Self::Error> {
        let old = self.peek()?;
        self.current = self.lexer.lex_token()?;
        Ok(old)
    }

    fn peek(&mut self) -> Result<Option<Token>, Self::Error> {
        if self.current.is_some() {
            Ok(self.current.clone())
        } else {
            let token = self.lexer.lex_token()?;
            self.current = token.clone();
            Ok(token)
        }
    }
}

impl From<()> for LexerError {
    fn from(_: ()) -> Self {
        Self::Input("".to_string())
    }
}

#[cfg(test)]
mod tests {
    use ast::{
        token::{BasicOperator, Keyword, Punctuation},
        Token,
    };

    use crate::input::{Input, StrInput};

    use super::{Lexer, LexerInput};

    #[test]
    fn lex_number() {
        let mut lexer = Lexer::new(StrInput::new("1234"));

        assert_eq!(lexer.lex_number(), Ok(Token::IntegerLiteral(1234)));
    }

    #[test]
    fn lex_operator() {
        let mut lexer = Lexer::new(StrInput::new("======"));

        assert_eq!(
            lexer.lex_operator(),
            Ok(Token::CustomOperator("======".to_string()))
        );

        let mut lexer = Lexer::new(StrInput::new("= => ==> -> -"));

        assert_eq!(
            (
                lexer.lex_token(),
                lexer.lex_token(),
                lexer.lex_token(),
                lexer.lex_token(),
                lexer.lex_token()
            ),
            (
                Ok(Some(Token::BasicOperator(BasicOperator::Assign))),
                Ok(Some(Token::BasicOperator(BasicOperator::FatArrow))),
                Ok(Some(Token::CustomOperator("==>".to_owned()))),
                Ok(Some(Token::BasicOperator(BasicOperator::Arrow))),
                Ok(Some(Token::BasicOperator(BasicOperator::Sub))),
            )
        );
    }

    #[test]
    fn lex_punctuation() {
        let mut lexer = Lexer::new(StrInput::new(";"));

        assert_eq!(
            lexer.lex_token(),
            Ok(Some(Token::Punctuation(Punctuation::Semicolon)))
        );
    }

    #[test]
    fn lex_keyword_or_ident() {
        let mut lexer = Lexer::new(StrInput::new("operator new fn static"));

        assert_eq!(
            (
                lexer.lex_token(),
                lexer.lex_token(),
                lexer.lex_token(),
                lexer.lex_token()
            ),
            (
                Ok(Some(Token::Keyword(Keyword::Operator))),
                Ok(Some(Token::Ident("new".to_owned()))),
                Ok(Some(Token::Keyword(Keyword::Fn))),
                Ok(Some(Token::Keyword(Keyword::Static)))
            )
        );
    }

    #[test]
    fn lex_all() {
        let mut lexer = Lexer::new(StrInput::new(
            r#"
operator? ++(a: T1, b: T2) -> T3 {
    a.x + b.y
}

fn main() {
    let x = T1::new();
    let y = T2::new();

    x ++ y
}
"#,
        ));

        let tokens = vec![
            // operator? ++(a: T1, b: T2) -> T3 {
            Token::Keyword(Keyword::Operator),
            Token::BasicOperator(BasicOperator::Question),
            Token::CustomOperator("++".to_owned()),
            Token::Punctuation(Punctuation::LParen),
            Token::Ident("a".to_owned()),
            Token::BasicOperator(BasicOperator::Colon),
            Token::Ident("T1".to_owned()),
            Token::Punctuation(Punctuation::Comma),
            Token::Ident("b".to_owned()),
            Token::BasicOperator(BasicOperator::Colon),
            Token::Ident("T2".to_owned()),
            Token::Punctuation(Punctuation::RParen),
            Token::BasicOperator(BasicOperator::Arrow),
            Token::Ident("T3".to_owned()),
            Token::Punctuation(Punctuation::LBrace),
            // a.x + b.y
            Token::Ident("a".to_owned()),
            Token::BasicOperator(BasicOperator::Dot),
            Token::Ident("x".to_owned()),
            Token::BasicOperator(BasicOperator::Add),
            Token::Ident("b".to_owned()),
            Token::BasicOperator(BasicOperator::Dot),
            Token::Ident("y".to_owned()),
            // }
            Token::Punctuation(Punctuation::RBrace),
            // fn main() {
            Token::Keyword(Keyword::Fn),
            Token::Ident("main".to_owned()),
            Token::Punctuation(Punctuation::LParen),
            Token::Punctuation(Punctuation::RParen),
            Token::Punctuation(Punctuation::LBrace),
            // let x = T1::new();
            Token::Keyword(Keyword::Let),
            Token::Ident("x".to_owned()),
            Token::BasicOperator(BasicOperator::Assign),
            Token::Ident("T1".to_owned()),
            Token::BasicOperator(BasicOperator::DoubleColon),
            Token::Ident("new".to_owned()),
            Token::Punctuation(Punctuation::LParen),
            Token::Punctuation(Punctuation::RParen),
            Token::Punctuation(Punctuation::Semicolon),
            // let y = T2::new();
            Token::Keyword(Keyword::Let),
            Token::Ident("y".to_owned()),
            Token::BasicOperator(BasicOperator::Assign),
            Token::Ident("T2".to_owned()),
            Token::BasicOperator(BasicOperator::DoubleColon),
            Token::Ident("new".to_owned()),
            Token::Punctuation(Punctuation::LParen),
            Token::Punctuation(Punctuation::RParen),
            Token::Punctuation(Punctuation::Semicolon),
            // x ++ y
            Token::Ident("x".to_owned()),
            Token::CustomOperator("++".to_owned()),
            Token::Ident("y".to_owned()),
            // }
            Token::Punctuation(Punctuation::RBrace),
        ];

        let res = lexer.lex_all().unwrap();

        assert_eq!(res.len(), tokens.len());

        for (i, (x, y)) in tokens.iter().zip(res.iter()).enumerate() {
            assert_eq!(
                x, y,
                "Token mismatch: expected={:#?}, got={:#?}, pos={}",
                x, y, i
            );
        }
    }

    #[test]
    fn lexer_input() {
        let mut input = LexerInput::new(Lexer::new(StrInput::new("a + b")));

        assert_eq!(input.peek(), Ok(Some(Token::Ident("a".to_owned()))));
        assert_eq!(input.peek(), Ok(Some(Token::Ident("a".to_owned()))));
        assert_eq!(input.next(), Ok(Some(Token::Ident("a".to_owned()))));
        assert_eq!(
            input.peek(),
            Ok(Some(Token::BasicOperator(BasicOperator::Add)))
        );
        assert_eq!(
            input.next(),
            Ok(Some(Token::BasicOperator(BasicOperator::Add)))
        );
        assert_eq!(input.peek(), Ok(Some(Token::Ident("b".to_owned()))));
        assert_eq!(input.next(), Ok(Some(Token::Ident("b".to_owned()))));
        assert_eq!(input.peek(), Ok(None));
        assert_eq!(input.next(), Ok(None));
        assert_eq!(input.peek(), Ok(None));

        let mut input = LexerInput::new(Lexer::new(StrInput::new("a + b")));

        assert_eq!(input.next(), Ok(Some(Token::Ident("a".to_owned()))));
        assert_eq!(
            input.peek(),
            Ok(Some(Token::BasicOperator(BasicOperator::Add)))
        );
        assert_eq!(
            input.next(),
            Ok(Some(Token::BasicOperator(BasicOperator::Add)))
        );
        assert_eq!(input.peek(), Ok(Some(Token::Ident("b".to_owned()))));
        assert_eq!(input.next(), Ok(Some(Token::Ident("b".to_owned()))));
        assert_eq!(input.peek(), Ok(None));
        assert_eq!(input.next(), Ok(None));
        assert_eq!(input.peek(), Ok(None));
    }
}
