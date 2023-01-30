use std::{fmt::Debug, iter::Peekable, str::Chars};

use ast::token::SourcePosition;

pub trait Input<T> {
    type Error: Debug;

    fn next(&mut self) -> Result<Option<T>, Self::Error>;
    fn peek(&mut self) -> Result<Option<T>, Self::Error>;
}

pub trait InputPosition {
    fn pos(&mut self) -> SourcePosition;
}

// pub struct FileInput {
//     reader: BufReader<File>
// }

pub struct StrInput<'a> {
    iter: Peekable<Chars<'a>>,
    pos: SourcePosition,
}

impl<'a> StrInput<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            iter: data.chars().peekable(),
            pos: SourcePosition { line: 0, column: 0 },
        }
    }
}

impl<'a> Input<char> for StrInput<'a> {
    type Error = ();

    fn next(&mut self) -> Result<Option<char>, Self::Error> {
        let c = self.iter.next();
        if c == Some('\n') {
            self.pos.line += 1;
            self.pos.column = 0;
        } else {
            self.pos.column += 1;
        }
        Ok(c)
    }

    fn peek(&mut self) -> Result<Option<char>, Self::Error> {
        Ok(self.iter.peek().cloned())
    }
}

impl<'a> InputPosition for StrInput<'a> {
    fn pos(&mut self) -> SourcePosition {
        self.pos
    }
}
