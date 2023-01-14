use std::{fmt::Debug, io::BufReader, fs::File, iter::Peekable, str::Chars};

pub trait Input<T> {
    type Error: Debug;

    fn next(&mut self) -> Result<Option<T>, Self::Error>;
    fn peek(&mut self) -> Result<Option<T>, Self::Error>;
}

pub struct FileInput {
    reader: BufReader<File>
}

pub struct StrInput<'a> {
    iter: Peekable<Chars<'a>>
}

impl<'a> StrInput<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            iter: data.chars().peekable()
        }
    }
}

impl<'a> Input<char> for StrInput<'a> {
    type Error = ();

    fn next(&mut self) -> Result<Option<char>, Self::Error> {
        Ok(self.iter.next())
    }

    fn peek(&mut self) -> Result<Option<char>, Self::Error> {
        Ok(self.iter.peek().cloned())
    }
}
