use std::error;
use std::fmt;
use std::io::Read;
use super::pts;
pub mod default;

#[derive(Debug)]
pub struct ParserError{
    line: u32,
    column: u32,
    message: String
}

impl error::Error for ParserError{}
impl fmt::Display for ParserError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {}: {}", self.line, self.column, self.message)
    }
}

pub enum Parser {
    Default
}

pub fn parse<'a, Reader: Read>(parser: Parser, input: Reader)
                                        -> Result<pts::PTS<'a>, ParserError> {
    match parser {
        Default => default::parse(input)
    }
}

