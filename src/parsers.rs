use std::char;
use std::error;
use std::fmt;
use super::alts;
pub mod default;

#[derive(Debug)]
struct ParserError{
    line: u32,
    message: String
}

impl error::Error for ParserError{}
impl fmt::Display for ParserError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.line, self.message)
    }
}

pub enum Parser {
    Default
}

pub fn parse<IStream: Iterator<char>>(parser: Parser, input: IStream)
                                        -> Result<alts::ALTS, ParserError> {
    match parser {
        Default => default::parse(input)
    }
}

