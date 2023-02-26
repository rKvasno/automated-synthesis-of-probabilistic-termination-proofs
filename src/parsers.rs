use std::char;
use std::error;
use std::fmt;
use std::io::Read;
use super::alts;
pub mod default;

#[derive(Debug)]
struct ParserError{
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

pub fn parse<Reader: Read>(parser: Parser, input: Reader)
                                        -> Result<alts::ALTS, ParserError> {
    match parser {
        Default => default::parse(input)
    }
}

