pub mod default;
mod grammars;
use crate::pts::PTS;

use std::error;
use std::fmt;

type LineColumnPair = (usize, usize);

#[derive(Debug)]
pub struct ParserError{
    location: ErrorLocation,
    message: String
}

#[derive(Debug)]
pub enum ErrorLocation {
    Position(LineColumnPair),
    Span(LineColumnPair, LineColumnPair)
}

impl fmt::Display for ErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Position((line, col)) => write!(f, "{}, {}", line, col),
            Self::Span((line_start, col_start), (line_end, col_end)) => write!(f, "{}, {} - {}, {}", line_start, col_start, line_end, col_end),
        
        }
    }
}

impl error::Error for ParserError{}
impl fmt::Display for ParserError{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.message)
    }
}

pub enum Parser {
    Default
}

pub fn parse<'a>(parser: Parser, input: &str)
                                        -> Result<PTS, ParserError> {
    match parser {
        Parser::Default => default::parse(input)
    }
}

