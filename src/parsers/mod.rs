pub mod default;
mod grammars;
use pest::error::Error as PestError;
use pest::RuleType;

use crate::pts::PTS;

use std::error;
use std::fmt;

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
}

impl error::Error for ParserError {}
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        std::fmt::Display::fmt(&self.message, f)
    }
}

pub enum Parser {
    Default,
}

pub fn parse<'a>(parser: Parser, input: &str) -> Result<PTS, ParserError> {
    match parser {
        Parser::Default => default::parse(input),
    }
}

pub fn handle_pest_error<R: RuleType>(error: PestError<R>) -> ParserError {
    ParserError {
        message: error.to_string(),
    }
}
