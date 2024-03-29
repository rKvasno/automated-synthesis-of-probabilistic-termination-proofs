pub mod default;
mod grammars;
pub mod linear_polynomial;
use pest::error::Error as PestError;
use pest::RuleType;

use crate::pts::PTS;

use std::error;
use std::fmt;

pub const INVARIANT_ERROR: &'static str = "Programmer error: Function invariants not upheld.";

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

pub trait Parser {
    fn parse(input: &str) -> Result<PTS, ParserError>;
}

pub fn handle_pest_error<R: RuleType>(error: PestError<R>) -> ParserError {
    ParserError {
        message: error.to_string(),
    }
}
