use super::{pts, ParserError};
use crate::parsers::grammars::default::DefaultParser;
use pest::Parser;

pub fn parse<'a>(input: &str) -> Result<pts::PTS<'a>, ParserError> {
    todo!();
}

