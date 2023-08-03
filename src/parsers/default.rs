use super::{pts, ParserError};
use crate::parsers::grammars::default::{DefaultParser, Rule};
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error as PestError;
use crate::pts::variable_map::Variable;
use crate::pts::linear_polynomial::Constant;
use std::f64;


type PestResult<'a> = Result<Pairs<'a, Rule>, PestError<Rule>>;

pub fn parse<'a>(input: &str) -> Result<pts::PTS<'a>, ParserError> {
    todo!();
}

fn parse_variable<'a>(parse: Pair<'a, Rule>) -> Variable {
    Variable::new(parse.as_str())
}

fn parse_constant<'a>(parse: Pair<'a, Rule>) -> Constant {
    // all parses have to follow f64 grammar, no need to handle errors
    parse.as_str().parse::<f64>().unwrap()
}

#[cfg(test)]
mod tests {
    use super::{DefaultParser, Rule, Parser, parse_variable, parse_constant};

    #[test]
    fn variable_sanity() {
        let variable_name = "abc";
        let mut parse = DefaultParser::parse(Rule::variable, variable_name).unwrap();
        let var = parse_variable(parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(var.as_str(), variable_name);
    }

    #[test]
    fn constant_sanity() {
        let mut parse = DefaultParser::parse(Rule::constant, "123").unwrap();
        let constant = parse_constant(parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(constant, 123.0);
    }
}

