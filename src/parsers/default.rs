use super::{pts, ParserError};
use crate::parsers::grammars::default::{DefaultParser, Rule};
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error as PestError;
use crate::pts::variable_map::Variable;
use crate::pts::linear_polynomial::Constant;

macro_rules! invariant_error {
    () => {
        "Programmer error: Function invariants not upheld."
    };
}

#[derive(Debug, PartialEq, Eq)]
enum Operation {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Power
}

type PestResult<'a> = Result<Pairs<'a, Rule>, PestError<Rule>>;

pub fn parse<'a>(input: &str) -> Result<pts::PTS<'a>, ParserError> {
    todo!();
}

// assumes the parses rule is Rule::variable
fn parse_variable<'a>(parse: Pair<'a, Rule>) -> Variable {
    Variable::new(parse.as_str())
}

// assumes the parses rule is Rule::constant
fn parse_constant<'a>(parse: Pair<'a, Rule>) -> Constant {
    // all parses have to follow f64 grammar, no need to handle errors
    parse.as_str().parse::<f64>().expect(invariant_error!())
}

// assumes the parses rule is Rule::arithmetic_op or Rule::additive_op
fn parse_operation<'a>(parse: Pair<'a, Rule>) -> Operation {
    match parse.as_str() {
        "+" => Operation::Addition,
        "-" => Operation::Subtraction,
        "*" => Operation::Multiplication,
        "/" => Operation::Division,
        "^" => Operation::Power,
        _ => panic!(invariant_error!()),
    }
}

// assumes the parses rule is Rule::constant_expr
fn parse_constant_expr<'a>(parse: Pair<'a, Rule>) -> Constant {
    
    //TODO: doesn't do operation priority when chaining multiplicative and additive operations
    let mut iter = parse.into_inner().into_iter();
    let first = iter.next().unwrap();
    let mut acc = match first.as_rule() {
        Rule::constant => parse_constant(first),
        Rule::constant_expr => parse_constant_expr(first),
        _ => panic!(invariant_error!()),
    };

    loop {
        let op = iter.next();
        if op.is_none() { 
            return acc;
        }
        match parse_operation(op.unwrap()) {
            Operation::Addition => acc += parse_constant_expr(iter.next().unwrap()),
            Operation::Subtraction => acc -= parse_constant_expr(iter.next().unwrap()),
            Operation::Multiplication => acc *= parse_constant_expr(iter.next().unwrap()),
            Operation::Division => acc /= parse_constant_expr(iter.next().unwrap()),
            Operation::Power => acc = acc.powf(parse_constant_expr(iter.next().unwrap())),
        }
    }
    // unreachable!();
}

#[cfg(test)]
mod tests {
    use super::{DefaultParser, Rule, Parser, parse_variable, parse_constant, Operation, parse_operation, parse_constant_expr};
    use std::iter::zip;

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

    #[test]
    fn operation_sanity() {
        let inputs = vec!("+", "-", "*", "/", "^");
        let ops = vec!(Operation::Addition, Operation::Subtraction, Operation::Multiplication, Operation::Division, Operation::Power);

        for (input, op) in zip(inputs, ops) {
            let mut parse = DefaultParser::parse(Rule::arithmetic_op, input).unwrap();
            assert_eq!(parse_operation(parse.next().unwrap()), op);
            assert!(parse.next().is_none());
        }
    }

    #[test]
    fn constant_expr_sanity() {
        let mut parse = DefaultParser::parse(Rule::constant_expr, "((4^2 + 5) - (2 * 2 / 2))").unwrap();
        assert_eq!(parse_constant_expr(parse.next().unwrap()), 19.0);
        assert!(parse.next().is_none());
    }
}

