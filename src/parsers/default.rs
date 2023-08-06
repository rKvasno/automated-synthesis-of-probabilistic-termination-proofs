use super::{pts, ParserError};
use crate::parsers::grammars::default::{DefaultParser, Rule};
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error as PestError;
use crate::pts::variable_map::{Variable, VariableMap};
use crate::pts::linear_polynomial::{Constant, Term, LinearPolynomial};

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

// assumes the parses rule is Rule::power_op, Rule::multiplicative_op or Rule::additive_op
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

// assumes the parses rule is Rule::term
fn parse_term<'a>(parse: Pair<'a, Rule>) -> Term {
    let mut pairs = parse.into_inner();
    // TODO: replace the hard coded defaults
    let mut variable: Option<Variable> = None;
    let mut coefficient = 1.0;
    for pair in pairs {
        match pair.as_rule() {
            Rule::variable => variable = Some(parse_variable(pair)),
            Rule::constant_expr => coefficient = parse_constant_expr(pair),
            _ => panic!(invariant_error!()),
        }
    }
    Term::new(variable, coefficient)
}

// assumes the parses rule is Rule::linear_polynomial
fn parse_linear_polynomial<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> LinearPolynomial {
    let mut op = Operation::Addition;
    let mut pairs = parse.into_inner();
    let mut pol = LinearPolynomial::new();
    for pair in pairs {
        match pair.as_rule() {
            Rule::additive_op => op = parse_operation(pair),
            Rule::term if op == Operation::Addition => pol.add_term(map, parse_term(pair)),
            Rule::term if op == Operation::Subtraction => pol.add_term(map, -parse_term(pair)),
            _ => panic!(invariant_error!()),
        }
    }
    pol
}


#[cfg(test)]
mod tests {
    use super::{DefaultParser, Rule, Parser, Variable, parse_variable, parse_constant, Operation, parse_operation, parse_constant_expr, Term, parse_term, parse_linear_polynomial, VariableMap};
    use std::iter::zip;
    use crate::pts::misc::{check_terms};

    #[test]
    fn variable_sanity() {
        let variable = Variable::new("abc");
        let mut parse = DefaultParser::parse(Rule::variable, variable.as_str()).unwrap();
        let var = parse_variable(parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(var.as_str(), variable.as_str());
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
        let rules = vec!(Rule::additive_op, Rule::additive_op, Rule::multiplicative_op, Rule::multiplicative_op, Rule::power_op);

        let zipped = zip(zip(inputs, ops), rules);

        for ((input, op), rule) in zipped {
            let mut parse = DefaultParser::parse(rule, input).unwrap();
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

    #[test]
    fn term_sanity() {
        let mut parse = DefaultParser::parse(Rule::term, "5a").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), 5.0));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a * 5").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), 5.0));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), 1.0));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "5").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(None, 5.0));
        assert!(parse.next().is_none());
        
    }

    #[test]
    fn linear_polynomial_sanity() {
        let mut parse = DefaultParser::parse(Rule::linear_polynomial, "- a + 5 -(1/2) * b").unwrap();
        let mut map = VariableMap::new();
        let pol = parse_linear_polynomial(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        check_terms(&pol, &map, vec!(Some(5.0), Some(-1.0), Some(-0.5)));
    }

}

