use crate::pts::PTS;
use crate::pts::guard::Guards;
use crate::pts::transition::Transition;
use crate::{pts, parsers};
use parsers::grammars::default::{DefaultParser, Rule};
use parsers::ParserError;
use pts::variable_map::{Variable, VariableMap};
use pts::linear_polynomial::LinearPolynomial;
use pts::linear_polynomial::term::Term;
use pts::linear_polynomial::constant::{ONE, Constant};
use pts::transition::Assignment;
use pts::inequality::{Inequality, ComparisonOperator, InequalitySystem};
use pts::location::LocationHandle;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::error::Error as PestError;
use std::iter::{zip, once};

macro_rules! invariant_error {
    () => {
        "Programmer error: Function invariants not upheld."
    };
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Power
}

type PestResult<'a> = Result<Pairs<'a, Rule>, PestError<Rule>>;

pub fn parse<'a>(input: &str) -> Result<pts::PTS, ParserError> {
    todo!();
}

// assumes the parses rule is Rule::variable
fn parse_variable<'a>(parse: Pair<'a, Rule>) -> Variable {
    Variable::new(parse.as_str())
}

// assumes the parses rule is Rule::constant
fn parse_constant<'a>(parse: Pair<'a, Rule>) -> Constant {
    // all parses have to follow f64 grammar, no need to handle errors
    parse.as_str().parse::<Constant>().expect(invariant_error!())
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
            Operation::Power => acc = acc.pow(parse_constant_expr(iter.next().unwrap())),
        }
    }
    // unreachable!();
}

// assumes the parses rule is Rule::term
fn parse_term<'a>(parse: Pair<'a, Rule>) -> Term {
    let mut pairs = parse.into_inner();
    let mut variable: Option<Variable> = None;
    let mut coefficient = ONE;
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

// assumes the parses rule is Rule::assign_inst
fn parse_assign<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> Assignment {
    let mut pairs = parse.into_inner();
    let var: Variable = parse_variable(pairs.next().unwrap());
    map.find_or_add(var.clone());
    let pol: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    Assignment::new(var, pol)
}

// assumes the parses rule is Rule::comparison_op
fn parse_comparison_op<'a> (parse: Pair<'a, Rule>) -> ComparisonOperator {
    match parse.as_str(){
        ">" => ComparisonOperator::GT,
        ">=" => ComparisonOperator::GE,
        "<" => ComparisonOperator::LT,
        "<=" => ComparisonOperator::LE,
        _ => panic!(invariant_error!()),
    }
}

// assumes pairs is the iterator of a parse with rule Rule::logic_condition
fn parse_inequality<'a>(map: &mut VariableMap, pairs: &mut Pairs<'a, Rule>) -> Inequality {
    let lhs: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    let op = parse_comparison_op(pairs.next().unwrap());
    let rhs: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    Inequality::new(lhs, op, rhs)
}

// assumes the parses rule is Rule::logic_condition
fn parse_inequality_system<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> InequalitySystem {
    let mut pairs = parse.into_inner();
    let mut system = InequalitySystem::new();
    while pairs.peek().is_some() {
        system.push(parse_inequality(map, &mut pairs));
    }
    system
}

// assumes the parses rule is Rule::program
fn parse_program<'a, 'b>(pts: &'a mut PTS, parse: Pair<'b, Rule>) {
    let mut transition = Transition::default();
    let mut iter = parse.into_inner();
    parse_locations(pts, iter.next().unwrap(), &mut transition, pts.locations.get_terminating_location());
    pts.locations.initial = transition.target;
    pts.locations.set_invariant(pts.locations.get_terminating_location(), parse_inequality_system(&mut pts.variables, iter.next().unwrap())) ; 
}

// assumes the parses rule is Rule::locations
fn parse_locations<'a, 'b>(pts: &'a mut PTS, parse: Pair<'b, Rule>, start_transition: &mut Transition, end: LocationHandle) {
    let parse_locations = parse.into_inner();
    let mut pts_locations = pts.locations.new_n_locations(parse_locations.len()).peekable();

    // locations nonterminal always has atleast one location
    start_transition.target = pts_locations.peek().unwrap().clone();

    for ((local_start, local_end), pair) in zip(zip(pts_locations.clone(), pts_locations.skip(1).chain(once(end))), parse_locations) {
        let mut location_iter = pair.into_inner();
        pts.locations.set_invariant(local_start, parse_inequality_system(&mut pts.variables, location_iter.next().unwrap()));

        let instruction_parse = location_iter.next().unwrap();
        match instruction_parse.as_rule() {
            Rule::if_inst => parse_if(pts, instruction_parse, local_start, local_end),
            Rule::prob_inst => parse_odds(pts, instruction_parse, local_start, local_end),
            Rule::nondet_inst => parse_nondet(pts, instruction_parse, local_start, local_end),
            Rule::while_inst => parse_while(pts, instruction_parse, local_start, local_end),
            // can unwrap here, since local_start can't be None and parse_assign always returns
            Rule::assign_inst => pts.locations.set_outgoing(
                                    local_start,
                                    Guards::Unguarded(
                                        Box::new(
                                            Transition {
                                                assignments: vec!(
                                                    parse_assign(
                                                        &mut pts.variables,
                                                        instruction_parse
                                                    )
                                                ),
                                                target: local_end 
                                            }
                                        )
                                    )
                                ).unwrap(),
            _=> panic!(invariant_error!()),
        }
    }
}

fn parse_while<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, start: LocationHandle, end: LocationHandle) {
    todo!()
}

fn parse_nondet<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, start: LocationHandle, end: LocationHandle) {
    todo!()
}

fn parse_if<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, start: LocationHandle, end: LocationHandle) {
    todo!()
}

fn parse_odds<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, start: LocationHandle, end: LocationHandle) {
    todo!()
}


#[cfg(test)]
mod tests {
    use super::{DefaultParser, Rule, Operation, Variable, Parser, Term, VariableMap, Constant,
    parse_variable, parse_constant, parse_operation, parse_constant_expr, parse_term, parse_linear_polynomial};
    use std::iter::zip;
    use crate::{misc::{check_terms, setup_test_map}, parsers::default::{parse_assign, parse_inequality, parse_inequality_system}};

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
        assert_eq!(constant, Constant::new(123.0));
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
        assert_eq!(parse_constant_expr(parse.next().unwrap()), Constant::new(19.0));
        assert!(parse.next().is_none());
    }

    #[test]
    fn term_sanity() {
        let mut parse = DefaultParser::parse(Rule::term, "5a").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), Constant::new(5.0)));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a * 5").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), Constant::new(5.0)));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(Some(Variable::new("a")), Constant::new(1.0)));
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "5").unwrap();
        assert_eq!(parse_term(parse.next().unwrap()), Term::new(None, Constant::new(5.0)));
        assert!(parse.next().is_none());
        
    }

    #[test]
    fn linear_polynomial_sanity() {
        let mut parse = DefaultParser::parse(Rule::linear_polynomial, "- a + 5 -(1/2) * b").unwrap();
        let mut map = VariableMap::default();
        let pol = parse_linear_polynomial(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        check_terms(&pol, &map, vec!(Some(Constant::new(5.0)), Some(Constant::new(-1.0)), Some(Constant::new(-0.5))));
    }

    #[test]
    fn assignment_sanity() {
        let mut parse = DefaultParser::parse(Rule::assign_inst, "x = -2a + 4b - 0c - 2").unwrap();
        let mut map = VariableMap::default();
        let assign = parse_assign(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(assign.0, Variable::new("x"));
        check_terms(&assign.1, &map, vec!(Some(Constant::new(-2.0)), Some(Constant::new(0.0)), Some(Constant::new(-2.0)), Some(Constant::new(4.0)), Some(Constant::new(0.0))));
    }

    #[test]
    fn inequality_sanity() {
        let mut map = VariableMap::default();
        let mut parse = DefaultParser::parse(Rule::logic_condition, "3a - 4 + b < 0").unwrap();
        let mut pairs = parse.next().unwrap().into_inner();
        assert!(parse.next().is_none());
        let cond = parse_inequality(&mut map, &mut pairs);
        assert!(pairs.next().is_none());
        assert!(cond.is_strict());
        check_terms(&cond.as_linear_polynomial(), &map, vec!(Some(Constant::new(-4.0)), Some(Constant::new(3.0)), Some(Constant::new(1.0))));
    }

    #[test]
    fn inequality_system_sanity() {
        let mut map = setup_test_map();
        let mut parse = DefaultParser::parse(Rule::logic_condition, "- 2b - 4 < - a and 0 >= 0").unwrap();
        let system = parse_inequality_system(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        let cond = system.get(0).unwrap();
        check_terms(&cond.as_linear_polynomial(), &map, vec!(Some(Constant::new(-4.0)), Some(Constant::new(1.0)), Some(Constant::new(-2.0)), Some(Constant::new(0.0))));
        assert!(cond.is_strict());
        let cond = system.get(1).unwrap();
        check_terms(&cond.as_linear_polynomial(), &map, vec!(Some(Constant::new(0.0)), Some(Constant::new(0.0)), Some(Constant::new(0.0)), Some(Constant::new(0.0))));
        assert!(!cond.is_strict());
        assert!(system.get(2).is_none());
    }
}

