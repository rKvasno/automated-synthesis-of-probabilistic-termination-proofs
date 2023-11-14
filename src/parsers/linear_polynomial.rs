use crate::parsers::INVARIANT_ERROR;
use crate::pts::linear_polynomial::coefficient::Constant;
use crate::pts::variable::program_variable::ProgramVariable;
use crate::pts::{linear_polynomial::State, variable::program_variable::ProgramVariables};
use crate::{consume, program_var};

use super::default::Operation;
use super::grammars::linear_polynomial::{PolynomialPestParser, Rule};
use pest::iterators::Pair;
use pest::Parser as PestParser;

pub struct LinearPolynomialParser;
impl LinearPolynomialParser {
    // assumes the input can be parsed as a singular Rule::linear_polynomial nonterminal
    pub fn parse_polynomial<'parse>(variables: &mut ProgramVariables, input: &str) -> State {
        let mut parse_iter =
            PolynomialPestParser::parse(Rule::linear_polynomial, input).expect(INVARIANT_ERROR);
        let parse = parse_iter.next().expect(INVARIANT_ERROR);
        assert!(parse_iter.next().is_none());
        let mut op = Operation::Addition;
        let pairs = parse.into_inner();
        // theres no guarantee that there wont be duplicit terms, but its probably better to use
        // with_capacity than shrink_to_fit afterwards
        let mut pol = State::with_capacity(pairs.len());
        for pair in pairs {
            match pair.as_rule() {
                Rule::additive_op => op = LinearPolynomialParser::parse_operation(pair),
                Rule::term if op == Operation::Addition => {
                    let (coeff, var) = LinearPolynomialParser::parse_term(variables, pair);

                    consume!(pol.add_term(coeff, var))
                }
                Rule::term if op == Operation::Subtraction => {
                    let (coeff, var) = LinearPolynomialParser::parse_term(variables, pair);
                    consume!(pol.add_term(-coeff, var))
                }
                //_ => panic!(INVARIANT_ERROR),
                rule => panic!("{:?}", rule),
            }
        }
        pol
    }

    // assumes the input can be parsed as a singular Rule::variable nonterminal
    pub fn parse_variable<'parse>(
        variables: &mut ProgramVariables,
        input: &str,
    ) -> ProgramVariable {
        program_var!(variables, input)
    }

    // assumes the input can be parsed as a singular Rule::constant nonterminal
    // assumes the grammar enforces a subset of f64 grammar
    pub fn parse_constant<'parse>(input: &str) -> Constant {
        input.parse::<Constant>().expect(INVARIANT_ERROR)
    }

    // assumes the parses rule is Rule::power_op, Rule::multiplicative_op or Rule::additive_op
    fn parse_operation<'parse>(parse: Pair<'parse, Rule>) -> Operation {
        match parse.as_str() {
            "+" => Operation::Addition,
            "-" => Operation::Subtraction,
            "*" => Operation::Multiplication,
            "/" => Operation::Division,
            "^" => Operation::Power,
            _ => panic!("{}", INVARIANT_ERROR),
        }
    }

    // assumes the parses rule is Rule::constant_expr
    fn parse_constant_expr<'parse>(parse: Pair<'parse, Rule>) -> Constant {
        assert_eq!(parse.to_owned().as_rule(), Rule::constant_expr);
        let mut iter = parse.into_inner().into_iter();
        let first = iter.next().unwrap();
        let mut acc = match first.as_rule() {
            // first.as_str() upholds invariants for parse_constant
            Rule::constant => LinearPolynomialParser::parse_constant(first.as_str()),
            Rule::constant_expr => LinearPolynomialParser::parse_constant_expr(first),
            _ => panic!("{}", INVARIANT_ERROR),
        };

        loop {
            let op = iter.next();
            if op.is_none() {
                return acc;
            }
            match LinearPolynomialParser::parse_operation(op.unwrap()) {
                Operation::Addition => {
                    acc += LinearPolynomialParser::parse_constant_expr(iter.next().unwrap())
                }
                Operation::Subtraction => {
                    acc -= LinearPolynomialParser::parse_constant_expr(iter.next().unwrap())
                }
                Operation::Multiplication => {
                    acc *= LinearPolynomialParser::parse_constant_expr(iter.next().unwrap())
                }
                Operation::Division => {
                    acc /= LinearPolynomialParser::parse_constant_expr(iter.next().unwrap())
                }
                Operation::Power => {
                    acc = acc.pow(LinearPolynomialParser::parse_constant_expr(
                        iter.next().unwrap(),
                    ))
                }
            }
        }
        // unreachable!();
    }

    // assumes the parses rule is Rule::term
    fn parse_term<'parse>(
        variables: &mut ProgramVariables,
        parse: Pair<'parse, Rule>,
    ) -> (Constant, Option<ProgramVariable>) {
        assert_eq!(parse.to_owned().as_rule(), Rule::term);
        let pairs = parse.into_inner();
        let mut variable: Option<ProgramVariable> = None;
        let mut coefficient = Constant(1.0);
        for pair in pairs {
            match pair.as_rule() {
                // pair.as_str() upholds invariants for parse_variable
                Rule::variable => {
                    variable = Some(LinearPolynomialParser::parse_variable(
                        variables,
                        pair.as_str(),
                    ))
                }
                Rule::constant_expr => {
                    coefficient = LinearPolynomialParser::parse_constant_expr(pair)
                }
                _ => panic!("{}", INVARIANT_ERROR),
            }
        }
        (coefficient, variable)
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser as PestParser;

    use crate::{
        parsers::{
            default::Operation,
            grammars::linear_polynomial::{PolynomialPestParser, Rule},
            linear_polynomial::LinearPolynomialParser,
        },
        program_var,
        pts::{
            linear_polynomial::coefficient::Constant, variable::program_variable::ProgramVariables,
        },
        state, variables,
    };

    #[test]
    fn variable_sanity() {
        let mut variables: ProgramVariables = variables!();
        let variable = program_var!(&mut variables, "abc");
        let input = variable.to_string();
        let var = LinearPolynomialParser::parse_variable(&mut variables, input.as_str());
        assert_eq!(var.to_string(), variable.to_string());
    }

    #[test]
    fn constant_sanity() {
        let constant = LinearPolynomialParser::parse_constant("123");
        assert_eq!(constant, Constant(123.0));
    }

    #[test]
    fn operation_sanity() {
        let inputs = vec!["+", "-", "*", "/", "^"];
        let ops = vec![
            Operation::Addition,
            Operation::Subtraction,
            Operation::Multiplication,
            Operation::Division,
            Operation::Power,
        ];
        let rules = vec![
            Rule::additive_op,
            Rule::additive_op,
            Rule::multiplicative_op,
            Rule::multiplicative_op,
            Rule::power_op,
        ];

        let zipped = std::iter::zip(std::iter::zip(inputs, ops), rules);

        for ((input, op), rule) in zipped {
            let mut parse = PolynomialPestParser::parse(rule, input).unwrap();
            assert_eq!(
                LinearPolynomialParser::parse_operation(parse.next().unwrap()),
                op
            );
            assert!(parse.next().is_none());
        }
    }

    #[test]
    fn constant_expr_sanity() {
        let mut parse =
            PolynomialPestParser::parse(Rule::constant_expr, "((4^2 + 5) - (2 * 2 / 2))").unwrap();
        assert_eq!(
            LinearPolynomialParser::parse_constant_expr(parse.next().unwrap()),
            Constant(19.0)
        );
        assert!(parse.next().is_none());
    }

    #[test]
    fn term_sanity() {
        let mut variables: ProgramVariables = variables!();
        let mut parse = PolynomialPestParser::parse(Rule::term, "5a").unwrap();
        assert_eq!(
            LinearPolynomialParser::parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = PolynomialPestParser::parse(Rule::term, "a * 5").unwrap();
        assert_eq!(
            LinearPolynomialParser::parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = PolynomialPestParser::parse(Rule::term, "a").unwrap();
        assert_eq!(
            LinearPolynomialParser::parse_term(&mut variables, parse.next().unwrap()),
            (Constant(1.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = PolynomialPestParser::parse(Rule::term, "5").unwrap();
        assert_eq!(
            LinearPolynomialParser::parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), None)
        );
        assert!(parse.next().is_none());
    }

    #[test]
    fn linear_polynomial_sanity() {
        let mut parse =
            PolynomialPestParser::parse(Rule::linear_polynomial, "- a + 5 -(1/2) * b").unwrap();
        let mut variables: ProgramVariables = variables!();
        let pol = LinearPolynomialParser::parse_polynomial(
            &mut variables,
            parse.next().unwrap().as_str(),
        );
        assert!(parse.next().is_none());
        assert_eq!(pol, state!(5.0, &mut variables, -1.0, "a", -0.5, "b"));
    }
}
