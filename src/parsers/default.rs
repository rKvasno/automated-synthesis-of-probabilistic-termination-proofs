use crate::pts::guard::Guards;
use crate::pts::transition::Transition;
use crate::pts::PTS;
use crate::{parsers, pts};
use parsers::grammars::default::{DefaultParser, Rule};
use parsers::{ErrorLocation, ParserError};
use pts::linear_polynomial::constant::Constant;
use pts::linear_polynomial::term::Term;
use pts::linear_polynomial::LinearPolynomial;
use pts::location::LocationHandle;
use pts::relation::{Relation, RelationSign};
use pts::system::System;
use pts::transition::Assignment;
use pts::variable_map::{Variable, VariableMap};

use pest::error::{Error as PestError, LineColLocation};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::iter::{once, zip};

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
    Power,
}

fn odds_to_probabilities(odds: Vec<Constant>) -> Vec<Constant> {
    // TODO handle division by zero
    let sum: Constant = odds.clone().into_iter().sum();
    // could probably return the iterator, but that poses some lifetime issues
    odds.into_iter().map(|x| x / sum).collect()
}

pub fn parse<'a>(input: &str) -> Result<pts::PTS, ParserError> {
    match DefaultParser::parse(Rule::program, input) {
        Err(error) => Err(handle_pest_error(error)),
        Ok(mut parse) => {
            let mut pts = Default::default();
            parse_program(&mut pts, parse.next().unwrap());
            // assert_eq!(parse.next(), None);
            Ok(pts)
        }
    }
}

fn handle_pest_error(error: PestError<Rule>) -> ParserError {
    let location: ErrorLocation = match error.line_col {
        LineColLocation::Pos(pair) => ErrorLocation::Position(pair),
        LineColLocation::Span(start, end) => ErrorLocation::Span(start, end),
    };
    ParserError {
        location,
        message: error.variant.message().to_string(),
    }
}

// assumes the parses rule is Rule::variable
fn parse_variable<'a>(parse: Pair<'a, Rule>) -> Variable {
    assert_eq!(parse.clone().as_rule(), Rule::variable);
    Variable::new(parse.as_str())
}

// assumes the parses rule is Rule::constant
fn parse_constant<'a>(parse: Pair<'a, Rule>) -> Constant {
    assert_eq!(parse.clone().as_rule(), Rule::constant);
    // all parses have to follow f64 grammar, no need to handle errors
    parse
        .as_str()
        .parse::<Constant>()
        .expect(invariant_error!())
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
    assert_eq!(parse.clone().as_rule(), Rule::constant_expr);
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
    assert_eq!(parse.clone().as_rule(), Rule::term);
    let pairs = parse.into_inner();
    let mut variable: Option<Variable> = None;
    let mut coefficient = Constant(1.0);
    for pair in pairs {
        match pair.as_rule() {
            Rule::variable => variable = Some(parse_variable(pair)),
            Rule::constant_expr => coefficient = parse_constant_expr(pair),
            _ => panic!(invariant_error!()),
        }
    }
    Term {
        variable,
        coefficient,
    }
}

// assumes the parses rule is Rule::linear_polynomial
fn parse_linear_polynomial<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> LinearPolynomial {
    assert_eq!(parse.clone().as_rule(), Rule::linear_polynomial);
    let mut op = Operation::Addition;
    let pairs = parse.into_inner();
    let mut pol = LinearPolynomial::default();
    for pair in pairs {
        match pair.as_rule() {
            Rule::additive_op => op = parse_operation(pair),
            Rule::term if op == Operation::Addition => pol.add_term(map, parse_term(pair)),
            Rule::term if op == Operation::Subtraction => pol.add_term(map, -parse_term(pair)),
            //_ => panic!(invariant_error!()),
            rule => panic!("{:?}", rule),
        }
    }
    pol
}

// assumes the parses rule is Rule::assign_inst
fn parse_assignment<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> Assignment {
    assert_eq!(parse.clone().as_rule(), Rule::assign_inst);
    let mut pairs = parse.into_inner();
    let var: Variable = parse_variable(pairs.next().unwrap());
    map.find_or_add(var.clone());
    let pol: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    Assignment(var, pol)
}

// assumes the parses rule is Rule::comparison_op
fn parse_comparison_op<'a>(parse: Pair<'a, Rule>) -> RelationSign {
    match parse.as_str() {
        ">" => RelationSign::GT,
        ">=" => RelationSign::GE,
        "<" => RelationSign::LT,
        "<=" => RelationSign::LE,
        _ => panic!(invariant_error!()),
    }
}

// assumes pairs is the iterator of a parse with rule Rule::invariants or Rule::logic_condition
fn parse_inequality<'a>(map: &mut VariableMap, pairs: &mut Pairs<'a, Rule>) -> Relation {
    // assert is checked before calling
    let lhs: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    let op = parse_comparison_op(pairs.next().unwrap());
    let rhs: LinearPolynomial = parse_linear_polynomial(map, pairs.next().unwrap());
    Relation::new(lhs, op, rhs)
}

// assumes the parses rule is Rule::invariants or Rule::logic_condition
fn parse_inequality_system<'a>(map: &mut VariableMap, parse: Pair<'a, Rule>) -> System {
    assert!(
        parse.clone().as_rule() == Rule::invariants
            || parse.clone().as_rule() == Rule::logic_condition
    );
    let mut pairs = parse.into_inner();
    let mut system = System::default();
    while pairs.peek().is_some() {
        system.push(parse_inequality(map, &mut pairs));
    }
    system
}

// assumes the parses rule is Rule::program
fn parse_program<'a>(pts: &mut PTS, parse: Pair<'a, Rule>) {
    assert_eq!(parse.clone().as_rule(), Rule::program);
    let mut transition = Transition::default();
    let mut iter = parse.into_inner();
    parse_locations(
        pts,
        iter.next().unwrap(),
        &mut transition,
        pts.locations.get_terminating_location(),
    );
    pts.locations.initial = transition.target;
    pts.locations.set_invariant(
        pts.locations.get_terminating_location(),
        parse_inequality_system(&mut pts.variables, iter.next().unwrap()),
    );
}

// assumes the parses rule is Rule::locations
fn parse_locations<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    start_transition: &mut Transition,
    end: LocationHandle,
) {
    assert_eq!(parse.clone().as_rule(), Rule::locations);
    let parse_locations = parse.into_inner();
    let mut pts_locations = pts
        .locations
        .new_n_locations(parse_locations.len())
        .peekable();

    // locations nonterminal always has atleast one location
    start_transition.target = pts_locations.peek().unwrap().clone();

    for ((local_start, local_end), pair) in zip(
        zip(
            pts_locations.clone(),
            pts_locations.skip(1).chain(once(end)),
        ),
        parse_locations,
    ) {
        let mut location_iter = pair.into_inner();
        let invariant_parse = location_iter.next().unwrap();
        pts.locations.set_invariant(
            local_start,
            parse_inequality_system(&mut pts.variables, invariant_parse),
        );

        let instruction_parse = location_iter.next().unwrap();
        match instruction_parse.as_rule() {
            Rule::if_inst => parse_if(pts, instruction_parse, local_start, local_end),
            Rule::prob_inst => parse_odds(pts, instruction_parse, local_start, local_end),
            Rule::nondet_inst => parse_nondet(pts, instruction_parse, local_start, local_end),
            Rule::while_inst => parse_while(pts, instruction_parse, local_start, local_end),
            Rule::assign_inst => parse_assign(pts, instruction_parse, local_start, local_end),
            _ => panic!(invariant_error!()),
        }
    }
}

// assumes the parses rule is Rule::assign_inst
fn parse_assign<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.clone().as_rule(), Rule::assign_inst);
    pts.locations
        .set_outgoing(
            start,
            Guards::Unguarded(Box::new(Transition {
                assignments: vec![parse_assignment(&mut pts.variables, parse)],
                target: end,
            })), // can unwrap here, since local_start can't be None and parse_assignment always returns
        )
        .unwrap()
}

// assumes the parses rule is Rule::*_condition
fn parse_condition<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, end: LocationHandle) -> Guards {
    match parse.clone().as_rule() {
        Rule::logic_condition => {
            let loop_condition = parse_inequality_system(&mut pts.variables, parse);
            Guards::Logic(vec![
                (loop_condition.clone(), Default::default()),
                (
                    !loop_condition,
                    Transition {
                        assignments: Default::default(),
                        target: end,
                    },
                ),
            ])
        }
        Rule::prob_condition => {
            let mut odds: Vec<Constant> = Default::default();
            for pair in parse.into_inner() {
                odds.push(parse_constant(pair));
            }
            // assert_eq!(odds.len(), 2);
            let probabilities = odds_to_probabilities(odds);
            Guards::Probabilistic(vec![
                (probabilities[0], Default::default()),
                (
                    probabilities[1],
                    Transition {
                        assignments: Default::default(),
                        target: end,
                    },
                ),
            ])
        }
        Rule::nondet_condition => Guards::Nondeterministic(vec![
            Default::default(),
            Transition {
                assignments: Default::default(),
                target: end,
            },
        ]),
        _ => panic!(invariant_error!()),
    }
}

// assumes the parses rule is Rule::while_inst
fn parse_while<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.clone().as_rule(), Rule::while_inst);
    // condition ~ locations
    let mut parse_iter = parse.into_inner();
    // save the condition parse for later
    let condition = parse_iter.next().unwrap();
    // parse locations
    let mut guards: Guards = parse_condition(pts, condition, end);
    match guards {
        Guards::Logic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0].1, start)
        }
        Guards::Probabilistic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0].1, start)
        }
        Guards::Nondeterministic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0], start)
        }
        _ => panic!(invariant_error!()),
    }

    // start cannot be None, see parse_locations, guards cannot be empty, see parse_condition
    pts.locations.set_outgoing(start, guards).unwrap();
}

// assumes the parses rule is Rule::nondet_inst
fn parse_nondet<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.clone().as_rule(), Rule::nondet_inst);
    let mut transitions = vec![];
    for locations_parse in parse.into_inner() {
        transitions.push(Default::default());
        parse_locations(pts, locations_parse, transitions.last_mut().unwrap(), end);
    }

    // if theres just one block, the choice is between executing and skipping it
    if transitions.len() == 1 {
        transitions.push(Transition {
            assignments: Default::default(),
            target: end,
        })
    }

    // grammar doesnt allow empty transitions, start will never be None, its ok to unwrap
    pts.locations
        .set_outgoing(start, Guards::Nondeterministic(transitions))
        .unwrap();
}

// assumes the parses rule is Rule::if_inst
fn parse_if<'a>(pts: &mut PTS, parse: Pair<'a, Rule>, start: LocationHandle, end: LocationHandle) {
    assert_eq!(parse.clone().as_rule(), Rule::if_inst);
    let mut else_condition = System::default();
    let mut conditions: Vec<System> = vec![];
    let mut transitions: Vec<Transition> = vec![];

    for pair in parse.into_inner() {
        match pair.as_rule() {
            Rule::logic_condition => {
                parse_if_condition(pts, pair, &mut conditions, &mut else_condition)
            }
            Rule::locations => {
                transitions.push(Transition::default());
                parse_locations(pts, pair, transitions.last_mut().unwrap(), end);
            }
            _ => panic!(invariant_error!()),
        }
    }

    conditions.push(else_condition);
    if conditions.len() > transitions.len() {
        transitions.push(Transition {
            assignments: Default::default(),
            target: end,
        });
    }
    // assert_eq!(conditions.len(), transitions.len())

    // start cannot ever be None, see parse_locations, guard cannot be empty due to grammar
    pts.locations
        .set_outgoing(start, Guards::Logic(zip(conditions, transitions).collect()))
        .unwrap();
}

// assumes the parses rule is Rule::logic_condition
fn parse_if_condition<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    conditions: &mut Vec<System>,
    negation_acc: &mut System,
) {
    assert_eq!(parse.clone().as_rule(), Rule::logic_condition);
    let mut new_cond = parse_inequality_system(&mut pts.variables, parse);
    conditions.push(negation_acc.clone());
    let pushed_cond = conditions.last_mut().unwrap();
    negation_acc.append(&mut !new_cond.clone());
    pushed_cond.append(&mut new_cond);
}

// assumes the parses rule is Rule::prob_inst
fn parse_odds<'a>(
    pts: &mut PTS,
    parse: Pair<'a, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.clone().as_rule(), Rule::prob_inst);
    // constant^n ~ block^(n-1)

    let mut odds: Vec<Constant> = vec![];
    let mut parse_iter = parse.into_inner();

    while parse_iter
        .peek()
        .is_some_and(|x| x.as_rule() == Rule::constant)
    {
        odds.push(parse_constant(parse_iter.next().unwrap()));
    }
    let probabilities = odds_to_probabilities(odds);
    let mut prob_iter = probabilities.into_iter();
    let mut guards: Vec<(Constant, Transition)> = vec![];
    for locations_parse in parse_iter {
        assert_eq!(locations_parse.clone().as_rule(), Rule::locations);
        // assert!(prob_iter.peek().is_some());
        guards.push((prob_iter.next().unwrap(), Default::default()));

        //TODO test this properly, cause I am not sure if theres no sneaky moves happening
        parse_locations(pts, locations_parse, &mut guards.last_mut().unwrap().1, end)
    }
    // assert!(prob_iter.peek().is_some());
    guards.push((
        prob_iter.next().unwrap(),
        Transition {
            assignments: Default::default(),
            target: end,
        },
    ));
    // assert!(prob_iter.peek().is_none());

    // start cannot be None, see parse_locations, guards cannot be empty, see grammar
    pts.locations
        .set_outgoing(start, Guards::Probabilistic(guards))
        .unwrap();
}

#[cfg(test)]
mod tests {
    use super::{
        parse, parse_constant, parse_constant_expr, parse_linear_polynomial, parse_operation,
        parse_term, parse_variable, Constant, DefaultParser, Operation, Parser, Rule, Term,
        Variable, VariableMap,
    };
    use crate::{
        misc::{read_test_string, setup_test_map},
        parsers::default::{parse_assignment, parse_inequality, parse_inequality_system},
        pts::{
            guard::Guards,
            linear_polynomial::LinearPolynomial,
            location::Locations,
            relation::{Relation, RelationType},
            system::System,
            transition::{Assignment, Transition},
            PTS,
        },
    };
    use std::iter::zip;

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

        let zipped = zip(zip(inputs, ops), rules);

        for ((input, op), rule) in zipped {
            let mut parse = DefaultParser::parse(rule, input).unwrap();
            assert_eq!(parse_operation(parse.next().unwrap()), op);
            assert!(parse.next().is_none());
        }
    }

    #[test]
    fn constant_expr_sanity() {
        let mut parse =
            DefaultParser::parse(Rule::constant_expr, "((4^2 + 5) - (2 * 2 / 2))").unwrap();
        assert_eq!(parse_constant_expr(parse.next().unwrap()), Constant(19.0));
        assert!(parse.next().is_none());
    }

    #[test]
    fn term_sanity() {
        let mut parse = DefaultParser::parse(Rule::term, "5a").unwrap();
        assert_eq!(
            parse_term(parse.next().unwrap()),
            Term {
                variable: Some(Variable::new("a")),
                coefficient: Constant(5.0)
            }
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a * 5").unwrap();
        assert_eq!(
            parse_term(parse.next().unwrap()),
            Term {
                variable: Some(Variable::new("a")),
                coefficient: Constant(5.0)
            }
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a").unwrap();
        assert_eq!(
            parse_term(parse.next().unwrap()),
            Term {
                variable: Some(Variable::new("a")),
                coefficient: Constant(1.0)
            }
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "5").unwrap();
        assert_eq!(
            parse_term(parse.next().unwrap()),
            Term {
                variable: None,
                coefficient: Constant(5.0)
            }
        );
        assert!(parse.next().is_none());
    }

    #[test]
    fn linear_polynomial_sanity() {
        let mut parse =
            DefaultParser::parse(Rule::linear_polynomial, "- a + 5 -(1/2) * b").unwrap();
        let mut map = VariableMap::default();
        let pol = parse_linear_polynomial(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(
            &pol,
            &LinearPolynomial::mock(vec!(Constant(5.0), Constant(-1.0), Constant(-0.5)))
        );
    }

    #[test]
    fn assignment_sanity() {
        let mut parse = DefaultParser::parse(Rule::assign_inst, "x = -2a + 4b - 0c - 2").unwrap();
        let mut map = VariableMap::default();
        let assign = parse_assignment(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(
            assign,
            Assignment(
                Variable::new("x"),
                LinearPolynomial::mock(vec!(
                    Constant(-2.0),
                    Constant(0.0),
                    Constant(-2.0),
                    Constant(4.0),
                    Constant(0.0)
                ))
            )
        );
    }

    #[test]
    fn inequality_sanity() {
        let mut map = VariableMap::default();
        let mut parse = DefaultParser::parse(Rule::logic_condition, "3a - 4 + b < 0").unwrap();
        let mut pairs = parse.next().unwrap().into_inner();
        assert!(parse.next().is_none());
        let cond = parse_inequality(&mut map, &mut pairs);
        assert!(pairs.next().is_none());
        assert_eq!(
            cond,
            Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec!(Constant(-4.0), Constant(3.0), Constant(1.0)))
            )
        );
    }

    #[test]
    fn inequality_system_sanity() {
        let mut map = setup_test_map();
        let mut parse = DefaultParser::parse(
            Rule::logic_condition,
            "- 2b - 4 < - a and 0 >= 0 and a >= 0",
        )
        .unwrap();
        let system = parse_inequality_system(&mut map, parse.next().unwrap());
        assert!(parse.next().is_none());
        let cond = system.get(0).unwrap();
        assert_eq!(
            *cond,
            Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec!(
                    Constant(-4.0),
                    Constant(1.0),
                    Constant(-2.0),
                    Constant(0.0)
                ))
            )
        );
        let cond = system.get(1).unwrap();
        assert_eq!(
            *cond,
            Relation::mock(
                RelationType::NonstrictInequality,
                LinearPolynomial::mock(vec!(
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0)
                ))
            )
        );
        let cond = system.get(2).unwrap();
        assert_eq!(
            *cond,
            Relation::mock(
                RelationType::NonstrictInequality,
                LinearPolynomial::mock(vec!(
                    Constant(0.0),
                    Constant(-1.0),
                    Constant(0.0),
                    Constant(0.0)
                ))
            )
        );
        assert!(system.get(3).is_none());
    }

    #[test]
    fn parse_program_trivial() {
        let input = read_test_string("code/default/trivial_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let handle = locations.new_location();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(
            handle,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(-1.0)]),
            )]),
        );
        // 2
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();
        // 3
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_program_simple() {
        let input = read_test_string("code/default/simple_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let handle = locations_iter.next().unwrap();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(
            handle,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0)]),
            )]),
        );
        // 2
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0), Constant(0.0)]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();
        // 3
        let handle = next_location;
        locations.set_invariant(
            handle,
            System::mock(vec![
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0), Constant(-1.0)]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0), Constant(0.0)]),
                ),
            ]),
        );

        // 4
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("c"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(1.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();

        // 5
        let handle = next_location;
        locations.set_invariant(
            handle,
            System::mock(vec![
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(0.0),
                        Constant(-1.0),
                    ]),
                ),
            ]),
        );
        // 6
        let next_location = locations.get_terminating_location();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(2.0),
                            Constant(0.0),
                            Constant(1.0),
                        ]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();
        // 7
        let handle = next_location;
        locations.set_invariant(
            handle,
            System::mock(vec![
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(0.0),
                        Constant(-1.0),
                    ]),
                ),
            ]),
        );

        let variables = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_simple_if_program() {
        let input = read_test_string("code/default/simple_if_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        let br_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    (
                        // 2
                        System::mock(vec![Relation::mock(
                            RelationType::NonstrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(-1.0),
                                Constant(1.0),
                            ]),
                        )]),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 6
                        System::mock(vec![
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            ),
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            ),
                        ]),
                        Transition {
                            assignments: vec![],
                            target: br_2,
                        },
                    ),
                    (
                        // 10
                        System::mock(vec![
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            ),
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(0.0),
                                    Constant(-1.0),
                                    Constant(1.0),
                                ]),
                            ),
                        ]),
                        Transition {
                            assignments: vec![],
                            target: br_3,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )]),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 11
        locations.set_invariant(
            br_3,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )]),
        );

        // 12
        locations
            .set_outgoing(
                br_3,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 14
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )]),
        );

        // 15
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )]),
        );

        let variables = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_trivial_if_program() {
        let input = read_test_string("code/default/trivial_if_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    (
                        // 2
                        System::mock(vec![Relation::mock(
                            RelationType::NonstrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(-1.0),
                                Constant(1.0),
                            ]),
                        )]),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 5
                        System::mock(vec![Relation::mock(
                            RelationType::StrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(1.0),
                                Constant(-1.0),
                            ]),
                        )]),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_simple_odds_program() {
        let input = read_test_string("code/default/simple_odds_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(4);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    (
                        // 2
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 6
                        Constant(1.0),
                        Transition {
                            assignments: vec![],
                            target: br_2,
                        },
                    ),
                    (
                        // 9
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            System::mock(vec![Relation::mock(
                RelationType::NonstrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(-1.0)]),
            )]),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 10
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 11
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 12
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_trivial_odds_program() {
        let input = read_test_string("code/default/trivial_odds_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    (
                        // 2
                        Constant(0.5),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 9
                        Constant(0.5),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_simple_nondet_program() {
        let input = read_test_string("code/default/simple_nondet_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        let br_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //6
                    Transition {
                        assignments: vec![],
                        target: br_2,
                    },
                    //10
                    Transition {
                        assignments: vec![],
                        target: br_3,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 11
        locations.set_invariant(
            br_3,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 12
        locations
            .set_outgoing(
                br_3,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 14
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 15
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_trivial_nondet_program() {
        let input = read_test_string("code/default/trivial_nondet_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //5
                    Transition {
                        assignments: vec![],
                        target: junction,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_logic_while_program() {
        let input = read_test_string("code/default/while_logic_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    // 2
                    (
                        System::mock(vec![
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                        ]),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    //5
                    (
                        System::mock(vec![
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                        ]),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_prob_while_program() {
        let input = read_test_string("code/default/while_prob_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    // 2
                    (
                        Constant(1.0),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    //5
                    (
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_nondet_while_program() {
        let input = read_test_string("code/default/while_nondet_program");
        let parsed = parse(input.as_str()).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )]),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //5
                    Transition {
                        assignments: vec![],
                        target: junction,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            System::mock(vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )]),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }
}
