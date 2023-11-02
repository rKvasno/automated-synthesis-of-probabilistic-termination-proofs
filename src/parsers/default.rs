use std::str::FromStr;

use pest::iterators::Pair;
use pest::Parser;

// TODO use LinearPolynomial parser from DefaultParser
use crate::consume;
use crate::pts::guard::Guards;
use crate::pts::invariant::Invariant;
use crate::pts::linear_polynomial::coefficient::{Coefficient, Constant};
use crate::pts::linear_polynomial::State;
use crate::pts::location::LocationHandle;
use crate::pts::relation::{RelationSign, StateRelation};
use crate::pts::system::StateSystem;
use crate::pts::transition::{Assignment, StateAssignment, Transition};
use crate::pts::variable::program_variable::{ProgramVariable, ProgramVariables};
use crate::pts::variable::Variable;
use crate::pts::PTS;

use super::grammars::default::{DefaultParser, Rule};
use super::{handle_pest_error, ParserError};

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
    // checks for negative probabilities
    assert!(odds.iter().find(|x| x.is_negative()).is_none());
    let sum: Constant = odds.to_owned().into_iter().sum();
    // TODO since the user can input all 0.0, we need a ParserError
    // could probably return the iterator, but that poses some lifetime issues
    odds.into_iter().map(|x| x / sum).collect()
}

// TODO error propagation for division by zero
pub fn parse(input: &str) -> Result<PTS, ParserError> {
    match DefaultParser::parse(Rule::program, input) {
        Err(error) => Err(handle_pest_error(error)),
        Ok(mut parse) => {
            let mut pts = Default::default();
            parse_program(&mut pts, parse.next().unwrap());
            assert_eq!(parse.next(), None);
            pts.finalize();
            Ok(pts)
        }
    }
}

// assumes the parses rule is Rule::variable
fn parse_variable<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> ProgramVariable {
    assert_eq!(parse.to_owned().as_rule(), Rule::variable);
    ProgramVariable::new(variables, &parse.as_str())
}

// assumes the parses rule is Rule::constant
fn parse_constant<'parse>(parse: Pair<'parse, Rule>) -> Constant {
    assert_eq!(parse.to_owned().as_rule(), Rule::constant);
    // all parses have to follow f64 grammar, no need to handle errors
    parse
        .as_str()
        .parse::<Constant>()
        .expect(invariant_error!())
}

// assumes the parses rule is Rule::power_op, Rule::multiplicative_op or Rule::additive_op
fn parse_operation<'parse>(parse: Pair<'parse, Rule>) -> Operation {
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
fn parse_constant_expr<'parse>(parse: Pair<'parse, Rule>) -> Constant {
    assert_eq!(parse.to_owned().as_rule(), Rule::constant_expr);
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
            Rule::variable => variable = Some(parse_variable(variables, pair)),
            Rule::constant_expr => coefficient = parse_constant_expr(pair),
            _ => panic!(invariant_error!()),
        }
    }
    (coefficient, variable)
}

// assumes the parses rule is Rule::linear_polynomial
fn parse_linear_polynomial<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> State {
    assert_eq!(parse.to_owned().as_rule(), Rule::linear_polynomial);
    let mut op = Operation::Addition;
    let pairs = parse.into_inner();
    // theres no guarantee that there wont be duplicit terms, but its probably better to use
    // with_capacity than shrink_to_fit afterwards
    let mut pol = State::with_capacity(pairs.len());
    for pair in pairs {
        match pair.as_rule() {
            Rule::additive_op => op = parse_operation(pair),
            Rule::term if op == Operation::Addition => {
                let (coeff, var) = parse_term(variables, pair);

                consume!(pol.add_term(coeff, var))
            }
            Rule::term if op == Operation::Subtraction => {
                let (coeff, var) = parse_term(variables, pair);
                consume!(pol.add_term(-coeff, var))
            }
            //_ => panic!(invariant_error!()),
            rule => panic!("{:?}", rule),
        }
    }
    pol
}

// assumes the parses rule is Rule::assignment_statement
fn parse_assignment<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> StateAssignment {
    assert_eq!(parse.to_owned().as_rule(), Rule::assignment_statement);
    let mut pairs = parse.into_inner();
    let var = parse_variable(variables, pairs.next().unwrap());
    let pol: State = parse_linear_polynomial(variables, pairs.next().unwrap());
    Assignment::new(var, pol)
}

// assumes the parses rule is Rule::comparison_op
fn parse_comparison_op<'parse>(parse: Pair<'parse, Rule>) -> RelationSign {
    RelationSign::from_str(parse.as_str()).unwrap()
}

// assumes the parses rule is Rule::inequality
fn parse_inequality<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> StateRelation {
    assert!(parse.to_owned().as_rule() == Rule::inequality);
    let mut pairs = parse.into_inner();
    let lhs: State = parse_linear_polynomial(variables, pairs.next().unwrap());
    let op = parse_comparison_op(pairs.next().unwrap());
    let rhs: State = parse_linear_polynomial(variables, pairs.next().unwrap());
    // assert!(pairs.next().is_none());
    StateRelation::new(lhs, op, rhs)
}

// assumes the parses rule is Rule::logic_condition
fn parse_inequality_system<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> StateSystem {
    assert!(parse.to_owned().as_rule() == Rule::logic_condition);
    let mut system = StateSystem::default();
    for pair in parse.into_inner() {
        system.push(parse_inequality(variables, pair));
    }
    system
}

// assumes the parses rule is Rule::invariant
fn parse_invariant<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> Invariant {
    assert!(parse.to_owned().as_rule() == Rule::invariant);
    let iter = parse.into_inner();
    let mut assertions = Vec::with_capacity(iter.len());
    for pair in iter {
        assertions.push(parse_inequality_system(variables, pair));
    }
    Invariant::from(assertions)
}

// assumes the parses rule is Rule::program
fn parse_program<'parse>(pts: &mut PTS, parse: Pair<'parse, Rule>) {
    assert_eq!(parse.to_owned().as_rule(), Rule::program);
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
        parse_invariant(&mut pts.variables, iter.next().unwrap()),
    );
}

// assumes the parses rule is Rule::locations
fn parse_locations<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start_transition: &mut Transition,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::locations);
    let parse_locations = parse.into_inner();
    let mut pts_locations = pts
        .locations
        .new_n_locations(parse_locations.len())
        .peekable();

    // locations nonterminal always has atleast one location
    start_transition.target = pts_locations.peek().unwrap().to_owned();

    for ((local_start, local_end), pair) in std::iter::zip(
        std::iter::zip(
            pts_locations.to_owned(),
            pts_locations.skip(1).chain(std::iter::once(end)),
        ),
        parse_locations,
    ) {
        let mut location_iter = pair.into_inner();
        let invariant_parse = location_iter.next().unwrap();
        pts.locations.set_invariant(
            local_start,
            parse_invariant(&mut pts.variables, invariant_parse),
        );

        let instruction_parse = location_iter.next().unwrap();
        match instruction_parse.as_rule() {
            Rule::if_statement => parse_if(pts, instruction_parse, local_start, local_end),
            Rule::odds_statement => parse_odds(pts, instruction_parse, local_start, local_end),
            Rule::choose_statement => parse_nondet(pts, instruction_parse, local_start, local_end),
            Rule::while_statement => parse_while(pts, instruction_parse, local_start, local_end),
            Rule::assignment_statement => {
                parse_assign(pts, instruction_parse, local_start, local_end)
            }
            _ => panic!(invariant_error!()),
        }
    }
}

// assumes the parses rule is Rule::assignment_statement
fn parse_assign<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::assignment_statement);
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

// assumes the parses rule is Rule::*_condition or Rule::nondeterminism_sign
fn parse_condition<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    end: LocationHandle,
) -> Guards {
    match parse.to_owned().as_rule() {
        Rule::logic_condition => {
            let loop_condition = parse_inequality_system(&mut pts.variables, parse);
            Guards::Logic(vec![
                (loop_condition.to_owned(), Default::default()),
                (
                    !loop_condition,
                    Transition {
                        assignments: Default::default(),
                        target: end,
                    },
                ),
            ])
        }
        Rule::odds_condition => {
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
        Rule::nondeterminism_sign => Guards::Nondeterministic(vec![
            Default::default(),
            Transition {
                assignments: Default::default(),
                target: end,
            },
        ]),
        _ => panic!(invariant_error!()),
    }
}

// assumes the parses rule is Rule::while_statement
fn parse_while<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::while_statement);
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

// assumes the parses rule is Rule::choose_statement
fn parse_nondet<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::choose_statement);
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

// assumes the parses rule is Rule::if_statement
fn parse_if<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::if_statement);
    let mut else_condition = StateSystem::default();
    let mut conditions: Vec<StateSystem> = vec![];
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
        .set_outgoing(
            start,
            Guards::Logic(std::iter::zip(conditions, transitions).collect()),
        )
        .unwrap();
}

// assumes the parses rule is Rule::logic_condition
fn parse_if_condition<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    conditions: &mut Vec<StateSystem>,
    negation_acc: &mut StateSystem,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::logic_condition);
    let mut new_cond = parse_inequality_system(&mut pts.variables, parse);
    conditions.push(negation_acc.to_owned());
    let pushed_cond = conditions.last_mut().unwrap();
    negation_acc.append(&mut !new_cond.to_owned());
    pushed_cond.append(&mut new_cond);
}

// assumes the parses rule is Rule::odds_statement
fn parse_odds<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::odds_statement);
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
        assert_eq!(locations_parse.to_owned().as_rule(), Rule::locations);
        // assert!(prob_iter.peek().is_some());
        guards.push((prob_iter.next().unwrap(), Default::default()));

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
    use pest::Parser;

    use crate::{
        assignment, guards, invariant,
        misc::tests::parsers::default::{
            INVARIANT_PROGRAM, SIMPLE_CHOOSE_PROGRAM, SIMPLE_IF_PROGRAM, SIMPLE_ODDS_PROGRAM,
            SIMPLE_PROGRAM, TRIVIAL_CHOOSE_PROGRAM, TRIVIAL_IF_PROGRAM, TRIVIAL_ODDS_PROGRAM,
            TRIVIAL_PROGRAM, WHILE_LOGIC_PROGRAM, WHILE_NONDETERMINISTIC_PROGRAM,
            WHILE_PROB_PROGRAM,
        },
        parsers::{
            default::{
                parse, parse_assignment, parse_constant, parse_constant_expr, parse_inequality,
                parse_inequality_system, parse_linear_polynomial, parse_operation, parse_term,
                parse_variable,
            },
            grammars::default::{DefaultParser, Rule},
        },
        pts::{
            linear_polynomial::coefficient::Constant,
            location::Locations,
            variable::{program_variable::ProgramVariable, Variable},
            PTS,
        },
        relation, state, state_system, transition, variables,
    };

    use super::Operation;
    use std::iter::zip;

    #[test]
    fn variable_sanity() {
        let mut variables = variables!();
        let variable = ProgramVariable::new(&mut variables, "abc");
        let string = variable.to_string();
        let mut parse = DefaultParser::parse(Rule::variable, string.as_str()).unwrap();
        let var = parse_variable(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(var.to_string(), variable.to_string());
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
        let mut variables = variables!();
        let mut parse = DefaultParser::parse(Rule::term, "5a").unwrap();
        assert_eq!(
            parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a * 5").unwrap();
        assert_eq!(
            parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "a").unwrap();
        assert_eq!(
            parse_term(&mut variables, parse.next().unwrap()),
            (Constant(1.0), variables.get("a").cloned())
        );
        assert!(parse.next().is_none());
        parse = DefaultParser::parse(Rule::term, "5").unwrap();
        assert_eq!(
            parse_term(&mut variables, parse.next().unwrap()),
            (Constant(5.0), None)
        );
        assert!(parse.next().is_none());
    }

    #[test]
    fn linear_polynomial_sanity() {
        let mut parse =
            DefaultParser::parse(Rule::linear_polynomial, "- a + 5 -(1/2) * b").unwrap();
        let mut variables = variables!();
        let pol = parse_linear_polynomial(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(&pol, &state!(5.0, &mut variables, -1.0, "a", -0.5, "b"));
    }

    #[test]
    fn assignment_sanity() {
        let mut variables = variables!("x", "a", "b", "c");
        let mut parse =
            DefaultParser::parse(Rule::assignment_statement, "x = -2a + 4b - 0c - 2").unwrap();
        let assign = parse_assignment(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(
            assign,
            assignment!(
                &mut variables,
                "x",
                -2.0,
                0.0,
                "x",
                -2.0,
                "a",
                4.0,
                "b",
                0.0,
                "c"
            )
        );
    }

    #[test]
    fn inequality_sanity() {
        let mut variables = variables!();
        let mut parse = DefaultParser::parse(Rule::inequality, "3a - 4 + b < 0").unwrap();
        let pair = parse.next().unwrap();
        assert!(parse.next().is_none());
        let cond = parse_inequality(&mut variables, pair);
        assert_eq!(
            cond,
            relation!["<", -4.0, &mut variables, 3.0, "a", 1.0, "b"]
        );
    }

    #[test]
    fn inequality_system_sanity() {
        let mut variables = variables!("a", "b", "c");
        let mut parse = DefaultParser::parse(
            Rule::logic_condition,
            "- 2b - 4 < - a and 0 >= 0 and a >= 0",
        )
        .unwrap();
        let system = parse_inequality_system(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        let cond = system.get(0).unwrap();
        assert_eq!(
            *cond,
            relation!("<", -4.0, &mut variables, 1.0, "a", -2.0, "b", 0.0, "c")
        );
        let cond = system.get(1).unwrap();
        assert_eq!(
            *cond,
            relation!(">=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c")
        );
        let cond = system.get(2).unwrap();
        assert_eq!(
            *cond,
            relation!(">=", 0.0, &mut variables, 1.0, "a", 0.0, "b", 0.0, "c")
        );
        assert!(system.get(3).is_none());
    }

    #[test]
    fn invariant_sanity() {
        let input = INVARIANT_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let handle = locations.new_location();
        let mut variables = variables!("a", "b", "c");

        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(
            handle,
            invariant!( &mut variables,
                ["<=", 0.0, 0.0, "a"; "<", 0.0, -1.0, "a", 1.0, "b"],
                [">", 0.0, -1.0, "a", 1.0, "b"; ">", 0.0, 0.0, "a", 0.0, "b"],
                ["<", 0.0, 1.0, "a", -1.0, "b"; ">=", 0.0, 0.0, "a", 0.0, "b"]
            ),
        );
        // 2
        locations
            .set_outgoing(
                handle,
                guards!(transition![None, &mut variables; "a", 0.0, 1.0, "a", 0.0,"b"]),
            )
            .unwrap();
        // 3
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!( &mut variables,
                ["<=", 0.0, 0.0, "a", 0.0, "b"; "<", 0.0, -1.0, "a", 1.0, "b"],
                [">", 0.0, -1.0, "a", 1.0, "b"; ">", 0.0, 0.0, "a", 0.0, "b"],
                ["<", 0.0, 1.0, "a", -1.0, "b"; ">=", 0.0, 0.0, "a", 0.0, "b"]
            ),
        );

        let variables = variables!("a", "b");

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
        let input = SIMPLE_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!();

        let handle = locations_iter.next().unwrap();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(handle, invariant!(&mut variables, [">", 0.0, 1.0, "a"]));
        // 2
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                guards!(transition!(next_location, &mut variables; "b", 1.0, 0.0, "a", 0.0, "b")),
            )
            .unwrap();
        // 3
        let handle = next_location;
        locations.set_invariant(
            handle,
            invariant!( &mut variables,
            [
                ">=", -1.0, 0.0, "a", 1.0, "b";
                ">", 0.0, 1.0, "a", 0.0, "b"
            ]),
        );

        // 4
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                guards!(transition!(next_location, &mut variables; "c", 0.0, 1.0, "a", 1.0, "b", 0.0, "c")),
            )
            .unwrap();

        // 5
        let handle = next_location;
        locations.set_invariant(
            handle,
            invariant!(&mut variables,
            [
                ">=", -1.0, 0.0, "a", 1.0, "b", 0.0, "c";
                ">", 0.0, 1.0, "a", 0.0, "b", 0.0, "c";
                ">", -1.0, 0.0, "a", 0.0, "b", 1.0, "c"
            ]),
        );
        // 6
        let next_location = locations.get_terminating_location();
        locations
            .set_outgoing(
                handle,
                guards!(transition!(next_location, &mut variables; "b", 0.0, 2.0, "a", 0.0, "b", 1.0, "c")),
            )
            .unwrap();
        // 7
        let handle = next_location;
        locations.set_invariant(
            handle,
            invariant!(&mut variables,
            [
                ">", -1.0, 0.0, "a", 1.0, "b", 0.0, "c";
                ">", 0.0, 1.0, "a", 0.0, "b", 0.0, "c";
                ">", -1.0, 0.0, "a", 0.0, "b", 1.0, "c"
            ]),
        );

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_program_trivial() {
        let input = TRIVIAL_PROGRAM;
        let parsed = parse(input).unwrap();
        let mut variables = variables!("a", "b", "c",);

        let mut locations = Locations::default();
        let handle = locations.new_location();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(handle, invariant!(&mut variables, [">", 1.0]));
        // 2
        locations
            .set_outgoing(
                handle,
                guards!(transition![None,&mut variables; "a", 1.0, 0.0, "a"]),
            )
            .unwrap();
        // 3
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 1.0, 0.0, "a"]),
        );

        let variables = variables!("a");

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
        let input = SIMPLE_IF_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);
        let mut variables = variables!();

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        let branch_2 = locations_iter.next().unwrap();
        let branch_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(L:
                    // 2
                    state_system!(&mut variables
                                 ;">=", 0.0, 1.0, "a", -1.0, "b"),
                    transition!(branch_1),
                    // 6
                    state_system!(&mut variables;
                        "<", 0.0, 1.0, "a", -1.0, "b";
                        "<=", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"
                    ),
                    transition!(branch_2),
                    // 10
                    state_system!(&mut variables;
                        "<", 0.0, 1.0, "a", -1.0, "b";
                        ">", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"
                    ),
                    transition!(branch_3),
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            branch_1,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            branch_2,
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
        );

        // 8
        locations
            .set_outgoing(
                branch_2,
                guards!(
                    transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                ),
            )
            .unwrap();

        // 11
        locations.set_invariant(
            branch_3,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
        );

        // 12
        locations
            .set_outgoing(
                branch_3,
                guards!(
                    transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                ),
            )
            .unwrap();

        // 14
        locations.set_invariant(
            junction,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
        );

        // 15
        locations
            .set_outgoing(
                junction,
                guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
        );

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
        let input = TRIVIAL_IF_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a", "b");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(L:
                    // 2
                    state_system!(&mut variables; ">=", 0.0, 1.0, "a", -1.0, "b"),
                    transition!(branch_1),

                    // 5
                    state_system!(&mut variables; "<", 0.0, 1.0, "a", -1.0, "b")
                    ,
                    transition!(junction)
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            branch_1,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(), &mut variables;
                    "a", 0.0, 1.0, "a", 0.0, "b")),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

        let variables = variables!("a", "b");

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
        let input = SIMPLE_ODDS_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(4);
        let mut variables = variables!();

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        let branch_2 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(P:
                    // 2
                    0.0,
                    transition!(branch_1),
                    // 5
                    1.0,
                    transition!(branch_2),
                    // 9
                    0.0,
                    transition!(junction),
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            branch_2,
            invariant!(&mut variables, [">=", 0.0, -1.0, "a", 1.0, "b"]),
        );

        // 8
        locations
            .set_outgoing(
                branch_2,
                guards!(transition!(junction, &mut variables; "b", 0.0, 1.0, "a", 0.0, "b")),
            )
            .unwrap();

        // 10
        locations.set_invariant(
            junction,
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 11
        locations
            .set_outgoing(
                junction,
                guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")),
            )
            .unwrap();

        // 12
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

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
        let input = TRIVIAL_ODDS_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(P:
                    // 2
                    0.5,
                    transition!(branch_1),
                    // 9
                    0.5,
                    transition!(junction)
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
            )
            .unwrap();

        // 6
        locations.set_invariant(junction, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(), &mut variables;
                    "a",
                    0.0,
                    1.0, "a"
                )),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
        );

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_simple_choose_program() {
        let input = SIMPLE_CHOOSE_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);
        let mut variables = variables!("a");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        let branch_2 = locations_iter.next().unwrap();
        let branch_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(
                    // 2
                    transition!(branch_1),
                    // 6
                    transition!(branch_2),
                    // 10
                    transition!(branch_3)
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction,&mut variables ; "a", 0.0, 1.0, "a")),
            )
            .unwrap();

        // 7
        locations.set_invariant(branch_2, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 8
        locations
            .set_outgoing(
                branch_2,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
            )
            .unwrap();

        // 11
        locations.set_invariant(branch_3, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 12
        locations
            .set_outgoing(
                branch_3,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
            )
            .unwrap();

        // 14
        locations.set_invariant(junction, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]));

        // 15
        locations
            .set_outgoing(
                junction,
                guards!(
                    transition!(locations.get_terminating_location(), &mut variables;
                    "a", 0.0, 1.0, "a")
                ),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
        );

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_trivial_choose_program() {
        let input = TRIVIAL_CHOOSE_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));

        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(
                    // 2
                    transition!(branch_1),
                    // 5
                    transition!(junction),
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
            )
            .unwrap();
        // 6
        locations.set_invariant(junction, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(locations.get_terminating_location(),
                    &mut variables;
                    "a", 0.0, 1.0, "a"
                )),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
        );

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
        let input = WHILE_LOGIC_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a", "b");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));
        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(L:
                    // 2
                    state_system!(&mut variables;
                        ">", 0.0, 0.0, "a";
                        "<", 0.0, 0.0, "a"
                    ),
                    transition!(branch_1),
                    // 5
                    state_system!(&mut variables;
                        "<=", 0.0, 0.0, "a";
                        ">=", 0.0, 0.0, "a"
                    ),
                    transition!(junction),
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(
                    transition!(locations.get_terminating_location(), &mut variables;
                            "a", 0.0, 0.0, "a", 1.0, "b"
                    )
                ),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

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
        let input = WHILE_PROB_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a", "b");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));

        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(P:
                    // 2
                    1.0,
                    transition!(branch_1),
                    // 5
                    0.0,
                    transition!(junction),
                ),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(), &mut variables;
                    "a", 0.0, 0.0, "a", 1.0, "b"
                )),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    #[test]
    fn parse_nondeterministic_while_program() {
        let input = WHILE_NONDETERMINISTIC_PROGRAM;
        let parsed = parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables = variables!("a", "b");

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(start, invariant!(&mut variables, ["<", 0.0]));

        let junction = locations_iter.next().unwrap();
        let branch_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                guards!(transition!(branch_1), transition!(junction),),
            )
            .unwrap();

        // 3
        locations.set_invariant(branch_1, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 4
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            invariant!(&mut variables, [">", 0.0, 0.0, "a", 0.0, "b"]),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(), &mut variables;
                    "a", 0.0, 0.0, "a", 1.0, "b"
                )),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

        assert_eq!(
            parsed,
            PTS {
                locations,
                variables
            }
        );
    }

    mod error_message {
        use crate::parsers::default::parse;

        #[test]
        fn empty() {
            assert_eq!(
                parse("").err().unwrap().to_string(),
                " --> 1:1\n  |\n1 | \n  | ^---\n  |\n  = expected invariant"
            );
        }

        #[test]
        fn deeper_rules() {
            assert_eq!(parse("# 0>0\n  while ").err().unwrap().to_string(), " --> 2:9\n  |\n2 |   while \n  |         ^---\n  |\n  = expected nondeterminism_sign, linear_polynomial, or constant");
        }

        #[test]
        fn choice() {
            assert_eq!(parse("# 0>0\n  choose ").err().unwrap().to_string(), " --> 2:3\n  |\n2 |   choose \n  |   ^---\n  |\n  = expected if_statement, odds_statement, choose_statement, while_statement, or assignment_statement");
        }
    }
}
