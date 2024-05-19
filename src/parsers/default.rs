use std::str::FromStr;

use pest::iterators::Pair;
use pest::Parser as PestParser;

use crate::invariant;
use crate::parsers::linear_polynomial::LinearPolynomialParser;
use crate::parsers::INVARIANT_ERROR;
use crate::pts::guard::Guards;
use crate::pts::invariant::Invariant;
use crate::pts::linear_polynomial::coefficient::{Coefficient, Constant};
use crate::pts::linear_polynomial::State;
use crate::pts::location::LocationHandle;
use crate::pts::relation::{RelationSign, StateRelation};
use crate::pts::system::StateSystem;
use crate::pts::transition::{
    Assignment, Sampling, StateAssignment, StateSampling, Transition, UpdateOperation,
};
use crate::pts::variable::program_variable::{ProgramVariable, ProgramVariables};
use crate::pts::PTS;

use super::grammars::default::{DefaultPestParser, Rule};
use super::{handle_pest_error, Parser, ParserError};

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Power,
}

fn odds_to_probabilities(odds: Vec<Constant>) -> Result<Vec<Constant>, ParserError> {
    // checks for negative probabilities
    assert!(odds.iter().find(|x| x.is_negative()).is_none());
    let sum: Constant = odds.to_owned().into_iter().sum();
    if sum.is_zero() {
        Err(ParserError {
            message: "the sum of odds cannot be zero".to_string(),
        })
    } else {
        // could probably return the iterator, but that poses some lifetime issues
        Ok(odds.into_iter().map(|x| x / sum).collect())
    }
}

fn is_keyword(var: &ProgramVariable) -> bool {
    vec!["if", "else", "while", "odds", "choose", "random", "and"]
        .contains(&var.to_string().as_str())
}

pub struct DefaultParser;
impl Parser for DefaultParser {
    fn parse(input: &str) -> Result<PTS, ParserError> {
        match DefaultPestParser::parse(Rule::program, input) {
            Err(error) => Err(handle_pest_error(error)),
            Ok(mut parse) => {
                let mut pts = Default::default();
                parse_program(&mut pts, parse.next().unwrap())?;
                assert_eq!(parse.next(), None);
                if (&pts.variables)
                    .into_iter()
                    .map(is_keyword)
                    .fold(false, |acc, x| acc || x)
                {
                    Err(ParserError {
                        message: "keywords cannot be used as variable names".to_string(),
                    })
                } else {
                    pts.finalize();
                    Ok(pts)
                }
            }
        }
    }
}

fn parse_linear_polynomial<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> State {
    assert_eq!(parse.to_owned().as_rule(), Rule::linear_polynomial);
    // parse.as_str() upholds invariants for parse_polynomial
    LinearPolynomialParser::parse_polynomial(variables, parse.as_str())
}

// assumes the parses rule is Rule::assignment_statement
fn parse_assignment<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> StateAssignment {
    assert_eq!(parse.to_owned().as_rule(), Rule::assignment_statement);
    let mut pairs = parse.into_inner();
    // pairs.next().unwrap().as_str() upholds invariants for parse_variable
    let var = LinearPolynomialParser::parse_variable(variables, pairs.next().unwrap().as_str());
    let pol: State = parse_linear_polynomial(variables, pairs.next().unwrap());
    Assignment::new(var, pol)
}

// assumes the parses rule is Rule::sampling_statement
fn parse_sampling<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> Result<StateSampling, ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::sampling_statement);
    let mut pairs = parse.into_inner();
    let var = LinearPolynomialParser::parse_variable(variables, pairs.next().unwrap().as_str());
    let min = LinearPolynomialParser::parse_constant(pairs.next().unwrap().as_str());
    let max = LinearPolynomialParser::parse_constant(pairs.next().unwrap().as_str());
    let expected_value = LinearPolynomialParser::parse_constant(pairs.next().unwrap().as_str());
    match Sampling::new(var, min, max, expected_value) {
        Some(x) => Ok(x),
        None => Err(ParserError {
            message: "incorrect bounds".to_string(),
        }),
    }
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
    assert!(parse.to_owned().as_rule() == Rule::conjunction);
    let mut system = StateSystem::default();
    for pair in parse.into_inner() {
        system.push(parse_inequality(variables, pair));
    }
    system
}

// assumes the parses rule is Rule::disjunction
fn parse_disjunction<'parse>(
    variables: &mut ProgramVariables,
    parse: Pair<'parse, Rule>,
) -> Invariant {
    assert!(parse.to_owned().as_rule() == Rule::disjunction);
    let iter = parse.into_inner();
    let mut assertions = Vec::with_capacity(iter.len());
    for pair in iter {
        assertions.push(parse_inequality_system(variables, pair));
    }
    Invariant::from(assertions)
}

// assumes the parses rule is Rule::program
fn parse_program<'parse>(pts: &mut PTS, parse: Pair<'parse, Rule>) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::program);
    let mut transition = Transition::default();
    let mut iter = parse.into_inner();
    parse_locations(
        pts,
        iter.next().unwrap(),
        &mut transition,
        pts.locations.get_terminating_location(),
    )?;

    pts.locations.initial = transition.target;
    let invariant_parse = iter.next().unwrap();
    // invariant_parse can be EOI or Invariant
    let invariant = if invariant_parse.as_rule() == Rule::disjunction {
        parse_disjunction(&mut pts.variables, invariant_parse)
    } else {
        invariant!(&mut pts.variables, ["<=", 0.0])
    };

    pts.locations
        .set_invariant(pts.locations.get_terminating_location(), invariant); // can unwrap because parse_disjunction never returns an invalid invariant
    Ok(())
}

// assumes the parses rule is Rule::statements
fn parse_locations<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start_transition: &mut Transition,
    end: LocationHandle,
) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::statements);
    let parse_locations = parse.into_inner();
    let mut pts_locations = pts
        .locations
        .new_n_locations(parse_locations.len())
        .peekable();

    start_transition.target = pts_locations.peek().unwrap_or(&end).to_owned();

    for ((local_start, local_end), pair) in std::iter::zip(
        std::iter::zip(
            pts_locations.to_owned(),
            pts_locations.skip(1).chain(std::iter::once(end)),
        ),
        parse_locations,
    ) {
        let mut location_iter = pair.into_inner();
        let invariant_parse = location_iter.next();
        let mut instruction_parse = location_iter.next();

        let invariant = if instruction_parse.is_some() {
            parse_disjunction(&mut pts.variables, invariant_parse.unwrap())
        } else {
            instruction_parse = invariant_parse;
            invariant!(&mut pts.variables, ["<=", 0.0])
        };

        pts.locations.set_invariant(local_start, invariant); // can unwrap because parse_disjunction never returns an invalid invariant

        let instruction_parse = instruction_parse.unwrap();
        match instruction_parse.as_rule() {
            Rule::if_statement => parse_if(pts, instruction_parse, local_start, local_end)?,
            Rule::odds_statement => parse_odds(pts, instruction_parse, local_start, local_end)?,
            Rule::choose_statement => parse_nondet(pts, instruction_parse, local_start, local_end)?,
            Rule::while_statement => parse_while(pts, instruction_parse, local_start, local_end)?,
            Rule::assignment_statement | Rule::sampling_statement => {
                parse_assign(pts, instruction_parse, local_start, local_end)?
            }
            _ => panic!("{}", INVARIANT_ERROR),
        };
    }
    Ok(())
}

// assumes the parses rule is Rule::assignment_statement or Rule::sampling_statement
fn parse_assign<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) -> Result<(), ParserError> {
    match parse.to_owned().as_rule() {
        Rule::assignment_statement => {
            pts.locations
                .set_outgoing(
                    start,
                    Guards::Unguarded(Box::new(Transition {
                        update_function: vec![UpdateOperation::Assignment(parse_assignment(
                            &mut pts.variables,
                            parse,
                        ))],
                        target: end,
                    })), // can unwrap here, since local_start can't be None and parse_assignment always returns
                )
                .unwrap()
        }
        Rule::sampling_statement => pts
            .locations
            .set_outgoing(
                start,
                Guards::Unguarded(Box::new(Transition {
                    update_function: vec![UpdateOperation::Sampling(parse_sampling(
                        &mut pts.variables,
                        parse,
                    )?)],
                    target: end,
                })),
            )
            .unwrap(),
        _ => panic!("{}", INVARIANT_ERROR),
    }
    Ok(())
}

// assumes the parses rule is Rule::*_condition or Rule::nondeterminism_sign
fn parse_condition<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    end: LocationHandle,
) -> Result<Guards, ParserError> {
    let line_col = parse.as_span().start_pos().line_col();
    match parse.to_owned().as_rule() {
        Rule::disjunction => {
            let loop_condition = parse_disjunction(&mut pts.variables, parse);
            Ok(Guards::Logic(vec![
                // default gets replaced
                (loop_condition.to_owned(), Default::default()),
                (
                    !loop_condition,
                    Transition {
                        update_function: Default::default(),
                        target: end,
                    },
                ),
            ]))
        }
        Rule::odds_condition => {
            let mut odds: Vec<Constant> = Default::default();
            for pair in parse.into_inner() {
                // pairs.as_str() upholds invariants for parse_constant
                odds.push(LinearPolynomialParser::parse_constant(pair.as_str()));
            }
            // assert_eq!(odds.len(), 2);
            let probabilities = odds_to_probabilities(odds).map_err(|mut e| {
                e.message = format!(" --> {}:{}\n  = {}\n", line_col.0, line_col.1, e.message);
                e
            })?;
            Ok(Guards::Probabilistic(vec![
                // default gets replaced
                (probabilities[0], Default::default()),
                (
                    probabilities[1],
                    Transition {
                        update_function: Default::default(),
                        target: end,
                    },
                ),
            ]))
        }
        Rule::nondeterminism_sign => Ok(Guards::Nondeterministic(vec![
            // default gets replaced
            Default::default(),
            Transition {
                update_function: Default::default(),
                target: end,
            },
        ])),
        _ => panic!("{}", INVARIANT_ERROR),
    }
}

// assumes the parses rule is Rule::while_statement
fn parse_while<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::while_statement);
    // condition ~ locations
    let mut parse_iter = parse.into_inner();
    // parse conditions
    let mut guards: Guards = parse_condition(pts, parse_iter.next().unwrap(), end)?;
    // parse locations
    match guards {
        Guards::Logic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0].1, start)?
        }
        Guards::Probabilistic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0].1, start)?
        }

        Guards::Nondeterministic(ref mut vector) => {
            parse_locations(pts, parse_iter.next().unwrap(), &mut vector[0], start)?
        }

        _ => panic!("{}", INVARIANT_ERROR),
    };

    // start cannot be None, see parse_locations, guards cannot be empty, see parse_condition
    pts.locations.set_outgoing(start, guards).unwrap();
    Ok(())
}

// assumes the parses rule is Rule::choose_statement
fn parse_nondet<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::choose_statement);
    let mut transitions = vec![];
    for locations_parse in parse.into_inner() {
        transitions.push(Default::default());
        parse_locations(pts, locations_parse, transitions.last_mut().unwrap(), end)?;
    }

    // grammar doesnt allow empty transitions, start will never be None, its ok to unwrap
    pts.locations
        .set_outgoing(start, Guards::Nondeterministic(transitions))
        .unwrap();
    Ok(())
}

// assumes the parses rule is Rule::if_statement
fn parse_if<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::if_statement);
    let mut else_condition = Invariant::one();
    let mut conditions: Vec<Invariant> = vec![];
    let mut transitions: Vec<Transition> = vec![];

    for pair in parse.into_inner() {
        match pair.as_rule() {
            Rule::disjunction => {
                parse_if_condition(pts, pair, &mut conditions, &mut else_condition)
            }
            Rule::statements => {
                transitions.push(Transition::default());
                parse_locations(pts, pair, transitions.last_mut().unwrap(), end)?;
            }
            _ => panic!("{}", INVARIANT_ERROR),
        }
    }

    conditions.push(else_condition);
    if conditions.len() > transitions.len() {
        transitions.push(Transition {
            update_function: Default::default(),
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
    Ok(())
}

// assumes the parses rule is Rule::logic_condition
fn parse_if_condition<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    conditions: &mut Vec<Invariant>,
    negation_acc: &mut Invariant,
) {
    assert_eq!(parse.to_owned().as_rule(), Rule::disjunction);
    let new_cond: Invariant = parse_disjunction(&mut pts.variables, parse);
    conditions.push(negation_acc.to_owned());
    let pushed_cond: &mut Invariant = conditions.last_mut().unwrap();
    *pushed_cond *= &new_cond;
    *negation_acc *= &!new_cond;
}

// assumes the parses rule is Rule::odds_statement
fn parse_odds<'parse>(
    pts: &mut PTS,
    parse: Pair<'parse, Rule>,
    start: LocationHandle,
    end: LocationHandle,
) -> Result<(), ParserError> {
    assert_eq!(parse.to_owned().as_rule(), Rule::odds_statement);
    // constant^n ~ block^(n-1)
    let line_col = parse.as_span().start_pos().line_col();

    let mut odds: Vec<Constant> = vec![];
    let mut parse_iter = parse.into_inner();

    while parse_iter
        .peek()
        .is_some_and(|x| x.as_rule() == Rule::constant)
    {
        // parse_iter.next().unwrap().as_str() upholds invariants for parse_constant
        odds.push(LinearPolynomialParser::parse_constant(
            parse_iter.next().unwrap().as_str(),
        ));
    }
    let probabilities = odds_to_probabilities(odds).map_err(|mut e| {
        e.message = format!(" --> {}:{}\n  = {}\n", line_col.0, line_col.1, e.message);
        e
    })?;
    let mut prob_iter = probabilities.into_iter();
    let mut guards: Vec<(Constant, Transition)> = vec![];
    for locations_parse in parse_iter {
        assert_eq!(locations_parse.to_owned().as_rule(), Rule::statements);
        // assert!(prob_iter.peek().is_some());
        guards.push((prob_iter.next().unwrap(), Default::default()));

        parse_locations(pts, locations_parse, &mut guards.last_mut().unwrap().1, end)?
    }
    // assert!(prob_iter.peek().is_none());

    // start cannot be None, see parse_locations, guards cannot be empty, see grammar
    pts.locations
        .set_outgoing(start, Guards::Probabilistic(guards))
        .unwrap();
    Ok(())
}

#[cfg(test)]
mod tests {

    use pest::Parser as PestParser;

    use crate::{
        guards, invariant,
        misc::tests::parsers::default::{
            INVARIANT_PROGRAM, SIMPLE_CHOOSE_PROGRAM, SIMPLE_IF_PROGRAM, SIMPLE_ODDS_PROGRAM,
            SIMPLE_PROGRAM, TRIVIAL_CHOOSE_PROGRAM, TRIVIAL_IF_PROGRAM, TRIVIAL_ODDS_PROGRAM,
            TRIVIAL_PROGRAM, WHILE_LOGIC_PROGRAM, WHILE_NONDETERMINISTIC_PROGRAM,
            WHILE_PROB_PROGRAM,
        },
        parsers::{
            default::{parse_assignment, parse_inequality, parse_inequality_system, DefaultParser},
            grammars::default::{DefaultPestParser, Rule},
            Parser,
        },
        program_variables,
        pts::{
            location::Locations, transition::UpdateOperation,
            variable::program_variable::ProgramVariables, PTS,
        },
        state_assignment, state_relation, transition,
    };

    #[test]
    fn assignment_sanity() {
        let mut variables = program_variables!("x", "a", "b", "c");
        let mut parse =
            DefaultPestParser::parse(Rule::assignment_statement, "x = -2a + 4b - 0c - 2").unwrap();
        let assign = parse_assignment(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        assert_eq!(
            UpdateOperation::Assignment(assign),
            state_assignment!(
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
        let mut variables: ProgramVariables = program_variables!();
        let mut parse = DefaultPestParser::parse(Rule::inequality, "3a - 4 + b < 0").unwrap();
        let pair = parse.next().unwrap();
        assert!(parse.next().is_none());
        let cond = parse_inequality(&mut variables, pair);
        assert_eq!(
            cond,
            state_relation!["<", -4.0, &mut variables, 3.0, "a", 1.0, "b"]
        );
    }

    #[test]
    fn inequality_system_sanity() {
        let mut variables: ProgramVariables = program_variables!("a", "b", "c");
        let mut parse =
            DefaultPestParser::parse(Rule::conjunction, "- 2b - 4 < - a and 0 >= 0 and a >= 0")
                .unwrap();
        let system = parse_inequality_system(&mut variables, parse.next().unwrap());
        assert!(parse.next().is_none());
        let cond = system.get(0).unwrap();
        assert_eq!(
            *cond,
            state_relation!("<", -4.0, &mut variables, 1.0, "a", -2.0, "b", 0.0, "c")
        );
        let cond = system.get(1).unwrap();
        assert_eq!(
            *cond,
            state_relation!(">=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c")
        );
        let cond = system.get(2).unwrap();
        assert_eq!(
            *cond,
            state_relation!(">=", 0.0, &mut variables, 1.0, "a", 0.0, "b", 0.0, "c")
        );
        assert!(system.get(3).is_none());
    }

    #[test]
    fn invariant_sanity() {
        let input = INVARIANT_PROGRAM;
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let handle = locations.new_location();
        let mut variables: ProgramVariables = program_variables!("a", "b", "c");

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
                guards!(transition![
                    None,
                    state_assignment![&mut variables, "a", 0.0, 1.0, "a", 0.0, "b"]
                ]),
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

        let variables = program_variables!("a", "b");

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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!();

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
                guards!(transition!(
                    next_location,
                    state_assignment!(&mut variables, "b", 1.0, 0.0, "a", 0.0, "b")
                )),
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
                guards!(transition!(
                    next_location,
                    state_assignment!(&mut variables, "c", 0.0, 1.0, "a", 1.0, "b", 0.0, "c")
                )),
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
                guards!(transition!(
                    next_location,
                    state_assignment!(&mut variables, "b", 0.0, 2.0, "a", 0.0, "b", 1.0, "c")
                )),
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
        let parsed = DefaultParser::parse(input).unwrap();
        let mut variables: ProgramVariables = program_variables!("a", "b", "c",);

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
                guards!(transition![
                    None,
                    state_assignment!(&mut variables, "a", 1.0, 0.0, "a")
                ]),
            )
            .unwrap();
        // 3
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 1.0, 0.0, "a"]),
        );

        let variables = program_variables!("a");

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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);
        let mut variables: ProgramVariables = program_variables!();

        let start = locations_iter.next().unwrap();
        locations.initial = start;
        locations.set_invariant(start, invariant!(&mut pts.variables, ["<=", 0.0]));

        // line #
        let junction = locations_iter.next().unwrap();
        locations.set_invariant(junction, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_1 = locations_iter.next().unwrap();
        locations.set_invariant(branch_1, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_2 = locations_iter.next().unwrap();
        locations.set_invariant(branch_2, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_3 = locations_iter.next().unwrap();
        locations.set_invariant(branch_3, invariant!(&mut pts.variables, ["<=", 0.0]));

        locations
            .set_outgoing(
                start,
                guards!(L:
                    // 1
                    invariant!(&mut variables,[
                                 ">=", 0.0, 1.0, "a", -1.0, "b"]),
                    transition!(branch_1),
                    // 4
                    invariant!(&mut variables,[
                        "<", 0.0, 1.0, "a", -1.0, "b";
                        "<=", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"]
                    ),
                    transition!(branch_2),
                    // 7
                    invariant!(&mut variables,[
                        "<", 0.0, 1.0, "a", -1.0, "b";
                        ">", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"]
                    ),
                    transition!(branch_3),
                ),
            )
            .unwrap();

        // 2
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b")
                )),
            )
            .unwrap();

        // 5
        locations
            .set_outgoing(
                branch_2,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                )),
            )
            .unwrap();

        // 8
        locations
            .set_outgoing(
                branch_3,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                )),
            )
            .unwrap();

        // 10
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                )),
            )
            .unwrap();

        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut pts.variables, ["<=", 0.0]),
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a", "b");

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
                    invariant!(&mut variables,[ ">=", 0.0, 1.0, "a", -1.0, "b"]),
                    transition!(branch_1),

                    // 5
                    invariant!(&mut variables,[ "<", 0.0, 1.0, "a", -1.0, "b"])
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
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b")
                )),
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
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b")
                )),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
        );

        let variables = program_variables!("a", "b");

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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(4);
        let mut variables: ProgramVariables = program_variables!();

        let start = locations_iter.next().unwrap();
        locations.set_invariant(start, invariant!(&mut pts.variables, ["<=", 0.0]));
        locations.initial = start;

        // line #
        let junction = locations_iter.next().unwrap();
        locations.set_invariant(junction, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_1 = locations_iter.next().unwrap();
        locations.set_invariant(branch_1, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_2 = locations_iter.next().unwrap();
        locations.set_invariant(branch_2, invariant!(&mut pts.variables, ["<=", 0.0]));

        locations
            .set_outgoing(
                start,
                guards!(P:
                    // 1
                    0.0,
                    transition!(branch_1),
                    // 4
                    1.0,
                    transition!(branch_2),
                ),
            )
            .unwrap();

        // 2
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();

        // 5
        locations
            .set_outgoing(
                branch_2,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "b", 0.0, 1.0, "a", 0.0, "b")
                )),
            )
            .unwrap();

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a", 0.0, "b")
                )),
            )
            .unwrap();
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut pts.variables, ["<=", 0.0]),
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a");

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
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();

        // 6
        locations.set_invariant(junction, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);
        let mut variables: ProgramVariables = program_variables!("a");

        let start = locations_iter.next().unwrap();
        locations.initial = start;
        locations.set_invariant(start, invariant!(&mut pts.variables, ["<=", 0.0]));

        // line #
        let junction = locations_iter.next().unwrap();
        locations.set_invariant(junction, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_1 = locations_iter.next().unwrap();
        locations.set_invariant(branch_1, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_2 = locations_iter.next().unwrap();
        locations.set_invariant(branch_2, invariant!(&mut pts.variables, ["<=", 0.0]));
        let branch_3 = locations_iter.next().unwrap();
        locations.set_invariant(branch_3, invariant!(&mut pts.variables, ["<=", 0.0]));
        locations
            .set_outgoing(
                start,
                guards!(
                    // 1
                    transition!(branch_1),
                    // 4
                    transition!(branch_2),
                    // 7
                    transition!(branch_3)
                ),
            )
            .unwrap();

        // 2
        locations
            .set_outgoing(
                branch_1,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();

        // 5
        locations
            .set_outgoing(
                branch_2,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();

        // 8
        locations
            .set_outgoing(
                branch_3,
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();

        // 10
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();
        locations.set_invariant(
            locations.get_terminating_location(),
            invariant!(&mut pts.variables, ["<=", 0.0]),
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a");

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
                guards!(transition!(
                    junction,
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
                )),
            )
            .unwrap();
        // 6
        locations.set_invariant(junction, invariant!(&mut variables, [">", 0.0, 0.0, "a"]));

        // 7
        locations
            .set_outgoing(
                junction,
                guards!(transition!(
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 1.0, "a")
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a", "b");

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
                    invariant!(&mut variables,[
                        ">", 0.0, 0.0, "a";
                        "<", 0.0, 0.0, "a"]
                    ),
                    transition!(branch_1),
                    // 5
                    invariant!(&mut variables,
                               ["<=", 0.0, 0.0, "a"],
                               [">=", 0.0, 0.0, "a"]
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
                guards!(transition!(
                    start,
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
                )),
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
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
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
    fn parse_prob_while_program() {
        let input = WHILE_PROB_PROGRAM;
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a", "b");

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
                guards!(transition!(
                    start,
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
                )),
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
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
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
        let parsed = DefaultParser::parse(input).unwrap();

        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);
        let mut variables: ProgramVariables = program_variables!("a", "b");

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
                guards!(transition!(
                    start,
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
                )),
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
                    locations.get_terminating_location(),
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a", 1.0, "b")
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
        use crate::parsers::{default::DefaultParser, Parser};

        #[test]
        fn garbage() {
            // not a very good error
            assert_eq!(
                DefaultParser::parse("test").err().unwrap().to_string(),
                " --> 1:1\n  |\n1 | test\n  | ^---\n  |\n  = expected program"
            );
        }

        #[test]
        fn variable_name_keyword() {
            assert_eq!(
                DefaultParser::parse("x = if").err().unwrap().to_string(),
                "keywords cannot be used as variable names"
            );
        }

        #[test]
        fn if_statement() {
            assert_eq!(
                DefaultParser::parse("if").err().unwrap().to_string(),
                " --> 1:3\n  |\n1 | if\n  |   ^---\n  |\n  = expected linear_polynomial"
            );
        }

        #[test]
        fn odds_statement() {
            assert_eq!(
                DefaultParser::parse("odds").err().unwrap().to_string(),
                " --> 1:5\n  |\n1 | odds\n  |     ^---\n  |\n  = expected constant"
            );
        }

        #[test]
        fn choose_statement() {
            assert_eq!(
                DefaultParser::parse("choose").err().unwrap().to_string(),
                " --> 1:7\n  |\n1 | choose\n  |       ^---\n  |\n  = expected choose_statement"
            );
        }

        #[test]
        fn while_statement() {
            assert_eq!(
                DefaultParser::parse("while").err().unwrap().to_string(),
                " --> 1:6\n  |\n1 | while\n  |      ^---\n  |\n  = expected while_statement"
            );
        }

        #[test]
        fn assignment_statement() {
            assert_eq!(
                DefaultParser::parse("a =").err().unwrap().to_string(),
                " --> 1:4\n  |\n1 | a =\n  |    ^---\n  |\n  = expected linear_polynomial"
            );
        }

        #[test]
        fn branching_odds_sum_zero() {
            assert_eq!(
                DefaultParser::parse("odds 0:0{\n#0<0\na=0\n}\n{\n#0<0\na=0\n}#0<0\n")
                    .err()
                    .unwrap()
                    .to_string(),
                " --> 1:6\n  = the sum of odds cannot be zero\n"
            );
        }

        #[test]
        fn while_odds_sum_zero() {
            assert_eq!(
                DefaultParser::parse("while 0:0{\n#0<0\na=0\n}#0<0\n")
                    .err()
                    .unwrap()
                    .to_string(),
                " --> 1:7\n  = the sum of odds cannot be zero\n"
            );
        }
    }
}
