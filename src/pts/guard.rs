use std::fmt;

use crate::pts::{inequality, linear_polynomial, transition};
use inequality::InequalitySystem;
use linear_polynomial::constant::Constant;
use transition::Transition;

use super::{variable_map::VariableMap, DisplayLabel};

pub type Probability = Constant;

// 32 bytes
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Guards {
    Logic(Vec<(InequalitySystem, Transition)>),
    Probabilistic(Vec<(Probability, Transition)>),
    Nondeterministic(Vec<Transition>),
    Unguarded(Box<Transition>),
}

impl Guards {
    pub fn is_empty(&self) -> bool {
        match self {
            Guards::Logic(data) => data.is_empty(),
            Guards::Probabilistic(data) => data.is_empty(),
            Guards::Nondeterministic(data) => data.is_empty(),
            Guards::Unguarded(_) => false,
        }
    }

    pub fn iter(&self) -> GuardsIterator {
        GuardsIterator(self, 0)
    }
}

impl Default for Guards {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GuardsError {
    TerminatingLocation,
    Empty,
}

impl fmt::Display for GuardsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GuardsError::Empty => write!(f, "Guards can't be empty!"),
            GuardsError::TerminatingLocation => write!(
                f,
                "The terminating location can't have outgoing transitions!"
            ),
        }
    }
}

pub enum GuardedTransition<'a> {
    Logic(&'a (InequalitySystem, Transition)),
    Probabilistic(&'a (Probability, Transition)),
    Nondeterministic(&'a Transition),
    Unguarded(&'a Transition),
}

impl<'a> GuardedTransition<'a> {
    pub fn as_transition(&'a self) -> &'a Transition {
        match self {
            Self::Logic((_, t)) => t,
            Self::Probabilistic((_, t)) => t,
            Self::Nondeterministic(t) => t,
            Self::Unguarded(t) => t,
        }
    }
}

impl<'a> DisplayLabel for GuardedTransition<'a> {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = match self {
            Self::Logic((condition, _)) => condition.label(variable_map),
            Self::Probabilistic((probability, _)) => format!("{}", probability),
            Self::Nondeterministic(_) => String::default(),
            Self::Unguarded(_) => String::default(),
        };
        for assign in self.as_transition().assignments.iter() {
            label.push_str("\n");
            label.push_str(assign.label(variable_map).as_str());
        }
        label
    }
}

pub struct GuardsIterator<'a>(&'a Guards, usize);

impl<'a> Iterator for GuardsIterator<'a> {
    type Item = GuardedTransition<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.0 {
            &Guards::Logic(ref branches) => {
                branches.get(self.1).map(|x| GuardedTransition::Logic(x))
            }
            &Guards::Probabilistic(ref branches) => branches
                .get(self.1)
                .map(|x| GuardedTransition::Probabilistic(x)),
            &Guards::Nondeterministic(ref branches) => branches
                .get(self.1)
                .map(|x| GuardedTransition::Nondeterministic(x)),
            &Guards::Unguarded(ref boxed_transition) => {
                if self.1 == 0 {
                    Some(GuardedTransition::Unguarded(boxed_transition))
                } else {
                    None
                }
            }
        };
        self.1 += 1;
        item
    }
}

#[cfg(test)]
mod tests {
    use crate::pts::{
        guard::GuardedTransition,
        inequality::{Inequality, InequalitySystem},
        linear_polynomial::{constant::Constant, LinearPolynomial},
        transition::{Assignment, Transition},
        variable_map::{Variable, VariableMap},
        DisplayLabel,
    };

    #[test]
    fn logic_label() {
        let data = (
            InequalitySystem::mock(vec![Inequality::mock(
                true,
                LinearPolynomial::mock(vec![Constant(1.0)]),
            )]),
            Transition {
                assignments: Vec::default(),
                target: None,
            },
        );
        let guarded_transition = GuardedTransition::Logic(&data);
        let map = VariableMap::mock(Default::default());
        assert_eq!(guarded_transition.label(&map), "0 < -1");

        let data = (
            InequalitySystem::mock(vec![
                Inequality::mock(
                    false,
                    LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                ),
                Inequality::mock(
                    true,
                    LinearPolynomial::mock(vec![Constant(1.0), Constant(2.0), Constant(1.0)]),
                    // 1 + 2a + len > 0
                    // 2a + len > -1
                ),
            ]),
            Transition {
                assignments: vec![Assignment(Variable::new("a"), LinearPolynomial::default())],
                target: None,
            },
        );
        let guarded_transition = GuardedTransition::Logic(&data);
        let map = VariableMap::mock(vec![Variable::new("a"), Variable::new("len")]);
        assert_eq!(
            guarded_transition.label(&map),
            "a <= 0\n2a + len < -1\na = 0"
        );
    }

    #[test]
    fn prob_label() {
        let data = (
            Constant(0.0),
            Transition {
                assignments: vec![
                    Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                    ),
                    Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![
                            Constant(2.0),
                            Constant(1.0),
                            Constant(2.0),
                            Constant(0.0),
                        ]),
                    ),
                    Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(-4.0),
                            Constant(0.0),
                            Constant(0.0),
                            Constant(1.0),
                        ]),
                    ),
                ],

                target: None,
            },
        );
        let guarded_transition = GuardedTransition::Probabilistic(&data);
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(
            guarded_transition.label(&map),
            "0\na = 0\nb = a + 2b + 2\na = c - 4"
        );

        let data = (
            Constant(0.2222),
            Transition {
                assignments: Vec::default(),
                target: None,
            },
        );
        let guarded_transition = GuardedTransition::Probabilistic(&data);
        let map = VariableMap::mock(Default::default());
        assert_eq!(guarded_transition.label(&map), "0.2222");
    }

    #[test]
    fn nondet_label() {
        let data = Transition {
            assignments: Vec::default(),
            target: None,
        };
        let guarded_transition = GuardedTransition::Nondeterministic(&data);
        let map = VariableMap::mock(Default::default());
        assert_eq!(guarded_transition.label(&map), "");
    }

    #[test]
    fn unguarded_label() {
        let data = Transition {
            assignments: Vec::default(),
            target: None,
        };
        let guarded_transition = GuardedTransition::Unguarded(&data);
        let map = VariableMap::mock(Default::default());
        assert_eq!(guarded_transition.label(&map), "");
    }
}
