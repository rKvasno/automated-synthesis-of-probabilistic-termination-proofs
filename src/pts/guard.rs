use std::fmt;

use crate::pts::{inequality, linear_polynomial, transition};
use inequality::InequalitySystem;
use linear_polynomial::constant::Constant;
use transition::Transition;

use super::variable_map::VariableMap;

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
