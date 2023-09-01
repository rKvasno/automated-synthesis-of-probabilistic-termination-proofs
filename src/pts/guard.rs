use std::fmt;

use crate::pts::{transition, inequality, linear_polynomial};
use transition::Transition;
use inequality::InequalitySystem;

use linear_polynomial::constant::Constant;

pub type Probability = Constant;

// 32 bytes
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Guards {
    Logic(Vec<(InequalitySystem, Transition)>),
    Probabilistic(Vec<(Probability, Transition)>),
    Nondeterministic(Vec<Transition>),
    Unguarded(Box<Transition>)
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
}

impl Default for Guards {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GuardsError{
    TerminatingLocation,
    Empty
}

impl fmt::Display for GuardsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GuardsError::Empty => write!(f, "Guards can't be empty!"),
            GuardsError::TerminatingLocation => write!(f, "The terminating location can't have outgoing transitions!"),
        }
    }
}

