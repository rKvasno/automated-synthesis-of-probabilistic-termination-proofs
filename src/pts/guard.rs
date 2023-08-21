use crate::pts::{transition, inequality, linear_polynomial};
use transition::Transition;
use inequality::InequalitySystem;

use linear_polynomial::constant::Constant;

pub type Probability = Constant;

// 32 bytes
#[derive(Debug)]
pub enum Guards{
    Logic(Vec<(InequalitySystem, Transition)>),
    Probabilistic(Vec<(Probability, Transition)>),
    Nondeterministic(Vec<Transition>),
    Unguarded(Box<Transition>)
}

impl Default for Guards {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}
