use crate::pts::{transition, inequality, linear_polynomial};
use transition::Transition;
use inequality::InequalitySystem;

use linear_polynomial::constant::Constant;

pub type Probability = Constant;

// 32 bytes
#[derive(Debug)]
pub enum Guards<'a>{
    Logic(Vec<(InequalitySystem, Transition<'a>)>),
    Probabilistic(Vec<(Probability, Transition<'a>)>),
    Nondeterministic(Vec<Transition<'a>>),
    Unguarded(Box<Transition<'a>>)
}

impl <'a> Default for Guards<'a> {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}
