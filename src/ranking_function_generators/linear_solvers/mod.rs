pub mod minilp;

use std::collections::HashMap;

use crate::pts::{
    linear_polynomial::{coefficient::Constant, State},
    system::StateSystem,
    variable::{program_variable::ProgramVariables, Variable},
};

#[macro_export]
macro_rules! domains {
    [
        $varset:expr
        $(
            ,$var:expr
            ,$lower:expr
            ,$upper:expr
        )*
        $(,)?
    ] => {
        {
            let mut temp = std::collections::hash_map::HashMap::default();

            $(
                temp.insert($crate::pts::variable::Variable::new($varset, $var), $crate::ranking_function_generators::linear_solvers::Interval($lower, $upper));
            )*

            temp
        }
    }
}

pub trait Solver {
    fn solve<V: Variable, Solution: Iterator<Item = (V, Constant)>>(
        problem: Problem<V>,
    ) -> Option<Solution>;
}

#[derive(Clone, Debug)]
pub enum Goal {
    Minimize(ProgramVariables, State),
    Maximize(ProgramVariables, State),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Interval(pub f64, pub f64);

impl Default for Interval {
    fn default() -> Self {
        Self(f64::NEG_INFINITY, f64::INFINITY)
    }
}

pub type DomainMap<V> = HashMap<V, Interval>;

#[derive(Clone)]
pub struct Problem<V: Variable> {
    // assumes every variable is in "variables"
    // HashMap has constant next() on its iterator and good insert/update time
    pub variables: V,
    // if None, default to R
    pub domains: DomainMap<V>,
    pub restrictions: StateSystem,
    pub goal: Goal,
}
