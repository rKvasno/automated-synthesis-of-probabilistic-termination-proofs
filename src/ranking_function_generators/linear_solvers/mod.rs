pub mod minilp;

use std::collections::HashMap;

use crate::pts::{
    linear_polynomial::{coefficient::Constant, State},
    system::StateSystem,
    variable::program_variable::{ProgramVariable, ProgramVariables},
};

pub trait Solver {
    fn solve<Solution: Iterator<Item = (ProgramVariables, Constant)>>(
        problem: Problem,
    ) -> Option<Solution>;
}

#[derive(Clone)]
pub enum Goal {
    Minimize(ProgramVariables, State),
    Maximize(ProgramVariables, State),
}

#[derive(Clone)]
pub struct Interval(f64, f64);

impl Default for Interval {
    fn default() -> Self {
        Self(f64::NEG_INFINITY, f64::INFINITY)
    }
}

#[derive(Clone)]
pub struct Problem {
    // assumes every variable is in "variables"
    // HashMap has constant next() on its iterator and good insert/update time
    pub variables: ProgramVariables,
    // if None, default to R
    pub domains: HashMap<ProgramVariable, Interval>,
    pub restrictions: StateSystem,
    pub goal: Goal,
}
