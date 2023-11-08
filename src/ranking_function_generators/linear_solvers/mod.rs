pub mod minilp;

use std::{collections::HashMap, error::Error};

use crate::pts::{
    linear_polynomial::{coefficient::Constant, Polynomial},
    system::System,
    variable::{set::VariableSet, Variable},
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

pub trait Solver<V: Variable> {
    type Error: Error;
    type Solution: IntoIterator<Item = (V, Constant)>;
    fn solve(problem: Problem<V>) -> Result<Self::Solution, SolverError<Self::Error>>;
}

#[derive(Debug)]
pub enum SolverError<E: Error> {
    InvalidRelationType,
    ForeignVariable,
    Other(E),
}

impl<E: Error> std::fmt::Display for SolverError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidRelationType => {
                write!(f, "only equations and nonstrict inequalities are allowed")
            }
            Self::ForeignVariable => write!(f, "variable not in variable set"),
            Self::Other(e) => write!(f, "solver failed with: {}", e),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Goal<V: Variable> {
    Minimize(Polynomial<V, Constant>),
    Maximize(Polynomial<V, Constant>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Interval(pub f64, pub f64);

impl Default for Interval {
    fn default() -> Self {
        Self(f64::NEG_INFINITY, f64::INFINITY)
    }
}

impl Into<(f64, f64)> for Interval {
    fn into(self) -> (f64, f64) {
        (self.0, self.1)
    }
}

pub type DomainMap<V> = HashMap<V, Interval>;

#[derive(Clone)]
pub struct Problem<V: Variable> {
    // assumes every variable is in "variables"
    // HashMap has constant next() on its iterator and good insert/update time
    pub variables: VariableSet<V>,
    // if None, default to R
    pub domains: DomainMap<V>,
    pub restrictions: System<V, Constant>,
    pub goal: Goal<V>,
}
