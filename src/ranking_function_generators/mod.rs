use core::fmt;

use crate::pts::{
    linear_polynomial::{coefficient::Constant, State},
    variable::{program_variable::ProgramVariable, Variable},
    PTS,
};

#[derive(Debug, Default)]
pub struct RankingFunction {
    // indexed by LocationId
    _locations: Vec<State>,
}

use self::linear_solvers::{Problem, Solver};

pub mod farkas_based;
pub mod linear_solvers;

#[derive(Debug, Default)]
pub struct RankedPTS {
    _pts: PTS,
    _function: RankingFunction,
}

// write!(f, "f({}, ...) = {}\n", l, fun)?;

// impl<'a> dot::Labeller<'a, LocationHandle, Edge> for PTS {}
// impl<'a> dot::GraphWalk<'a, LocationHandle, Edge> for PTS {}

#[derive(Debug)]
pub enum GeneratorError {
    WrongFormat,
}

impl std::error::Error for GeneratorError {}

impl fmt::Display for GeneratorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorError::WrongFormat => write!(f, "incorrect format of solution"),
        }
    }
}

pub trait Generator {
    type VAR: Variable;
    fn generate_problem<S: Solver<Self::VAR>>(&self, pts: &PTS) -> Problem<Self::VAR>;
    fn build_ranking_function<Solution: Iterator<Item = (ProgramVariable, Constant)>>(
        &self,
        pts: PTS,
        solution: Solution,
    ) -> Result<RankedPTS, GeneratorError>;
}
