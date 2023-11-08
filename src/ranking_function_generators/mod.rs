use core::fmt;

use crate::pts::{linear_polynomial::State, location::LocationHandle, variable::Variable, PTS};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RankingFunction {
    // indexed by LocationId
    data: Vec<State>,
    end_location: State,
}

impl RankingFunction {
    pub fn new(pts: &PTS) -> Self {
        Self {
            data: vec![Default::default(); pts.locations.len()],
            end_location: Default::default(),
        }
    }

    pub fn get(&self, location: LocationHandle) -> Option<&State> {
        match location {
            Some(i) => self.data.get(i),
            None => Some(&self.end_location),
        }
    }

    pub fn get_mut(&mut self, location: LocationHandle) -> Option<&mut State> {
        match location {
            Some(i) => self.data.get_mut(i),
            None => Some(&mut self.end_location),
        }
    }
}

use self::linear_solvers::{Problem, Solution, Solver};

pub mod farkas_based;
pub mod linear_solvers;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RankedPTS {
    pub pts: PTS,
    pub function: RankingFunction,
}

// write!(f, "f({}, ...) = {}\n", l, fun)?;

// impl<'a> dot::Labeller<'a, LocationHandle, Edge> for PTS {}
// impl<'a> dot::GraphWalk<'a, LocationHandle, Edge> for PTS {}

#[derive(Debug)]
pub enum GeneratorError {
    EpsIsZero,
}

impl std::error::Error for GeneratorError {}

impl fmt::Display for GeneratorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GeneratorError::EpsIsZero => write!(f, "no function was found"),
        }
    }
}

pub trait Generator {
    type VAR: Variable;
    fn generate_problem<S: Solver<Self::VAR>>(pts: &PTS) -> Problem<Self::VAR>;
    fn build_ranking_function<S: Solution<Self::VAR>>(
        pts: PTS,
        solution: S,
    ) -> Result<RankedPTS, GeneratorError>;
}
