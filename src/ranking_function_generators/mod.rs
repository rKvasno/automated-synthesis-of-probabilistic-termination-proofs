use core::fmt;
use std::borrow::Cow;

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

type Edge = (LocationHandle, LocationHandle);

impl<'a> dot::GraphWalk<'a, LocationHandle, Edge> for RankedPTS {
    fn nodes(&'a self) -> dot::Nodes<'a, LocationHandle> {
        self.pts.nodes()
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        self.pts.edges()
    }

    fn source(&'a self, edge: &Edge) -> LocationHandle {
        edge.0
    }

    fn target(&'a self, edge: &Edge) -> LocationHandle {
        edge.1
    }
}

impl<'a> dot::Labeller<'a, LocationHandle, Edge> for RankedPTS {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("RankedPTS").unwrap()
    }

    fn node_id(&'a self, node: &LocationHandle) -> dot::Id<'a> {
        self.pts.node_id(node)
    }

    fn node_label(&'a self, n: &LocationHandle) -> dot::LabelText<'a> {
        // invariant
        dot::LabelText::LabelStr(Cow::Owned(
            self.function.get(n.to_owned()).unwrap().to_string(),
        ))
    }

    fn edge_label(&'a self, e: &Edge) -> dot::LabelText<'a> {
        self.pts.edge_label(e)
    }

    fn node_style(&'a self, node: &LocationHandle) -> dot::Style {
        self.pts.node_style(node)
    }

    fn node_shape(&'a self, node: &LocationHandle) -> Option<dot::LabelText<'a>> {
        self.pts.node_shape(node)
    }
}
