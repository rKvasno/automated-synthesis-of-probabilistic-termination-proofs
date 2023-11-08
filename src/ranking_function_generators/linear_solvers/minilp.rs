use std::collections::{hash_map::IntoIter, HashMap};

use crate::{
    pts::{linear_polynomial::coefficient::Constant, variable::Variable},
    ranking_function_generators::linear_solvers::Goal,
};

use super::{Problem, Solver, SolverError};

use minilp::{
    Error as MinilpError, LinearExpr, OptimizationDirection, Problem as MinilpProblem,
    Solution as MinilpCrateSolution, Variable as MinilpVariable,
};

struct Minilp();

#[derive(Debug, Clone)]
struct MinilpSolution<V: Variable> {
    variables: HashMap<V, MinilpVariable>,
    solution: MinilpCrateSolution,
}

#[derive(Debug)]
struct MinilpIterator<V: Variable> {
    variable_iter: IntoIter<V, MinilpVariable>,
    solution: MinilpCrateSolution,
}

impl<V: Variable> MinilpSolution<V> {
    pub fn new(variables: HashMap<V, MinilpVariable>, solution: MinilpCrateSolution) -> Self {
        Self {
            variables,
            solution,
        }
    }
}
impl<V: Variable> IntoIterator for MinilpSolution<V> {
    type Item = (V, Constant);
    type IntoIter = MinilpIterator<V>;
    fn into_iter(self) -> Self::IntoIter {
        MinilpIterator {
            variable_iter: self.variables.into_iter(),
            solution: self.solution,
        }
    }
}

impl<V: Variable> Iterator for MinilpIterator<V> {
    type Item = (V, Constant);
    fn next(&mut self) -> Option<Self::Item> {
        let (var, index) = self.variable_iter.next()?;
        Some((var, self.solution[index.clone()].into()))
    }
}

impl<V: Variable> Solver<V> for Minilp {
    type Error = MinilpError;
    type Solution = MinilpSolution<V>;
    fn solve(problem: Problem<V>) -> Result<Self::Solution, SolverError<Self::Error>> {
        let (direction, function) = match problem.goal {
            Goal::Minimize(pol) => (OptimizationDirection::Minimize, pol),
            Goal::Maximize(pol) => (OptimizationDirection::Maximize, pol),
        };

        let mut minilp = MinilpProblem::new(direction);
        let mut varmap = HashMap::<V, MinilpVariable>::default();
        for var in &problem.variables {
            varmap.insert(
                var.clone(),
                minilp.add_var(
                    function
                        .get_coefficient(&Some(var.clone()))
                        .cloned()
                        .unwrap_or_default()
                        .into(),
                    problem
                        .domains
                        .get(var.borrow())
                        .cloned()
                        .unwrap_or_default()
                        .into(),
                ),
            );
        }
        for restriction in problem.restrictions {
            let mut expr = LinearExpr::empty();
            let mut constant = 0.0;
            for (var, coeff) in restriction.as_linear_polynomial() {
                match var {
                    Some(v) => {
                        let v = varmap.get(v.borrow()).cloned();
                        if v.is_none() {
                            return Err(SolverError::ForeignVariable);
                        }
                        expr.add(v.unwrap(), coeff.clone().into())
                    }
                    None => constant = coeff.clone().into(),
                };
            }
            let comp = if restriction.is_equation() {
                minilp::ComparisonOp::Eq
            } else if restriction.is_nonstrict_inequality() {
                minilp::ComparisonOp::Le
            } else {
                return Err(SolverError::InvalidRelationType);
            };
            minilp.add_constraint(expr, comp, -constant)
        }

        let solution = match minilp.solve() {
            Ok(sol) => sol,
            Err(e) => return Err(SolverError::Other(e)),
        };

        Ok(MinilpSolution::new(varmap, solution))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        domains,
        pts::{
            linear_polynomial::coefficient::Constant,
            variable::program_variable::{ProgramVariable, ProgramVariables},
        },
        ranking_function_generators::linear_solvers::{minilp::Minilp, Goal, Problem, Solver},
        state, state_system,
    };

    #[test]
    fn solve() {
        let mut variables: ProgramVariables = Default::default();
        let domains = domains!(&mut variables, "b", 0.0, f64::INFINITY);
        let restrictions = state_system!(
            &mut variables;
            "<=",
            10.0,
            3.0,
            "a",
            5.0,
            "b";
            "<=",
            20.0,
            1.0,
            "a",
            1.0,
            "b"
        );

        let goal = Goal::Maximize(state!(0.0, &mut variables, 1.0, "a", -1.0, "b"));

        let problem = Problem {
            variables,
            domains,
            restrictions,
            goal,
        };

        let solution = Minilp::solve(problem.clone()).unwrap();

        let solution_vec: Vec<(ProgramVariable, Constant)> = solution.into_iter().collect();
        assert!(
            (solution_vec
                == vec![
                    (problem.variables.get("b").unwrap().clone(), Constant(0.0)),
                    (problem.variables.get("a").unwrap().clone(), Constant(-20.0))
                ])
                || (solution_vec
                    == vec![
                        (problem.variables.get("a").unwrap().clone(), Constant(-20.0)),
                        (problem.variables.get("b").unwrap().clone(), Constant(0.0)),
                    ])
        )
    }
}
