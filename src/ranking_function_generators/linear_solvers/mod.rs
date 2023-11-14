pub mod minilp;

use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
};

use crate::pts::{
    linear_polynomial::{
        coefficient::{Coefficient, Constant},
        Polynomial,
    },
    relation::{Relation, RelationSign},
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
                temp.insert($crate::var!($varset, $var), $crate::ranking_function_generators::linear_solvers::Interval($lower, $upper));
            )*

            temp
        }
    }
}

pub trait Solution<V: Variable>: IntoIterator<Item = (V, Constant)> {}

#[derive(Debug, Hash, PartialEq, Eq)]
struct VariableWrapper<V: Variable> {
    data: Option<V>,
}

impl<V: Variable> VariableWrapper<V> {
    const EXTRA_VARIABLE: Self = Self { data: None };

    fn is_extra_variable(&self) -> bool {
        *self == Self::EXTRA_VARIABLE
    }

    fn wrap_variable<C: Coefficient>(
        wrapped_variables: &mut VariableSet<VariableWrapper<V>>,
        variable: V,
    ) -> VariableWrapper<V> {
        wrapped_variables.get_or_insert(VariableWrapper {
            data: Some(variable),
        })
    }

    fn wrap_polynomial<C: Coefficient>(
        wrapped_variables: &mut VariableSet<VariableWrapper<V>>,
        pol: &Polynomial<V, C>,
    ) -> Polynomial<VariableWrapper<V>, C> {
        let mut acc = Polynomial::<VariableWrapper<V>, C>::default();
        for (var, coeff) in pol.into_iter() {
            let wrapped_var: Option<VariableWrapper<V>> = var
                .clone()
                .map(|x| Self::wrap_variable::<Constant>(wrapped_variables, x));
            acc.add_term(coeff.clone(), wrapped_var);
        }
        acc
    }
}

impl<V: Variable> Clone for VariableWrapper<V> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.to_owned(),
        }
    }
}

impl<V: Variable + Debug> Variable for VariableWrapper<V> {}

pub trait Solver {
    type Error: Error;
    type Solution<V: Variable>: Solution<V>;
    fn solve<V: Variable>(problem: Problem<V>) -> Result<Self::Solution<V>, SolverError<V>>;
    fn is_empty<V: Variable>(polyhedron: &System<V, Constant>) -> Result<bool, SolverError<V>> {
        if polyhedron.is_empty() {
            return Ok(true);
        }

        let mut wrapped_variables: VariableSet<VariableWrapper<V>> = Default::default();
        wrapped_variables.insert(VariableWrapper::EXTRA_VARIABLE);

        let mut domains = HashMap::<VariableWrapper<V>, Interval>::default();
        domains.insert(
            VariableWrapper::EXTRA_VARIABLE,
            Interval(0.0, f64::INFINITY),
        );

        let has_strict = polyhedron
            .iter()
            .find(|x| Relation::is_strict_inequality(x))
            .is_some();

        let mut wrapped_restrictions = System::<VariableWrapper<V>, Constant>::default();
        for halfspace in polyhedron {
            if !(halfspace.is_nonstrict_inequality() || halfspace.is_strict_inequality()) {
                return Err(SolverError::InvalidRelationType(halfspace.clone()));
            }
            let mut wrapped_polynomial = VariableWrapper::wrap_polynomial(
                &mut wrapped_variables,
                halfspace.as_linear_polynomial(),
            );
            if !has_strict || halfspace.is_strict_inequality() {
                wrapped_polynomial.add_term(1.0, VariableWrapper::EXTRA_VARIABLE);
            }

            wrapped_restrictions.push(Relation::new(
                wrapped_polynomial,
                RelationSign::LE,
                Polynomial::default(),
            ));
        }

        let mut pol = Polynomial::<VariableWrapper<V>, Constant>::default();
        pol.add_term(1.0, VariableWrapper::EXTRA_VARIABLE);

        let solution = Self::solve(Problem {
            variables: wrapped_variables,
            domains: domains.into(),
            restrictions: wrapped_restrictions,
            goal: Goal::Maximize(pol),
        });

        if solution.is_err() {
            return match solution.err().unwrap() {
                SolverError::Unbounded => Ok(false),
                SolverError::Infeasible => Ok(true),
                // cant throw InvalidRelationType or ForeignVariable, both are properly handled
                // above
                _ => unreachable!(),
            };
        } else {
            if has_strict {
                // if slack is 0.0, the polyhedron is empty
                let slack = solution
                    .unwrap()
                    .into_iter()
                    .find(|(var, _)| var.is_extra_variable())
                    .unwrap()
                    .1;
                Ok(slack == 0.0.into())
            } else {
                // since theres no strict inequalities, any solution indicates that the polyhedron
                // is not empty
                Ok(false)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SolverError<V: Variable> {
    InvalidRelationType(Relation<V, Constant>),
    ForeignVariable(V),
    Unbounded,
    Infeasible,
}

impl<V: Variable> std::fmt::Display for SolverError<V>
where
    V: Display,
    Relation<V, Constant>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidRelationType(rel) => {
                write!(
                    f,
                    "{rel} is invalid, only equations and nonstrict inequalities are allowed"
                )
            }
            Self::ForeignVariable(var) => write!(f, "{var} is not in variable set"),
            Self::Unbounded => write!(f, "objective function is unbounded"),
            Self::Infeasible => write!(f, "given problem is infeasible"),
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

#[cfg(test)]
mod tests {
    mod is_empty {

        use crate::{
            program_variables,
            pts::variable::program_variable::ProgramVariables,
            ranking_function_generators::linear_solvers::{minilp::Minilp, Solver, SolverError},
            state_relation, state_system,
        };

        #[test]
        fn unrestricted() {
            assert!(!Minilp::is_empty(&state_system!(panic!(); ">=", 0.0)).unwrap());
        }

        #[test]
        fn trivially_infeasible() {
            assert!(Minilp::is_empty(&state_system!(panic!(); ">", 0.0)).unwrap());
        }

        #[test]
        fn empty_system() {
            assert!(Minilp::is_empty(&state_system!()).unwrap());
        }

        #[test]
        fn strict_empty() {
            let mut variables = program_variables!();
            let polyhedron = state_system!(
                        &mut variables;
                        "<=", 0.0, 1.0, "a", -1.0, "b"; // a <= b
                        ">", 0.0, 1.0, "a"; // a > 0
                        "<", 0.0, 1.0, "b");
            assert_eq!(variables, program_variables!("a", "b"));
            assert!(Minilp::is_empty(&polyhedron) // b < 0
                .unwrap());
        }

        #[test]
        fn strict_nonempty() {
            let mut variables: ProgramVariables = program_variables!("a", "b");
            let polyhedron = state_system!(
                &mut variables;
                ">=", 0.0, 1.0, "a", -1.0, "b"; // a >= b
                ">", -10.0, 1.0, "a"; // a > 10
                "<", 0.0, 1.0, "b" // b < 0
            );

            assert_eq!(variables, program_variables!("a", "b"));
            assert!(!Minilp::is_empty(&polyhedron).unwrap());
        }

        #[test]
        fn nonstrict_empty() {
            let mut variables = program_variables!();
            let polyhedron = state_system!(
                        &mut variables;
                        "<=", 0.0, 1.0, "a", -1.0, "b"; // a <= b
                        ">=", 0.0, 1.0, "a"; // a >= 0
                        "<=", 1.0, 1.0, "b");
            assert_eq!(variables, program_variables!("a", "b"));
            assert!(Minilp::is_empty(&polyhedron) // b <= -1
                .unwrap());
        }

        #[test]
        fn nonstrict_nonempty() {
            let mut variables = program_variables!();
            let polyhedron = state_system!(
                &mut variables;
                    ">=", 0.0, 1.0, "a", -1.0, "b"; // a >= b
                    ">=", -10.0, 1.0, "a"; // a >= 10
                    "<=", 0.0, 1.0, "b" // b <= 0
            );
            assert_eq!(variables, program_variables!("a", "b"));
            assert!(!Minilp::is_empty(&polyhedron).unwrap());
        }

        #[test]
        fn invalid_polyhedron() {
            let mut variables = program_variables!();
            let lhs = Minilp::is_empty(&state_system!(
                &mut variables;
                    ">=", 0.0, 1.0, "a", -1.0, "b"; // a >= b
                    "==", -10.0, 1.0, "a"; // a >= 10
                    "<=", 0.0, 1.0, "b" // b <= 0
            ))
            .err()
            .unwrap();
            let rhs = SolverError::InvalidRelationType(state_relation!(
                "==",
                -10.0,
                &mut variables,
                1.0,
                "a"
            ));
            assert_eq!(variables, program_variables!("a", "b"));
            assert_eq!(lhs, rhs);
        }
    }
}
