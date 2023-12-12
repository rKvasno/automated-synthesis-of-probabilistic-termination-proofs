use std::fmt::Display;

use crate::polynomial;

use super::{
    linear_polynomial::{
        coefficient::{Coefficient, Constant},
        Polynomial,
    },
    location::LocationHandle,
    variable::{program_variable::ProgramVariable, Variable},
};

#[macro_export]
macro_rules! assignment {
    [
        $varset:expr,
        $assign_var:expr,
        $constant:expr
        $(
            ,
            $coeff:expr,
            $var:expr
        )*
        $(,)?
    ] => {
        {
            $crate::pts::transition::Assignment::new(
                $crate::var!($varset, $assign_var),
                $crate::polynomial![
                    $constant,
                    $varset
                    $(
                        ,
                        $coeff,
                        $var
                    )*
                ]
            )
        }
    };
}
#[macro_export]
macro_rules! state_assignment {
    [
        $varset:expr,
        $assign_var:expr,
        $constant:expr
        $(
            ,
            $coeff:expr,
            $var:expr
        )*
        $(,)?
    ] => {
        $crate::pts::transition::UpdateOperation::Assignment(
            $crate::assignment![
                $varset,
                std::rc::Rc::from($assign_var),
                $constant
                $(
                    ,
                    $coeff,
                    std::rc::Rc::from($var)
                )*
            ]
        )
    }
}

#[macro_export]
macro_rules! sampling {
    [
        $varset:expr,
        $target:expr,
        $min:expr,
        $max:expr,
        $ex:expr
        $(,)?
    ] => {
        {
            $crate::pts::transition::Sampling::new(
                $crate::var!($varset, $target),
                $min,
                $max,
                $ex
            ).unwrap()
        }
    };
}
#[macro_export]
macro_rules! state_sampling {
    [
        $varset:expr,
        $target:expr,
        $min:expr,
        $max:expr,
        $ex:expr
        $(,)?
    ] => {
        $crate::pts::transition::UpdateOperation::Sampling(
            $crate::sampling![
                $varset,
                std::rc::Rc::from($target),
                $min,
                $max,
                $ex
            ]
        )
    }
}

#[macro_export]
macro_rules! transition {
    [
        $tar:expr
        $(
            ,
            $update_op:expr
            $(,)?
        )*
    ] => {
        {
            $crate::pts::transition::Transition{
                target: $tar,
                update_function: std::vec!
                [
                    $(
                        $update_op,
                    )*
                ]
            }
        }
    };
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Transition {
    pub target: LocationHandle,
    pub update_function: Vec<UpdateOperation>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub enum UpdateOperation {
    Assignment(StateAssignment),
    Sampling(StateSampling),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub struct Sampling<V: Variable> {
    target: V,
    min: Constant,
    max: Constant,
    expected_value: Constant,
}

pub type StateSampling = Sampling<ProgramVariable>;

impl<V: Variable> Sampling<V> {
    pub fn new<C: Into<Constant>>(target: V, min: C, max: C, expected_value: C) -> Option<Self> {
        let min_const: Constant = min.into();
        let max_const: Constant = max.into();
        let expected_value_const: Constant = expected_value.into();
        if min_const <= expected_value_const && expected_value_const <= max_const {
            Some(Sampling {
                target,
                min: min_const.into(),
                max: max_const.into(),
                expected_value: expected_value_const,
            })
        } else {
            None
        }
    }

    pub fn expectation(&self) -> Assignment<V> {
        Assignment(self.target.clone(), polynomial!(self.expected_value))
    }
}

impl std::fmt::Display for UpdateOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UpdateOperation::Assignment(assignment) => std::fmt::Display::fmt(assignment, f),
            UpdateOperation::Sampling(sampling) => std::fmt::Display::fmt(sampling, f),
        }
    }
}

impl<V: Variable + Display> std::fmt::Display for Sampling<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} = random({}, {}, {})",
            self.target, self.min, self.max, self.expected_value
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment<V: Variable>(V, Polynomial<V, Constant>);

pub type StateAssignment = Assignment<ProgramVariable>;

impl<V: Variable> Assignment<V> {
    pub fn new(var: V, value: Polynomial<V, Constant>) -> Self {
        Assignment(var.into(), value)
    }

    pub fn apply<'pol, C: Coefficient>(&self, mut pol: Polynomial<V, C>) -> Polynomial<V, C> {
        let multiplier = pol.remove_term(&Some(self.0.to_owned()));
        if multiplier.is_some() {
            let multiplier = multiplier.unwrap().1;
            for (var, coeff) in self.1.iter() {
                let mut c = multiplier.to_owned();
                c.mul_by_constant(coeff.to_owned());
                pol.add_term(c, var.to_owned());
            }
        }

        pol
    }
}

impl<V: Variable + Display> std::fmt::Display for Assignment<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.0, self.1)
    }
}

#[cfg(test)]
mod tests {

    mod macros {

        use crate::{
            program_var, program_variables,
            pts::{
                transition::{Assignment, StateSampling, UpdateOperation},
                variable::program_variable::ProgramVariables,
            },
            state,
        };

        #[test]
        fn state_assignment() {
            let mut variables: ProgramVariables = program_variables!("test");
            assert_eq!(
                UpdateOperation::Assignment(Assignment::new(
                    program_var!(&mut variables, "test"),
                    state!(
                        6.8,
                        &mut variables,
                        -0.0,
                        "a",
                        -6.8,
                        "b",
                        134.689,
                        "c",
                        0.0,
                        "d"
                    )
                )),
                state_assignment!(
                    &mut variables,
                    "test",
                    6.8,
                    -0.0,
                    "a",
                    -6.8,
                    "b",
                    134.689,
                    "c",
                    0.0,
                    "d"
                )
            );
            assert_eq!(variables, program_variables!("test", "a", "b", "c", "d"))
        }

        #[test]
        fn state_sampling() {
            let mut variables: ProgramVariables = program_variables!("test");
            assert_eq!(
                UpdateOperation::Sampling(
                    StateSampling::new(program_var!(&mut variables, "test"), -6.8, 134.689, 0.0)
                        .unwrap()
                ),
                state_sampling!(&mut variables, "test", -6.8, 134.689, 0.0)
            );
            assert_eq!(variables, program_variables!("test"))
        }

        #[test]
        #[should_panic(expected = "None")]
        fn state_sampling_fail() {
            let mut variables: ProgramVariables = program_variables!("test");
            state_sampling!(&mut variables, "test", 1.0, 0.0, 0.0);
        }
    }

    mod assignment {
        use crate::{
            program_var,
            pts::{transition::Assignment, variable::program_variable::ProgramVariables},
            state, variables,
        };

        #[test]
        fn apply() {
            let mut variables: ProgramVariables = variables!();
            assert_eq!(
                Assignment::new(
                    program_var!(&mut variables, "a"),
                    state!(
                        6.8,
                        &mut variables,
                        -0.0,
                        "a",
                        -6.8,
                        "b",
                        134.689,
                        "c",
                        0.0,
                        "d"
                    )
                )
                .apply(state!(0.0, &mut variables, 1.0, "a")),
                state!(
                    6.8,
                    &mut variables,
                    -0.0,
                    "a",
                    -6.8,
                    "b",
                    134.689,
                    "c",
                    0.0,
                    "d"
                )
            )
        }
    }

    mod sampling {
        use crate::{
            program_var,
            pts::{
                transition::{Assignment, StateSampling},
                variable::program_variable::ProgramVariables,
            },
            state, variables,
        };

        #[test]
        fn new() {
            let mut variables: ProgramVariables = variables!();

            assert_eq!(
                StateSampling::new(program_var!(&mut variables, "test"), 0.0, 1.0, 2.0),
                None
            );

            assert_eq!(
                StateSampling::new(program_var!(&mut variables, "test"), 1.0, 10.0, 5.5),
                Some(StateSampling {
                    target: program_var!(&mut variables, "test"),
                    min: 1.0.into(),
                    max: 10.0.into(),
                    expected_value: 5.5.into(),
                })
            );
        }

        #[test]
        fn expected_value() {
            let mut variables: ProgramVariables = variables!();

            assert_eq!(
                StateSampling::new(program_var!(&mut variables, "test"), 1.0, 10.0, 5.5)
                    .unwrap()
                    .expectation(),
                Assignment::new(program_var!(&mut variables, "test"), state!(5.5))
            )
        }
    }

    mod transition {
        use crate::{
            pts::{transition::Transition, variable::program_variable::ProgramVariables},
            variables,
        };

        #[test]
        fn some() {
            let mut variables: ProgramVariables = variables!();
            assert_eq!(
                Transition {
                    target: Some(1234),
                    update_function: vec![
                        state_assignment!(&mut variables, "b", -0.3, 5.0, "a", -0.0, "b", 0.0, "c"),
                        state_assignment!(&mut variables, "a", 1234.5, 1111.0, "a", 15.151515, "b")
                    ]
                },
                transition!(
                    Some(1234),
                    state_assignment!(&mut variables, "b", -0.3, 5.0, "a", -0.0, "b", 0.0, "c"),
                    state_assignment!(&mut variables, "a", 1234.5, 1111.0, "a", 15.151515, "b")
                )
            );
        }

        #[test]
        fn none() {
            let mut variables: ProgramVariables = variables!();
            assert_eq!(
                Transition {
                    target: None,
                    update_function: vec![
                        state_assignment!(&mut variables, "b", 0.3, -5.0, "a", 0.0, "b", 0.0, "c"),
                        state_assignment!(&mut variables, "a", 0.0, -1.0, "a", 15.15, "b")
                    ]
                },
                transition!(
                    None,
                    state_assignment!(&mut variables, "b", 0.3, -5.0, "a", 0.0, "b", 0.0, "c"),
                    state_assignment!(&mut variables, "a", 0.0, -1.0, "a", 15.15, "b")
                )
            );
        }
    }

    mod label {
        use crate::{program_variables, pts::variable::program_variable::ProgramVariables};

        #[test]
        fn one_var_assign() {
            let mut variables: ProgramVariables = program_variables!("test");
            assert_eq!(
                state_assignment!(&mut variables, "test", 0.0).to_string(),
                "test = 0"
            );
        }

        #[test]
        fn sampling() {
            let mut variables: ProgramVariables = program_variables!("test");
            assert_eq!(
                state_sampling!(&mut variables, "test", 0.0, 1.0, 0.5).to_string(),
                "test = random(0, 1, 0.5)"
            );
        }

        #[test]
        fn many_vars_assign() {
            let mut variables: ProgramVariables = program_variables!();
            assert_eq!(
                state_assignment!(&mut variables, "b", -5.0, -2.0, "a", 1.0, "b").to_string(),
                "b = -2a + b - 5"
            );
        }
    }
}
