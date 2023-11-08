use std::fmt::Display;

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
                $crate::pts::variable::Variable::new($varset, $assign_var),
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
macro_rules! transition {
    [
        $tar:expr
        $(
            ,
            $varset:expr
            $(
                ;
                $assign_var:expr,
                $constant:expr
                $(
                    ,
                    $coeff:expr,
                    $var:expr
                )*
            )*
            $(;)?
        )?
    ] => {
        {
            $crate::pts::transition::Transition{
                target: $tar,
                assignments: std::vec!
                [
                    $(
                        $(
                            $crate::assignment![
                                $varset,
                                $assign_var,
                                $constant
                                $(
                                    ,
                                    $coeff,
                                    $var
                                )*
                            ],
                        )*
                    )?
                ]
            }
        }
    };
}
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Transition {
    pub target: LocationHandle,
    pub assignments: Vec<StateAssignment>,
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
            pts::{
                transition::Assignment,
                variable::{
                    program_variable::{ProgramVariable, ProgramVariables},
                    Variable,
                },
            },
            state, variables,
        };

        #[test]
        fn assignment() {
            let mut variables: ProgramVariables = variables!("test");
            assert_eq!(
                Assignment::new(
                    ProgramVariable::new(&mut variables, "test"),
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
                ),
                assignment!(
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
            assert_eq!(variables, variables!("test", "a", "b", "c", "d"))
        }
    }

    mod assignment {
        use crate::{pts::variable::program_variable::ProgramVariables, state, variables};

        #[test]
        fn apply() {
            let mut variables: ProgramVariables = variables!();
            assert_eq!(
                assignment!(
                    &mut variables,
                    "a",
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
                        assignments: vec![
                            assignment!(&mut variables, "b", -0.3, 5.0, "a", -0.0, "b", 0.0, "c"),
                            assignment!(&mut variables, "a", 1234.5, 1111.0, "a", 15.151515, "b")
                        ]
                    },
                    transition!(Some(1234), &mut variables; "b", -0.3, 5.0, "a", -0.0, "b", 0.0, "c"; "a", 1234.5, 1111.0, "a", 15.151515, "b")
                );
            }

            #[test]
            fn none() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    Transition {
                        target: None,
                        assignments: vec![
                            assignment!(&mut variables, "b", 0.3, -5.0, "a", 0.0, "b", 0.0, "c"),
                            assignment!(&mut variables, "a", 0.0, -1.0, "a", 15.15, "b")
                        ]
                    },
                    transition!(None, &mut variables; "b", 0.3, -5.0, "a", 0.0, "b", 0.0, "c"; "a", 0.0, -1.0, "a", 15.15, "b")
                );
            }
        }
    }
    mod label {
        use crate::{pts::variable::program_variable::ProgramVariables, variables};

        #[test]
        fn one_var() {
            let mut variables: ProgramVariables = variables!("test");
            assert_eq!(
                assignment!(&mut variables, "test", 0.0).to_string(),
                "test = 0"
            );
        }

        #[test]
        fn many_vars() {
            let mut variables: ProgramVariables = variables!();
            assert_eq!(
                assignment!(&mut variables, "b", -5.0, -2.0, "a", 1.0, "b").to_string(),
                "b = -2a + b - 5"
            );
        }
    }
}
