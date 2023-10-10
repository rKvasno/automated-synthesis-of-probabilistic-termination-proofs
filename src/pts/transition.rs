use crate::pts::{linear_polynomial, location};
use linear_polynomial::LinearPolynomial;
use location::LocationHandle;

use super::{
    variable_map::{Variable, VariableID, VariableMap},
    DisplayLabel,
};

#[cfg(test)]
#[macro_export]
macro_rules! mock_assignment {
    [ $id:expr, $( $x:expr ),* $(,)?] => {
        {
            $crate::pts::transition::Assignment::mock($id, $crate::mock_polynomial![$($x, )*])
        }
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! mock_transition {
    [ $tar:expr $(; $( $id:expr, $($x:expr),* );*)? ] => {
        {
            $crate::pts::transition::Transition{ target: $tar, assignments: std::vec![$($($crate::mock_assignment![$id, $($x, )*], )*)? ] }
        }
    };
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct Assignment(pub VariableID, pub LinearPolynomial);

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct Transition {
    pub assignments: Vec<Assignment>,
    pub target: LocationHandle,
}

impl Assignment {
    pub fn new(variables: &mut VariableMap, name: &Variable, value: LinearPolynomial) -> Self {
        Assignment(variables.get_or_push(name), value)
    }

    pub fn apply(&self, mut pol: LinearPolynomial) -> LinearPolynomial {
        // we assume valid variableID in the assignment
        let mut substitution = self.1.clone();
        substitution.mul_by_constant(pol.get_coefficient(self.0).unwrap().clone());
        pol.get_mut_coefficient(self.0).unwrap().0 = 0.0;

        pol + substitution
    }

    #[cfg(test)]
    pub fn mock(id: VariableID, value: LinearPolynomial) -> Self {
        Assignment(id, value)
    }
}

impl DisplayLabel for Assignment {
    fn label(&self, variable_map: &VariableMap) -> String {
        // LHS of assignment should be a valid VariableID => unwrap
        let mut label = variable_map.get_variable(self.0).unwrap().to_string();
        label.push_str(" = ");
        label.push_str(self.1.label(variable_map).as_str());

        label
    }
}

#[cfg(test)]
mod tests {

    mod macros {

        mod assignment {
            use crate::{
                mock_polynomial, mock_varmap,
                pts::{transition::Assignment, variable_map::Variable},
            };

            #[test]
            fn new() {
                let mut map = mock_varmap!("test");
                assert_eq!(
                    Assignment::new(
                        &mut map,
                        &Variable::new("test"),
                        mock_polynomial!(6.8, -0.0, -6.8, 134.689, 0.0)
                    ),
                    mock_assignment!(1, 6.8, -0.0, -6.8, 134.689, 0.0)
                );
                assert_eq!(map, mock_varmap!("test"))
            }

            #[test]
            fn apply() {
                assert_eq!(
                    mock_assignment!(1, 6.8, -0.0, -6.8, 134.689, 0.0)
                        .apply(mock_polynomial!(0.0, 1.0)),
                    mock_polynomial!(6.8, -0.0, -6.8, 134.689, 0.0)
                )
            }
        }

        mod transition {
            use crate::pts::transition::Transition;

            #[test]
            fn some() {
                assert_eq!(
                    Transition {
                        target: Some(1999),
                        assignments: vec![
                            mock_assignment!(2, -0.3, 5.0, -0.0, 0.0),
                            mock_assignment!(1, 1234.5, 1111.0, 15.151515)
                        ]
                    },
                    mock_transition!(Some(1999); 2, -0.3, 5.0, -0.0, 0.0; 1, 1234.5, 1111.0, 15.151515)
                );
            }

            #[test]
            fn none() {
                assert_eq!(
                    Transition {
                        target: None,
                        assignments: vec![
                            mock_assignment!(2, 0.3, -5.0, 0.0, 0.0),
                            mock_assignment!(1, 0.0, -1.0, 15.15)
                        ]
                    },
                    mock_transition!(None; 2, 0.3, -5.0, 0.0, 0.0; 1, 0.0, -1.0, 15.15)
                );
            }
        }
    }
    mod label {
        use crate::{mock_varmap, pts::DisplayLabel};

        #[test]
        fn one_var() {
            let map = mock_varmap!("test");
            assert_eq!(mock_assignment!(1, 0.0).label(&map), "test = 0");
        }

        #[test]
        fn many_vars() {
            let map = mock_varmap!("a", "b");
            assert_eq!(
                mock_assignment!(2, -5.0, -2.0, 1.0).label(&map),
                "b = -2a + b - 5"
            );
        }
    }
}
