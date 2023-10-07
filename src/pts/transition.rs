use crate::pts::{linear_polynomial, location, variable_map};
use linear_polynomial::LinearPolynomial;
use location::LocationHandle;
use variable_map::Variable;

use super::{variable_map::VariableMap, DisplayLabel};

#[macro_export]
macro_rules! assignment {
    [ $var:expr, $( $x:expr ),* $(,)?] => {
        {
            $crate::pts::transition::Assignment($crate::pts::variable_map::Variable::new($var), $crate::mock_polynomial![$($x, )*])
        }
    };
}

#[macro_export]
macro_rules! transition {
    [ $tar:expr $(; $( $var:expr, $($x:expr),* );*)? ] => {
        {
            $crate::pts::transition::Transition{ target: $tar, assignments: std::vec![$($($crate::assignment![$var, $($x, )*], )*)? ] }
        }
    };
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct Assignment(pub Variable, pub LinearPolynomial);

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct Transition {
    pub assignments: Vec<Assignment>,
    pub target: LocationHandle,
}

impl DisplayLabel for Assignment {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = self.0.to_string();
        label.push_str(" = ");
        label.push_str(self.1.label(variable_map).as_str());

        label
    }
}

#[cfg(test)]
mod tests {

    mod macros {
        use crate::{
            mock_polynomial,
            pts::{transition::Assignment, variable_map::Variable},
        };

        #[test]
        fn assignment() {
            assert_eq!(
                Assignment(
                    Variable::new("test"),
                    mock_polynomial!(6.8, -0.0, -6.8, 134.689, 0.0)
                ),
                assignment!("test", 6.8, -0.0, -6.8, 134.689, 0.0)
            );
        }

        mod transition {
            use crate::pts::transition::Transition;

            #[test]
            fn some() {
                assert_eq!(
                    Transition {
                        target: Some(1999),
                        assignments: vec![
                            assignment!("br", -0.3, 5.0, -0.0, 0.0),
                            assignment!("test", 1234.5, 1111.0, 15.151515)
                        ]
                    },
                    transition!(Some(1999); "br", -0.3, 5.0, -0.0, 0.0; "test", 1234.5, 1111.0, 15.151515)
                );
            }

            #[test]
            fn none() {
                assert_eq!(
                    Transition {
                        target: None,
                        assignments: vec![
                            assignment!("a", 0.3, -5.0, 0.0, 0.0),
                            assignment!("r", 0.0, -1.0, 15.15)
                        ]
                    },
                    transition!(None; "a", 0.3, -5.0, 0.0, 0.0; "r", 0.0, -1.0, 15.15)
                );
            }
        }
    }
    mod label {
        use crate::{mock_varmap, pts::DisplayLabel};

        #[test]
        fn one_var() {
            let map = mock_varmap!();
            assert_eq!(assignment!("test", 0.0).label(&map), "test = 0");
        }

        #[test]
        fn many_vars() {
            let map = mock_varmap!("a", "b");
            assert_eq!(
                assignment!("b", -5.0, -2.0, 1.0).label(&map),
                "b = -2a + b - 5"
            );
        }
    }
}
