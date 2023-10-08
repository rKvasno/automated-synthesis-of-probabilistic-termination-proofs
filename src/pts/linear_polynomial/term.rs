use crate::pts::{linear_polynomial, variable_map};
use linear_polynomial::constant::Constant;
use variable_map::Variable;

use std::{fmt, ops::Neg};

#[macro_export]
macro_rules! term {
    [ $constant:expr, $var:expr ] => {
        {
            $crate::pts::linear_polynomial::term::Term{variable: Some($crate::pts::variable_map::Variable::new($var)), coefficient: $crate::pts::linear_polynomial::constant::Constant($constant)}
        }
    };
    [ $constant:expr ] => {
        {
            $crate::pts::linear_polynomial::term::Term{variable: None, coefficient: $crate::pts::linear_polynomial::constant::Constant($constant)}
        }
    };
}

// default: constant term 0
#[derive(Debug, Default, PartialEq)]
pub struct Term {
    // no variable => constant term
    pub variable: Option<Variable>,
    pub coefficient: Constant,
}

impl Term {
    pub fn is_constant(&self) -> bool {
        self.variable.is_none()
    }
    pub fn is_variable(&self) -> bool {
        self.variable.is_some()
    }
}

impl Neg for Term {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            variable: self.variable,
            coefficient: -self.coefficient,
        }
    }
}

impl Neg for &mut Term {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.coefficient = -self.coefficient;
        self
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if self.variable.is_none() {
            // Constant term
            if self.coefficient == Constant(0.0) {
                write!(f, "0",)
            } else {
                write!(f, "{}", self.coefficient.to_string(),)
            }
        } else {
            // Linear term

            if self.coefficient == Constant(1.0) {
                write!(
                    f,
                    "{}",
                    self.variable.as_ref().map(|x| x.as_str()).unwrap_or(""),
                )
            } else if self.coefficient == Constant(-1.0) {
                write!(
                    f,
                    "-{}",
                    self.variable.as_ref().map(|x| x.as_str()).unwrap_or(""),
                )
            } else if self.coefficient == Constant(0.0) {
                write!(f, "",)
            } else {
                write!(
                    f,
                    "{}{}",
                    self.coefficient.to_string(),
                    self.variable.as_ref().map(|x| x.as_str()).unwrap_or(""),
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    mod macros {
        use crate::pts::{
            linear_polynomial::{constant::Constant, term::Term},
            variable_map::Variable,
        };

        #[test]
        fn coeff_var() {
            assert_eq!(
                term!(40.0, "test"),
                Term {
                    variable: Some(Variable::new("test")),
                    coefficient: Constant(40.0)
                }
            );
        }

        #[test]
        fn constant() {
            assert_eq!(
                term!(-45.3),
                Term {
                    variable: None,
                    coefficient: Constant(-45.3)
                }
            );
        }
    }

    mod fmt {
        #[test]
        fn coeff_var() {
            assert_eq!(term!(50.0, "Test").to_string(), "50Test");
        }

        #[test]
        fn neg_coeff_var() {
            assert_eq!(term!(-0.5, "a").to_string(), "-0.5a");
        }

        #[test]
        fn neg_var() {
            assert_eq!(term!(-1.0, "a").to_string(), "-a");
        }

        #[test]
        fn pos_var() {
            assert_eq!(term!(1.0, "a").to_string(), "a");
        }

        #[test]
        fn zero_coeff() {
            assert_eq!(term!(0.0, "test").to_string(), "");
        }

        #[test]
        fn neg_zero_coeff() {
            assert_eq!(term!(-0.0, "test").to_string(), "");
        }

        #[test]
        fn zero() {
            assert_eq!(term!(0.0).to_string(), "0");
        }

        #[test]
        fn neg_zero() {
            assert_eq!(term!(-0.0).to_string(), "0");
        }

        #[test]
        fn neg_const() {
            assert_eq!(term!(-5.0).to_string(), "-5");
        }

        #[test]
        fn neg_one() {
            assert_eq!(term!(-1.0).to_string(), "-1");
        }

        #[test]
        fn one() {
            assert_eq!(term!(1.0).to_string(), "1");
        }
    }
}
