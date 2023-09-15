use crate::pts::{linear_polynomial, variable_map};
use linear_polynomial::constant::Constant;
use variable_map::Variable;

use std::{fmt, ops::Neg};

// default: constant term 0
#[derive(Debug, Default, PartialEq)]
pub struct Term {
    pub variable: Option<Variable>,
    pub coefficient: Constant,
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
    use crate::pts::{linear_polynomial::constant::Constant, variable_map::Variable};

    use super::Term;

    #[test]
    fn fmt() {
        let term = Term {
            variable: Some(Variable::new("Test")),
            coefficient: Constant(50.0),
        };
        assert_eq!(term.to_string(), "50Test");

        let term = Term {
            variable: Some(Variable::new("a")),
            coefficient: Constant(-0.5),
        };
        assert_eq!(term.to_string(), "-0.5a");

        let term = Term {
            variable: Some(Variable::new("a")),
            coefficient: Constant(-1.0),
        };
        assert_eq!(term.to_string(), "-a");

        let term = Term {
            variable: Some(Variable::new("a")),
            coefficient: Constant(1.0),
        };
        assert_eq!(term.to_string(), "a");

        let term = Term {
            variable: Some(Variable::new("a")),
            coefficient: Constant(0.0),
        };
        assert_eq!(term.to_string(), "");

        let term = Term {
            variable: None,
            coefficient: Constant(0.0),
        };
        assert_eq!(term.to_string(), "0");

        let term = Term {
            variable: None,
            coefficient: Constant(-0.0),
        };
        assert_eq!(term.to_string(), "0");

        let term = Term {
            variable: Some(Variable::new("a")),
            coefficient: Constant(-0.0),
        };
        assert_eq!(term.to_string(), "");

        let term = Term {
            variable: None,
            coefficient: Constant(-5.0),
        };
        assert_eq!(term.to_string(), "-5");

        let term = Term {
            variable: None,
            coefficient: Constant(-1.0),
        };
        assert_eq!(term.to_string(), "-1");

        let term = Term {
            variable: None,
            coefficient: Constant(1.0),
        };
        assert_eq!(term.to_string(), "1");
    }
}
