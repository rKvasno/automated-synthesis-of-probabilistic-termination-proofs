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
        write!(
            f,
            "{}{}",
            self.variable.map(|x| x.as_str()).unwrap_or(""),
            self.coefficient
        )
    }
}
