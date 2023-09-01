use crate::pts::{variable_map, linear_polynomial};
use variable_map::Variable;
use linear_polynomial::constant::Constant;

use std::ops::Neg;

// default: constant term 0
#[derive(Debug, Default, PartialEq)]
pub struct Term {
    pub variable: Option<Variable>,
    pub coefficient: Constant
}

impl Neg for Term {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self{ variable: self.variable, coefficient: -self.coefficient }
    }
}

impl Neg for &mut Term {
    type Output = Self;

    fn neg(self) -> Self::Output {
        self.coefficient = -self.coefficient;
        self
    }
}

