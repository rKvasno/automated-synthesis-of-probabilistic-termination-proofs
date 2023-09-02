pub mod constant;
pub mod term;
use crate::pts::variable_map::{VariableError, VariableMap};

use constant::Constant;
use std::{
    iter::zip,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
};
use term::Term;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub struct LinearPolynomial {
    coefficients: Vec<Constant>,
}

impl LinearPolynomial {
    pub fn test() -> Self {
        Default::default()
    }
}

impl LinearPolynomial {
    pub fn len(&self) -> usize {
        self.coefficients.len()
    }

    fn resize(&mut self, map: &VariableMap) {
        if self.len() < map.len() + 1 {
            // +1 for constant term
            self.coefficients.resize(map.len() + 1, Constant(0.0));
        }
    }

    pub fn try_add_term(&mut self, map: &VariableMap, term: Term) -> Result<(), VariableError> {
        self.resize(map);
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            Ok(())
        } else {
            let index = map.get_index(term.variable.as_ref().unwrap());
            // if variable is not in the map, its not a program variable
            if index.is_none() {
                Err(VariableError::new(term.variable.as_ref().unwrap()))
            } else {
                self.coefficients[index.unwrap()] += term.coefficient;
                Ok(())
            }
        }
    }

    pub fn add_term(&mut self, map: &mut VariableMap, term: Term) {
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            self.resize(map);
        } else {
            let index = map.find_or_add(term.variable.unwrap());
            self.resize(map);
            self.coefficients[index] += term.coefficient;
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<Constant> {
        self.coefficients.get(index).map(|x| x.clone())
    }
}

#[cfg(test)]
impl LinearPolynomial {
    pub fn mock(coefficients: Vec<Constant>) -> Self {
        LinearPolynomial { coefficients }
    }
}

impl Default for LinearPolynomial {
    fn default() -> Self {
        LinearPolynomial {
            coefficients: vec![Constant(0.0)],
        }
    }
}

impl AddAssign for LinearPolynomial {
    fn add_assign(&mut self, other: Self) {
        let iter = zip(self.coefficients.iter_mut(), other.coefficients.into_iter());
        for (lhs, rhs) in iter {
            *lhs += rhs;
        }
    }
}

impl Add for LinearPolynomial {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        self += other;
        self
    }
}

impl SubAssign for LinearPolynomial {
    fn sub_assign(&mut self, other: Self) {
        let iter = zip(self.coefficients.iter_mut(), other.coefficients.into_iter());
        for (lhs, rhs) in iter {
            *lhs -= rhs;
        }
    }
}

impl Sub for LinearPolynomial {
    type Output = Self;

    fn sub(mut self, other: Self) -> Self::Output {
        self -= other;
        self
    }
}

impl Neg for LinearPolynomial {
    type Output = Self;

    fn neg(self) -> Self::Output {
        LinearPolynomial {
            coefficients: self.coefficients.into_iter().map(|x| -x).collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Constant, LinearPolynomial, Term, VariableError};
    use crate::misc::{setup_test_map, setup_test_polynomial};
    use crate::pts::variable_map::Variable;

    #[test]
    fn add_resizing() {
        let mut pol = LinearPolynomial::default();
        assert_eq!(pol.len(), 1);
        let mut map = setup_test_map();
        let var = Option::<&Variable>::cloned(map.get_variable(1));
        pol.add_term(
            &mut map,
            Term {
                variable: var,
                coefficient: Constant(0.0),
            },
        );
        assert_eq!(pol.len(), map.len() + 1);
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn add_variable() {
        let mut pol = LinearPolynomial::default();
        let mut map = setup_test_map();
        let b = Variable::new("b");
        pol.add_term(
            &mut map,
            Term {
                variable: Some(b.clone()),
                coefficient: Constant(1.0),
            },
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(1.0), Constant(0.0))
            }
        );
        pol.add_term(
            &mut map,
            Term {
                variable: Some(b),
                coefficient: Constant(1.0),
            },
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(2.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn add_constant() {
        let mut pol = LinearPolynomial::default();
        let mut map = setup_test_map();
        pol.add_term(
            &mut map,
            Term {
                variable: None,
                coefficient: Constant(1.0),
            },
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(1.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
        pol.add_term(
            &mut map,
            Term {
                variable: None,
                coefficient: Constant(1.0),
            },
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(2.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn add_out_of_bounds() {
        let mut pol = LinearPolynomial::default();
        let mut map = setup_test_map();
        let e = Variable::new("e");
        pol.add_term(
            &mut map,
            Term {
                variable: Some(e),
                coefficient: Constant(1.0),
            },
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(1.0)
                )
            }
        );
    }

    #[test]
    fn try_add_resizing() {
        let mut pol = LinearPolynomial::default();
        assert_eq!(pol.len(), 1);
        let map = setup_test_map();
        assert_eq!(
            pol.try_add_term(
                &map,
                Term {
                    variable: Option::<&Variable>::cloned(map.get_variable(1)),
                    coefficient: Constant(0.0)
                }
            ),
            Ok(())
        );
        assert_eq!(pol.len(), map.len() + 1);
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn try_add_variable() {
        let mut pol = LinearPolynomial::default();
        let map = setup_test_map();
        let b = Variable::new("b");
        pol.try_add_term(
            &map,
            Term {
                variable: Some(b.clone()),
                coefficient: Constant(1.0),
            },
        )
        .unwrap();
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(1.0), Constant(0.0))
            }
        );
        pol.try_add_term(
            &map,
            Term {
                variable: Some(b),
                coefficient: Constant(1.0),
            },
        )
        .unwrap();
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(2.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn try_add_constant() {
        let mut pol = LinearPolynomial::default();
        let map = setup_test_map();
        pol.try_add_term(
            &map,
            Term {
                variable: None,
                coefficient: Constant(1.0),
            },
        )
        .unwrap();
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(1.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
        pol.try_add_term(
            &map,
            Term {
                variable: None,
                coefficient: Constant(1.0),
            },
        )
        .unwrap();
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(2.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn try_add_out_of_bounds() {
        let mut pol = LinearPolynomial::default();
        let map = setup_test_map();
        let e = Variable::new("e");
        assert_eq!(
            pol.try_add_term(
                &map,
                Term {
                    variable: Some(e.clone()),
                    coefficient: Constant(1.0)
                }
            ),
            Err(VariableError::new(&e))
        );
        assert_eq!(
            &pol,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(0.0), Constant(0.0), Constant(0.0))
            }
        );
    }

    #[test]
    fn arithmetic_add() {
        let mut map = setup_test_map();
        let mut lhs = setup_test_polynomial(
            &mut map,
            Constant(1.0),
            Constant(2.0),
            Constant(3.0),
            Constant(4.0),
        );
        let rhs = setup_test_polynomial(
            &mut map,
            Constant(4.0),
            Constant(3.0),
            Constant(2.0),
            Constant(1.0),
        );
        let sum = lhs.clone() + rhs.clone();
        assert_eq!(
            &sum,
            &LinearPolynomial {
                coefficients: vec!(Constant(5.0), Constant(5.0), Constant(5.0), Constant(5.0))
            }
        );
        lhs += rhs;
        assert_eq!(
            &lhs,
            &LinearPolynomial {
                coefficients: vec!(Constant(5.0), Constant(5.0), Constant(5.0), Constant(5.0))
            }
        );
    }

    #[test]
    fn arithmetic_sub() {
        let mut map = setup_test_map();
        let mut lhs = setup_test_polynomial(
            &mut map,
            Constant(2.0),
            Constant(3.0),
            Constant(4.0),
            Constant(5.0),
        );
        let rhs = setup_test_polynomial(
            &mut map,
            Constant(1.0),
            Constant(1.0),
            Constant(1.0),
            Constant(1.0),
        );
        let diff = lhs.clone() - rhs.clone();
        assert_eq!(
            &diff,
            &LinearPolynomial {
                coefficients: vec!(Constant(1.0), Constant(2.0), Constant(3.0), Constant(4.0))
            }
        );
        lhs -= rhs;
        assert_eq!(
            &lhs,
            &LinearPolynomial {
                coefficients: vec!(Constant(1.0), Constant(2.0), Constant(3.0), Constant(4.0))
            }
        );
    }

    #[test]
    fn neg() {
        let mut map = setup_test_map();
        let lhs = setup_test_polynomial(
            &mut map,
            Constant(0.0),
            Constant(-3.0),
            Constant(4.0),
            Constant(-5.0),
        );
        let lhs = -lhs;
        assert_eq!(
            &lhs,
            &LinearPolynomial {
                coefficients: vec!(Constant(0.0), Constant(3.0), Constant(-4.0), Constant(5.0))
            }
        );
    }
}
