pub mod constant;
pub mod term;
use crate::pts::variable_map::{VariableError, VariableMap};

use constant::Constant;
use std::{
    iter::zip,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
};
use term::Term;

use super::DisplayLabel;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub struct LinearPolynomial {
    coefficients: Vec<Constant>,
}

impl<'a> LinearPolynomial {
    pub fn len(&self) -> usize {
        self.coefficients.len()
    }

    fn resize_to_map(&mut self, map: &VariableMap) {
        if self.len() < map.len() + 1 {
            // +1 for constant term
            self.coefficients.resize(map.len() + 1, Constant(0.0));
        }
    }

    fn resize_to_polynomial(&mut self, other: &mut Self) {
        if self.len() < other.len() {
            self.coefficients.resize(other.len(), Constant(0.0));
        } else if self.len() > other.len() {
            other
                .coefficients
                .resize(self.coefficients.len(), Constant(0.0));
        }
    }

    pub fn try_add_term(&mut self, map: &VariableMap, term: Term) -> Result<(), VariableError> {
        self.resize_to_map(map);
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            Ok(())
        } else {
            let index = map.get_index(&term.variable);
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
            self.resize_to_map(map);
        } else {
            let index = map.get_or_push(&term.variable);
            self.resize_to_map(map);
            self.coefficients[index] += term.coefficient;
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<Constant> {
        self.coefficients.get(index).map(|x| x.clone())
    }

    pub fn separate_constant_term(&mut self) -> LinearPolynomial {
        let mut new_pol = LinearPolynomial {
            coefficients: vec![Constant(0.0)],
        };

        unsafe {
            // we assume all polynomials to have atleast the constant term
            assert!(self.coefficients.len() > 0);
            assert!(new_pol.coefficients.len() > 0);
            std::mem::swap(
                self.coefficients.get_unchecked_mut(0),
                new_pol.coefficients.get_unchecked_mut(0),
            );
        }
        new_pol
    }

    pub fn iter(&'a self, variable_map: &'a VariableMap) -> TermIterator<'a> {
        TermIterator(self, variable_map, 0)
    }
}

pub struct TermIterator<'a>(&'a LinearPolynomial, &'a VariableMap, usize);

impl<'a> Iterator for TermIterator<'a> {
    type Item = Term;
    fn next(&mut self) -> Option<Self::Item> {
        let variable = self.1.get_variable(self.2);
        let coefficient = self.0.get_coefficient(self.2);
        self.2 += 1;
        match (variable, coefficient) {
            (Some(v), Some(c)) => Some(Term {
                variable: v.map(|x| x.clone()),
                coefficient: c,
            }),
            _ => None,
        }
    }
}

impl DisplayLabel for LinearPolynomial {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = String::default();
        let mut iter = self
            .iter(variable_map)
            .skip(1) // skip constant term
            .chain(self.iter(variable_map).take(1))
            .filter(|x| x.coefficient != Constant(0.0));

        let first_term = iter.next();
        if first_term.is_some() {
            label.push_str(first_term.unwrap().to_string().as_str());
        }

        for term in iter {
            // is_negative_sign returns true on -0.0, so I dont use it
            // xor, cause double negative is positive
            if term.coefficient < Constant(0.0) {
                label.push_str(" - ");
                label.push_str((-term).to_string().as_str());
            } else {
                label.push_str(" + ");
                label.push_str(term.to_string().as_str());
            }
        }

        // theres no non-zero coefficients
        if label.is_empty() {
            label.push_str("0");
        }

        label
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
    fn add_assign(&mut self, mut other: Self) {
        self.resize_to_polynomial(&mut other);
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
    fn sub_assign(&mut self, mut other: Self) {
        self.resize_to_polynomial(&mut other);
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
impl LinearPolynomial {
    pub fn mock(coefficients: Vec<Constant>) -> Self {
        assert!(coefficients.len() > 0);
        LinearPolynomial { coefficients }
    }
}

#[cfg(test)]
mod tests {
    use super::{Constant, LinearPolynomial, Term, VariableError};
    use crate::misc::{setup_test_map, setup_test_polynomial};
    use crate::pts::variable_map::{Variable, VariableMap};
    use crate::pts::DisplayLabel;

    #[test]
    fn add_resizing() {
        let mut pol = LinearPolynomial::default();
        assert_eq!(pol.len(), 1);
        let mut map = setup_test_map();
        let var = Option::<&Variable>::cloned(map.get_variable(1).unwrap());
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
                    variable: Option::<&Variable>::cloned(map.get_variable(1).unwrap()),
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

    #[test]
    fn separate_constant_term() {
        let mut map = setup_test_map();
        let mut lhs = setup_test_polynomial(
            &mut map,
            Constant(9.0),
            Constant(-3.0),
            Constant(4.0),
            Constant(-5.0),
        );
        let rhs = lhs.separate_constant_term();
        assert_eq!(
            lhs,
            setup_test_polynomial(
                &mut map,
                Constant(0.0),
                Constant(-3.0),
                Constant(4.0),
                Constant(-5.0),
            )
        );
        assert_eq!(
            rhs,
            LinearPolynomial {
                coefficients: vec![Constant(9.0)]
            }
        );
    }

    #[test]
    fn label() {
        let pol = LinearPolynomial {
            coefficients: vec![Constant(0.0)],
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(pol.label(&map), "0");

        let pol = LinearPolynomial {
            coefficients: vec![Constant(-5.0)],
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(pol.label(&map), "-5");

        let pol = LinearPolynomial {
            coefficients: vec![Constant(0.0), Constant(1.0)],
        };
        let map = VariableMap::mock(vec![Variable::new("test")]);
        assert_eq!(pol.label(&map), "test");

        let pol = LinearPolynomial {
            coefficients: vec![Constant(-5.0), Constant(-1.0), Constant(0.0), Constant(2.0)],
        };
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(pol.label(&map), "-a + 2c - 5");

        let pol = LinearPolynomial {
            coefficients: vec![Constant(-5.0), Constant(0.0), Constant(0.0), Constant(0.0)],
        };
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(pol.label(&map), "-5");
    }
}
