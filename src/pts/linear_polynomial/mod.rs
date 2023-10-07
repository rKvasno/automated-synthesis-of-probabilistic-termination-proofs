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

// Rust Book 19.5 Macros: example vec! macro
// test only, breaks interface
#[cfg(test)]
#[macro_export]
macro_rules! mock_polynomial {
    [ $( $x:expr ), + $(,)?] => {
        {
            $crate::pts::linear_polynomial::LinearPolynomial::mock(std::vec![$($crate::pts::linear_polynomial::constant::Constant($x), )+])
        }
    };
    [] => {
        {
            $crate::pts::linear_polynomial::LinearPolynomial::default()
        }
    };
}

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

    pub fn try_add_term(&mut self, map: &VariableMap, term: Term) -> Result<usize, VariableError> {
        self.resize_to_map(map);
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            Ok(0)
        } else {
            let index = map.get_index(&term.variable);
            // if variable is not in the map, its not a program variable
            if index.is_none() {
                Err(VariableError::new(term.variable.as_ref().unwrap()))
            } else {
                self.coefficients[index.unwrap()] += term.coefficient;
                Ok(index.unwrap())
            }
        }
    }

    pub fn add_term(&mut self, map: &mut VariableMap, term: Term) -> usize {
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            self.resize_to_map(map);
            0
        } else {
            let index = map.get_or_push(&term.variable);
            self.resize_to_map(map);
            self.coefficients[index] += term.coefficient;
            index
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<&Constant> {
        self.coefficients.get(index)
    }

    pub fn get_mut_coefficient(&mut self, index: usize) -> Option<&mut Constant> {
        self.coefficients.get_mut(index)
    }

    pub fn separate_constant_term(mut self) -> (LinearPolynomial, LinearPolynomial) {
        let mut new_pol = LinearPolynomial::default();
        unsafe {
            // we assume all polynomials to have atleast the constant term
            assert!(self.coefficients.len() > 0);
            assert!(new_pol.coefficients.len() > 0);
            std::mem::swap(
                self.coefficients.get_unchecked_mut(0),
                new_pol.coefficients.get_unchecked_mut(0),
            );
        }
        (self, new_pol)
    }

    // only affects the smaller range of the map and polynomial
    pub fn separate_with<F: FnMut(&Term) -> bool>(
        mut self,
        variables: &VariableMap,
        mut filter: F,
    ) -> (LinearPolynomial, LinearPolynomial) {
        let mut new_pol = LinearPolynomial {
            coefficients: Vec::with_capacity(self.len()),
        };

        for i in 0..std::cmp::min(self.len(), variables.len()) {
            if filter(&Term {
                coefficient: self.coefficients[i],
                variable: variables.get_variable(i).unwrap().cloned(),
            }) {
                unsafe {
                    // see min a few lines above
                    std::mem::swap(
                        self.coefficients.get_unchecked_mut(i),
                        new_pol.coefficients.get_unchecked_mut(i),
                    );
                }
            }
        }
        (self, new_pol)
    }

    pub fn iter(&'a self, variable_map: &'a VariableMap) -> TermIterator<'a> {
        TermIterator(self, variable_map, 0)
    }

    #[cfg(test)]
    pub fn mock(coefficients: Vec<Constant>) -> Self {
        assert!(coefficients.len() > 0);
        LinearPolynomial { coefficients }
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
                coefficient: c.clone(),
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
mod tests {
    mod macros {
        use crate::{mock_varmap, pts::linear_polynomial::LinearPolynomial, term};

        #[test]
        fn mock_polynomial() {
            let mut pol = LinearPolynomial::default();
            let map = mock_varmap!("a", "e", "i");
            pol.try_add_term(&map, term!(-5.0)).unwrap();
            pol.try_add_term(&map, term!(1.0, "i")).unwrap();
            pol.try_add_term(&map, term!(0.0, "a")).unwrap();
            pol.try_add_term(&map, term!(90.0, "e")).unwrap();

            assert_eq!(mock_polynomial!(-5.0, 0.0, 90.0, 1.0), pol);
            assert_eq!(mock_polynomial!(), LinearPolynomial::default())
        }
    }

    mod add {
        use crate::{mock_varmap, pts::linear_polynomial::LinearPolynomial, term};

        #[test]
        fn resizing() {
            let mut pol = LinearPolynomial::default();
            assert_eq!(pol.len(), 1);
            let mut map = mock_varmap!("a", "b", "c");
            let var = map.get_variable(1).unwrap().unwrap().clone();
            pol.add_term(&mut map, term!(0.0, var.as_str()));
            assert_eq!(pol.len(), map.len() + 1);
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 0.0, 0.0));
        }

        #[test]
        fn variable() {
            let mut pol = LinearPolynomial::default();
            let mut map = mock_varmap!("a", "b", "c");
            let b = "b";
            pol.add_term(&mut map, term!(1.0, b));
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 1.0, 0.0));
            pol.add_term(&mut map, term!(1.0, b));
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 2.0, 0.0));
            pol.add_term(&mut map, term!(-2.0, b));
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 0.0, 0.0));
        }

        #[test]
        fn constant() {
            let mut pol = LinearPolynomial::default();
            let mut map = mock_varmap!("a", "b", "c");
            pol.add_term(&mut map, term!(1.0));
            assert_eq!(pol, mock_polynomial!(1.0, 0.0, 0.0, 0.0));
            pol.add_term(&mut map, term!(1.0));
            assert_eq!(pol, mock_polynomial!(2.0, 0.0, 0.0, 0.0));
        }

        #[test]
        fn out_of_bounds() {
            let mut pol = LinearPolynomial::default();
            let mut map = mock_varmap!("a", "b", "c");
            pol.add_term(&mut map, term!(1.0, "e"));
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 0.0, 0.0, 1.0));
        }
    }
    mod try_add {
        use crate::{
            mock_varmap,
            pts::{
                linear_polynomial::{constant::Constant, term::Term, LinearPolynomial},
                variable_map::{Variable, VariableError},
            },
            term,
        };

        #[test]
        fn try_add_resizing() {
            let mut pol = LinearPolynomial::default();
            assert_eq!(pol.len(), 1);
            let map = mock_varmap!("a", "b", "c");
            assert_eq!(
                pol.try_add_term(
                    &map,
                    term!(0.0, map.get_variable(1).unwrap().unwrap().clone().as_str())
                ),
                Ok(1)
            );
            assert_eq!(pol.len(), map.len() + 1);
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 0.0, 0.0,));
        }

        #[test]
        fn try_add_variable() {
            let mut pol = LinearPolynomial::default();
            let map = mock_varmap!("a", "b", "c");
            pol.try_add_term(&map, term!(1.0, "b")).unwrap();
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 1.0, 0.0));
            pol.try_add_term(&map, term!(1.0, "b")).unwrap();
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 2.0, 0.0));
        }

        #[test]
        fn try_add_constant() {
            let mut pol = LinearPolynomial::default();
            let map = mock_varmap!("a", "b", "c");
            pol.try_add_term(&map, term!(1.0)).unwrap();
            assert_eq!(pol, mock_polynomial!(1.0, 0.0, 0.0, 0.0));
            pol.try_add_term(&map, term!(1.0)).unwrap();
            assert_eq!(pol, mock_polynomial!(2.0, 0.0, 0.0, 0.0));
        }

        #[test]
        fn try_add_out_of_bounds() {
            let mut pol = LinearPolynomial::default();
            let map = mock_varmap!("a", "b", "c");
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
            assert_eq!(pol, mock_polynomial!(0.0, 0.0, 0.0, 0.0));
        }
    }

    mod ops {
        #[test]
        fn add() {
            let mut lhs = mock_polynomial!(1.0, 2.0, 3.0, 4.0);
            let rhs = mock_polynomial!(4.0, 3.0, 2.0, 1.0);
            let sum = lhs.clone() + rhs.clone();
            assert_eq!(sum, mock_polynomial!(5.0, 5.0, 5.0, 5.0));
            lhs += rhs;
            assert_eq!(lhs, mock_polynomial!(5.0, 5.0, 5.0, 5.0));
        }

        #[test]
        fn sub() {
            let mut lhs = mock_polynomial!(2.0, 3.0, 4.0, 5.0);
            let rhs = mock_polynomial!(1.0, 1.0, 1.0, 1.0);
            let diff = lhs.clone() - rhs.clone();
            assert_eq!(diff, mock_polynomial!(1.0, 2.0, 3.0, 4.0));
            lhs -= rhs;
            assert_eq!(lhs, mock_polynomial!(1.0, 2.0, 3.0, 4.0));
        }

        #[test]
        fn neg() {
            let lhs = mock_polynomial!(0.0, -3.0, 4.0, -5.0);
            let lhs = -lhs;
            assert_eq!(lhs, mock_polynomial!(0.0, 3.0, -4.0, 5.0));
        }
    }

    #[test]
    fn separate_constant_term() {
        let lhs = mock_polynomial!(9.0, -3.0, 4.0, -5.0);
        let (lhs, rhs) = lhs.separate_constant_term();
        assert_eq!(lhs, mock_polynomial!(0.0, -3.0, 4.0, -5.0));
        assert_eq!(rhs, mock_polynomial!(9.0));
    }

    mod label {
        use crate::{
            mock_varmap,
            pts::{
                linear_polynomial::{constant::Constant, LinearPolynomial},
                DisplayLabel,
            },
        };

        #[test]
        fn zero() {
            let pol = LinearPolynomial {
                coefficients: vec![Constant(0.0)],
            };
            let map = mock_varmap!();
            assert_eq!(pol.label(&map), "0");
        }

        #[test]
        fn negative() {
            let pol = LinearPolynomial {
                coefficients: vec![Constant(-5.0)],
            };
            let map = mock_varmap!();
            assert_eq!(pol.label(&map), "-5");
        }

        #[test]
        fn variable() {
            let pol = LinearPolynomial {
                coefficients: vec![Constant(0.0), Constant(1.0)],
            };
            let map = mock_varmap!("test");
            assert_eq!(pol.label(&map), "test");
        }

        #[test]
        fn starts_negative() {
            let pol = LinearPolynomial {
                coefficients: vec![Constant(-5.0), Constant(-1.0), Constant(0.0), Constant(2.0)],
            };

            let map = mock_varmap!("a", "b", "c",);
            assert_eq!(pol.label(&map), "-a + 2c - 5");
        }

        #[test]
        fn only_constant() {
            let pol = mock_polynomial!(-5.0, 0.0, 0.0, 0.0);

            let map = mock_varmap!("a", "b", "c",);
            assert_eq!(pol.label(&map), "-5");
        }
    }
}
