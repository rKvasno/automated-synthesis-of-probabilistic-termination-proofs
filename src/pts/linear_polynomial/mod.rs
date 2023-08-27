pub mod constant;
pub mod term;
use crate::pts::variable_map::{VariableMap, VariableError};
use constant::{Constant, ZERO};
use term::Term;
use std::{ops::{AddAssign, SubAssign, Add, Sub, Neg}, iter::zip};

#[derive(Debug, Default, Clone)]
pub struct LinearPolynomial {
    coefficients: Vec<Constant>
}

impl LinearPolynomial {
    pub fn new() -> Self {
        LinearPolynomial { coefficients: vec!(ZERO) }
    }

    pub fn len(&self) -> usize {
        self.coefficients.len()
    }

    fn resize(&mut self, map: &VariableMap) {
        if self.len() < map.len() + 1 {
            // +1 for constant term
            self.coefficients.resize(map.len() + 1, ZERO);
        }
    }

    pub fn try_add_term(&mut self, 
                    map: &VariableMap,
                    term: Term) -> Result<(), VariableError> {
        self.resize(map);
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            Ok(())
        }
        else{
            let index = map.get_index(term.variable.as_ref().unwrap());
            // if variable is not in the map, its not a program variable
            if index.is_none() {
                Err(VariableError::new(term.variable.as_ref().unwrap()))   
            }
            else {
                self.coefficients[index.unwrap()] += term.coefficient;
                Ok(())
            }
        }
    }

    pub fn add_term(&mut self, 
                    map: &mut VariableMap,
                    term: Term) {
        if term.variable.is_none() {
            self.coefficients[0] += term.coefficient;
            self.resize(map);
        }
        else{
            let index = map.find_or_add(term.variable.unwrap());
            self.resize(map);
            self.coefficients[index] += term.coefficient;
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<Constant> {
        self.coefficients.get(index).map(|x| x.clone())
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
        LinearPolynomial{ coefficients: self.coefficients.into_iter().map(|x| -x).collect() }
    }
}

#[cfg(test)]
mod tests {
    use super::{LinearPolynomial, Term, VariableError, Constant, ZERO};
    use crate::pts::linear_polynomial::constant::ONE;
    use crate::misc::{check_terms, setup_test_map, setup_test_polynomial};
    use crate::pts::variable_map::Variable;

    #[test]
    fn add_resizing() {
        let mut pol = LinearPolynomial::new();
        assert_eq!(pol.len(), 1);
        let mut map = setup_test_map();
        let var = Option::<&Variable>::cloned(map.get_variable(1));
        pol.add_term(&mut map, Term::new(var, ZERO));
        assert_eq!(pol.len(), map.len() + 1);
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(ZERO), Some(ZERO)));
    }
    
    #[test]
    fn add_variable() {
        let mut pol = LinearPolynomial::new();
        let mut map = setup_test_map();
        let b = Variable::new("b");
        pol.add_term(&mut map, Term::new(Some(b.clone()), ONE));
        check_terms(&pol, &mut map, vec!(Some(ZERO), Some(ZERO), Some(ONE), Some(ZERO)));
        pol.add_term(&mut map, Term::new(Some(b), ONE));
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(Constant::new(2.0)), Some(ZERO)));
    }

    #[test]
    fn add_constant() {
        let mut pol = LinearPolynomial::new();
        let mut map = setup_test_map();
        pol.add_term(&mut map, Term::new(None, ONE));
        check_terms(&pol, &map, vec!(Some(ONE), Some(ZERO), Some(ZERO), Some(ZERO)));
        pol.add_term(&mut map, Term::new(None, ONE));
        check_terms(&pol, &map, vec!(Some(Constant::new(2.0)), Some(ZERO), Some(ZERO), Some(ZERO)));
    }

    #[test]
    fn add_out_of_bounds() {
        let mut pol = LinearPolynomial::new();
        let mut map = setup_test_map();
        let e = Variable::new("e");
        pol.add_term(&mut map, Term::new(Some(e), ONE));
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(ZERO), Some(ZERO), Some(ONE)));
    }

    #[test]
    fn try_add_resizing() {
        let mut pol = LinearPolynomial::new();
        assert_eq!(pol.len(), 1);
        let map = setup_test_map();
        assert_eq!(pol.try_add_term(&map, Term::new(Option::<&Variable>::cloned(map.get_variable(1)), ZERO)), Ok(()));
        assert_eq!(pol.len(), map.len() + 1);
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(ZERO), Some(ZERO)));
    }
    
    #[test]
    fn try_add_variable() {
        let mut pol = LinearPolynomial::new();
        let map = setup_test_map();
        let b = Variable::new("b");
        pol.try_add_term(&map, Term::new(Some(b.clone()), ONE)).unwrap();
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(ONE), Some(ZERO)));
        pol.try_add_term(&map, Term::new(Some(b), ONE)).unwrap();
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(Constant::new(2.0)), Some(ZERO)));
    }

    #[test]
    fn try_add_constant() {
        let mut pol = LinearPolynomial::new();
        let map = setup_test_map();
        pol.try_add_term(&map, Term::new(None, ONE)).unwrap();
        check_terms(&pol, &map, vec!(Some(ONE), Some(ZERO), Some(ZERO), Some(ZERO)));
        pol.try_add_term(&map, Term::new(None, ONE)).unwrap();
        check_terms(&pol, &map, vec!(Some(Constant::new(2.0)), Some(ZERO), Some(ZERO), Some(ZERO)));
    }

    #[test]
    fn try_add_out_of_bounds() {
        let mut pol = LinearPolynomial::new();
        let map = setup_test_map();
        let e = Variable::new("e");
        assert_eq!(pol.try_add_term(&map, Term::new(Some(e.clone()), ONE)), Err(VariableError::new(&e)));
        check_terms(&pol, &map, vec!(Some(ZERO), Some(ZERO), Some(ZERO), Some(ZERO)));
    }

    #[test]
    fn arithmetic_add() {
        let mut map = setup_test_map();
        let one = Constant::new(1.0);
        let two = Constant::new(2.0);
        let three = Constant::new(3.0);
        let four = Constant::new(4.0);
        let mut lhs = setup_test_polynomial(&mut map, one, two, three, four);
        let rhs = setup_test_polynomial(&mut map, four, three, two, one);
        let five = Constant::new(5.0);
        let sum = lhs.clone() + rhs.clone();
        check_terms(&sum, &map, vec!(Some(five), Some(five), Some(five), Some(five)));
        lhs += rhs;
        check_terms(&lhs, &map, vec!(Some(five), Some(five), Some(five), Some(five)));
    }
    
    #[test]
    fn arithmetic_sub() {
        let mut map = setup_test_map();
        let one = Constant::new(1.0);
        let two = Constant::new(2.0);
        let three = Constant::new(3.0);
        let four = Constant::new(4.0);
        let five = Constant::new(5.0);
        let mut lhs = setup_test_polynomial(&mut map, two, three, four, five);
        let rhs = setup_test_polynomial(&mut map, one, one, one, one);
        let diff = lhs.clone() - rhs.clone();
        check_terms(&diff, &map, vec!(Some(one), Some(two), Some(three), Some(four)));
        lhs -= rhs;
        check_terms(&lhs, &map, vec!(Some(one), Some(two), Some(three), Some(four)));
    }

    #[test]
    fn neg() {
        let mut map = setup_test_map();
        let zero = Constant::new(0.0);
        let three = Constant::new(3.0);
        let four = Constant::new(4.0);
        let five = Constant::new(5.0);
        let n_two = Constant::new(-2.0);
        let n_three = Constant::new(-3.0);
        let n_four = Constant::new(-4.0);
        let n_five = Constant::new(-5.0);
        let lhs = setup_test_polynomial(&mut map, zero, n_three, four, n_five);
        let lhs = -lhs;
        check_terms(&lhs, &map, vec!(Some(zero), Some(three), Some(n_four), Some(five)));
    }

}


