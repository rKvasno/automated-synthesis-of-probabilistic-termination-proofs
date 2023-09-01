use crate::pts::linear_polynomial::LinearPolynomial; 
use std::{ops::{Not}, iter::zip};

#[derive(Debug, PartialEq, Eq)]
pub enum ComparisonOperator {
    LT,
    LE,
    GT,
    GE,
}

// (a_1 + a_2 + ... + a_n) * x + b < 0
// default 0 <= 0
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Inequality {
    strict: bool, // default false
    pol: LinearPolynomial, // default 0
}

impl Inequality {
    pub fn new(lhs: LinearPolynomial, comp: ComparisonOperator, rhs: LinearPolynomial) -> Self {
        let strict = match comp {
            ComparisonOperator::GT | ComparisonOperator::LT => true,
            ComparisonOperator::GE | ComparisonOperator::LE => false,
        };
        let pol = match comp {
            ComparisonOperator::LT | ComparisonOperator::LE => lhs - rhs,
            ComparisonOperator::GT | ComparisonOperator::GE => rhs - lhs,
        };
        Inequality{strict, pol}
    }

    pub fn is_strict(&self) -> bool {
        self.strict
    }

    pub fn as_linear_polynomial<'a> (&'a self) -> &'a LinearPolynomial {
        &self.pol
    }
}

#[cfg(test)]
impl Inequality {
    pub fn mock(strict: bool, pol: LinearPolynomial) -> Self {
        Inequality { strict, pol }
    }
}

impl Not for Inequality {
    type Output = Self;

    fn not(mut self) -> Self::Output {
        self.strict = !self.strict;
        self.pol = -self.pol;
        self
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct InequalitySystem {
    inequalities: Vec<Inequality>,
}

impl InequalitySystem {
    pub fn push(&mut self, inequality: Inequality) {
        self.inequalities.push(inequality);
    }

    pub fn append(&mut self, system: &mut InequalitySystem) {
        self.inequalities.append(&mut system.inequalities);
    }

    pub fn len(&self) -> usize {
        self.inequalities.len()
    }

    pub fn get(&self, index: usize) -> Option<&Inequality> {
        self.inequalities.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Inequality> {
        self.inequalities.get_mut(index)
    }
}

#[cfg(test)]
impl InequalitySystem {
    pub fn mock(inequalities: Vec<Inequality>) -> Self {
        InequalitySystem { inequalities }
    }
}

impl Not for InequalitySystem {
    type Output = Self;
    fn not(self) -> Self::Output {
        InequalitySystem{ inequalities: self.inequalities.into_iter().map(|x| !x).collect() }
    }
}

#[cfg(test)]
mod tests {
    use super::{ComparisonOperator, Inequality, InequalitySystem};
    use crate::{misc::{setup_test_map, setup_test_polynomial}, pts::linear_polynomial::{constant::Constant, LinearPolynomial}};

    #[test]
    fn inequality() {
        let map = setup_test_map();
        let zero = Constant::new(0.0);
        let one = Constant::new(1.0);
        let two = Constant::new(2.0);
        let three = Constant::new(3.0);
        let four = Constant::new(4.0);
        let five = Constant::new(5.0);
        let cond = Inequality::new(setup_test_polynomial(&map, four, five, three, two), ComparisonOperator::LT, setup_test_polynomial(&map, one, five, four, two));
        assert_eq!(cond.as_linear_polynomial(), &LinearPolynomial::mock(vec!(three, zero, -one, zero)));
        assert!(cond.is_strict());
        let cond = Inequality::new(setup_test_polynomial(&map, one, zero, two, two), ComparisonOperator::GE, setup_test_polynomial(&map, five, one, four, two));
        assert_eq!(cond.as_linear_polynomial(), &LinearPolynomial::mock(vec!(four, one, two, zero)));
        assert!(!cond.is_strict());
    }

    #[test]
    fn not(){
        let map = setup_test_map();
        let zero = Constant::new(0.0);
        let one = Constant::new(1.0);
        let two = Constant::new(2.0);
        let three = Constant::new(3.0);
        let four = Constant::new(4.0);
        let five = Constant::new(5.0);
        let n_one = Constant::new(-1.0);
        let n_two = Constant::new(-2.0);
        let n_three = Constant::new(-3.0);
        let n_four = Constant::new(-4.0);
        let n_five = Constant::new(-5.0);

        let mut system = InequalitySystem::default();
        let cond = Inequality::new(setup_test_polynomial(&map, four, n_five, three, n_two), ComparisonOperator::LE, LinearPolynomial::default());
        system.push(cond);
        let cond = Inequality::new(setup_test_polynomial(&map, one, zero, two, n_two), ComparisonOperator::LT, LinearPolynomial::default());
        system.push(cond);

        system = !system;
        
        assert_eq!(system.get(0).unwrap().as_linear_polynomial(), &LinearPolynomial::mock(vec!(n_four, five, n_three, two)));
        assert_eq!(system.get(1).unwrap().as_linear_polynomial(), &LinearPolynomial::mock(vec!(n_one, zero, n_two, two)));

        assert!(&system.get(0).unwrap().is_strict());
        assert!(!&system.get(1).unwrap().is_strict());
    }
}
