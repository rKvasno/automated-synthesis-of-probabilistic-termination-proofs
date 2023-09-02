use crate::pts::linear_polynomial::LinearPolynomial;
use std::ops::Not;
use std::slice::Iter;

#[derive(Debug, PartialEq, Eq)]
pub enum ComparisonOperator {
    LT,
    LE,
    GT,
    GE,
}

pub type InequalityIter<'a> = Iter<'a, Inequality>;

// (a_1 + a_2 + ... + a_n) * x + b < 0
// default 0 <= 0
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Inequality {
    strict: bool,          // default false
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
        Inequality { strict, pol }
    }

    pub fn is_strict(&self) -> bool {
        self.strict
    }

    pub fn as_linear_polynomial<'a>(&'a self) -> &'a LinearPolynomial {
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

    pub fn iter<'a>(&'a self) -> InequalityIter<'a> {
        self.inequalities.iter()
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
        InequalitySystem {
            inequalities: self.inequalities.into_iter().map(|x| !x).collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ComparisonOperator, Inequality, InequalitySystem};
    use crate::{
        misc::{setup_test_map, setup_test_polynomial},
        pts::linear_polynomial::{constant::Constant, LinearPolynomial},
    };

    #[test]
    fn inequality() {
        let map = setup_test_map();
        let cond = Inequality::new(
            setup_test_polynomial(
                &map,
                Constant(4.0),
                Constant(5.0),
                Constant(3.0),
                Constant(2.0),
            ),
            ComparisonOperator::LT,
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(5.0),
                Constant(4.0),
                Constant(2.0),
            ),
        );
        assert_eq!(
            cond.as_linear_polynomial(),
            &LinearPolynomial::mock(vec!(
                Constant(3.0),
                Constant(0.0),
                -Constant(1.0),
                Constant(0.0)
            ))
        );
        assert!(cond.is_strict());
        let cond = Inequality::new(
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(0.0),
                Constant(2.0),
                Constant(2.0),
            ),
            ComparisonOperator::GE,
            setup_test_polynomial(
                &map,
                Constant(5.0),
                Constant(1.0),
                Constant(4.0),
                Constant(2.0),
            ),
        );
        assert_eq!(
            cond.as_linear_polynomial(),
            &LinearPolynomial::mock(vec!(
                Constant(4.0),
                Constant(1.0),
                Constant(2.0),
                Constant(0.0)
            ))
        );
        assert!(!cond.is_strict());
    }

    #[test]
    fn not() {
        let map = setup_test_map();

        let mut system = InequalitySystem::default();
        let cond = Inequality::new(
            setup_test_polynomial(
                &map,
                Constant(4.0),
                Constant(-5.0),
                Constant(3.0),
                Constant(-2.0),
            ),
            ComparisonOperator::LE,
            LinearPolynomial::default(),
        );
        system.push(cond);
        let cond = Inequality::new(
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(0.0),
                Constant(2.0),
                Constant(-2.0),
            ),
            ComparisonOperator::LT,
            LinearPolynomial::default(),
        );
        system.push(cond);

        system = !system;

        assert_eq!(
            system.get(0).unwrap().as_linear_polynomial(),
            &LinearPolynomial::mock(vec!(
                Constant(-4.0),
                Constant(5.0),
                Constant(-3.0),
                Constant(2.0)
            ))
        );
        assert_eq!(
            system.get(1).unwrap().as_linear_polynomial(),
            &LinearPolynomial::mock(vec!(
                Constant(-1.0),
                Constant(0.0),
                Constant(-2.0),
                Constant(2.0)
            ))
        );

        assert!(&system.get(0).unwrap().is_strict());
        assert!(!&system.get(1).unwrap().is_strict());
    }
}
