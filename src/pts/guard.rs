use crate::pts::{transition, linear_polynomial};
use transition::Transition;
use linear_polynomial::LinearPolynomial;

pub type Odds = u64;

#[derive(Debug)]
pub enum ComparisonOperator {
    LT,
    LE,
    GT,
    GE,
}

// (a_1 + a_2 + ... + a_n) * x + b < 0
#[derive(Debug)]
pub struct Inequality {
    strict: bool,
    pol: LinearPolynomial,
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

// 32 bytes
#[derive(Debug)]
pub enum Guards<'a>{
    Logic(Vec<(Inequality, Transition<'a>)>),
    Probabilistic(Vec<(Odds, Transition<'a>)>),
    Nondeterministic(Vec<Transition<'a>>),
    Unguarded(Box<Transition<'a>>)
}

#[cfg(test)]
mod tests {
    use std::mem;
    use super::Inequality;
    use crate::{misc::{setup_test_map, setup_test_polynomial, check_terms}, pts::{linear_polynomial::constant::Constant, guard::ComparisonOperator}};

    use super::{Odds, Guards};
    
    #[test]
    fn align_odds() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<Odds>(), 8);
        }
    }

    #[test]
    fn size_odds() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Odds>(), 8);
        }
    }

    #[test]
    fn align_guards() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<Guards>(), 8);
        }
    }

    #[test]
    fn size_guards() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Guards>(), 32);
        }
    }

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
        check_terms(&cond.as_linear_polynomial(), &map, vec!(Some(three), Some(zero), Some(-one), Some(zero)));
        assert!(cond.is_strict());
        let cond = Inequality::new(setup_test_polynomial(&map, one, zero, two, two), ComparisonOperator::GE, setup_test_polynomial(&map, five, one, four, two));
        check_terms(&cond.as_linear_polynomial(), &map, vec!(Some(four), Some(one), Some(two), Some(zero)));
        assert!(!cond.is_strict());
    }
    
}

