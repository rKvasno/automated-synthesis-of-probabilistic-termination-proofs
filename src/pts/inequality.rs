use crate::pts::linear_polynomial::LinearPolynomial; 

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

#[derive(Debug)]
pub struct InequalitySystem {
    inequalities: Vec<Inequality>,
}

impl InequalitySystem {
    pub fn new() -> Self {
        InequalitySystem { inequalities: vec!() }
    }
    
    pub fn push(&mut self, inequality: Inequality) {
        self.inequalities.push(inequality);
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
mod tests {
    use super::{ComparisonOperator, Inequality};
    use crate::{misc::{setup_test_map, setup_test_polynomial, check_terms}, pts::linear_polynomial::constant::Constant};

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
