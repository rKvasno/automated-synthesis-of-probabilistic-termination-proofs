use crate::pts::linear_polynomial::LinearPolynomial;
use std::ops::Not;
use std::slice::Iter;

use super::linear_polynomial::constant::Constant;
use super::variable_map::VariableMap;
use super::DisplayLabel;

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

impl DisplayLabel for Inequality {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = String::default();
        let leading_linear_term = self
            .as_linear_polynomial()
            .iter(variable_map)
            .skip(1) // skip constant term
            .find(|x| x.coefficient != Constant(0.0));

        let sign;
        let mut left_side;
        match leading_linear_term {
            Some(t) if t.coefficient < Constant(0.0) => {
                // the leading linear coefficient is negative =>
                // we turn the inequality around, to make it positive
                sign = " >";
                left_side = -self.pol.clone();
            }
            _ => {
                sign = " <";
                left_side = self.pol.clone();
            }
        }
        let right_side = left_side.separate_constant_term();
        label.push_str(left_side.label(variable_map).as_str());
        label.push_str(sign);
        if self.strict {
            label.push_str(" ");
        } else {
            label.push_str("= ");
        }

        label.push_str((-right_side).label(variable_map).as_str());
        label
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

#[cfg(test)]
impl Inequality {
    pub fn mock(strict: bool, pol: LinearPolynomial) -> Self {
        Inequality { strict, pol }
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

impl DisplayLabel for InequalitySystem {
    fn label(&self, variable_map: &VariableMap) -> String {
        //ineq (\n ineq)*
        let mut label = String::default();
        let mut iter = self.inequalities.iter().map(|x| x.label(variable_map));
        match iter.next() {
            Some(line) => label.push_str(line.as_str()),
            _ => (),
        }
        for line in iter {
            label.push_str("\n");
            label.push_str(line.as_str());
        }
        label
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
impl InequalitySystem {
    pub fn mock(inequalities: Vec<Inequality>) -> Self {
        InequalitySystem { inequalities }
    }
}

#[cfg(test)]
mod tests {
    use super::{ComparisonOperator, Inequality, InequalitySystem};
    use crate::{
        misc::{setup_test_map, setup_test_polynomial},
        pts::{
            linear_polynomial::{constant::Constant, LinearPolynomial},
            variable_map::{Variable, VariableMap},
            DisplayLabel,
        },
    };

    #[test]
    fn inequality_new() {
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
                Constant(-1.0),
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

    #[test]
    fn label_inequality() {
        let ineq = Inequality {
            strict: false,
            pol: LinearPolynomial::mock(vec![Constant(0.0)]),
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(ineq.label(&map), "0 <= 0");

        let ineq = Inequality {
            strict: true,
            pol: LinearPolynomial::mock(vec![Constant(-5.0)]),
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(ineq.label(&map), "0 < 5");

        let ineq = Inequality {
            strict: true,
            pol: LinearPolynomial::mock(vec![Constant(-5.0), Constant(-1.0)]),
        };
        let map = VariableMap::mock(vec![Variable::new("test")]);
        assert_eq!(ineq.label(&map), "test > -5");

        let ineq = Inequality {
            strict: false,
            pol: LinearPolynomial::mock(vec![
                Constant(0.0),
                Constant(-1.0),
                Constant(0.0),
                Constant(3.0),
            ]),
        };
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(ineq.label(&map), "a - 3c >= 0");

        let ineq = Inequality {
            strict: false,
            pol: LinearPolynomial::mock(vec![
                Constant(-1.0),
                Constant(1.0),
                Constant(0.0),
                Constant(0.0),
            ]),
        };
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(ineq.label(&map), "a <= 1");

        let ineq = Inequality {
            strict: false,
            pol: LinearPolynomial::mock(vec![
                Constant(0.0),
                Constant(1.0),
                Constant(0.0),
                Constant(0.0),
            ]),
        };
        let map = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);
        assert_eq!(ineq.label(&map), "a <= 0");
    }

    #[test]
    fn label_inequality_system() {
        let system = InequalitySystem {
            inequalities: vec![
                Inequality {
                    strict: false,
                    pol: LinearPolynomial::mock(vec![Constant(0.0)]),
                },
                Inequality {
                    strict: false,
                    pol: LinearPolynomial::mock(vec![Constant(0.0)]),
                },
            ],
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(system.label(&map), "0 <= 0\n0 <= 0");

        let system = InequalitySystem {
            inequalities: vec![
                Inequality {
                    strict: false,
                    pol: LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                },
                Inequality {
                    strict: false,
                    pol: LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0)]),
                },
            ],
        };
        let map = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);
        assert_eq!(system.label(&map), "a <= 0\na >= 0");

        let system = InequalitySystem {
            inequalities: vec![Inequality {
                strict: true,
                pol: LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(-1.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            }],
        };
        let map = VariableMap::mock(vec![
            Variable::new("test1"),
            Variable::new("test2"),
            Variable::new("test3"),
        ]);
        assert_eq!(system.label(&map), "test1 > 0");
    }
}
