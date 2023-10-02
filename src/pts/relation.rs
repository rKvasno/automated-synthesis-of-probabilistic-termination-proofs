use crate::pts::linear_polynomial::LinearPolynomial;
use std::ops::Not;

use super::linear_polynomial::constant::Constant;
use super::variable_map::VariableMap;
use super::DisplayLabel;

#[derive(Debug, PartialEq, Eq)]
pub enum RelationSign {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum RelationType {
    #[default]
    NonstrictInequality, // <=
    StrictInequality, // <
    Equation,         // ==
    Inequation,       // =/=
}

// (a_1 + a_2 + ... + a_n) * x + b < 0
// default 0 <= 0
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Relation {
    relation_type: RelationType, // default NonstrictInequality
    pol: LinearPolynomial,       // default 0
}

impl Relation {
    pub fn new(lhs: LinearPolynomial, sign: RelationSign, rhs: LinearPolynomial) -> Self {
        let relation_type = match sign {
            RelationSign::GE | RelationSign::LE => RelationType::NonstrictInequality,
            RelationSign::GT | RelationSign::LT => RelationType::StrictInequality,
            RelationSign::EQ => RelationType::Equation,
            RelationSign::NE => RelationType::Inequation,
        };
        let pol = match sign {
            RelationSign::LT | RelationSign::LE | RelationSign::EQ | RelationSign::NE => lhs - rhs,
            RelationSign::GT | RelationSign::GE => rhs - lhs,
        };
        Relation { relation_type, pol }
    }

    pub fn as_linear_polynomial<'a>(&'a self) -> &'a LinearPolynomial {
        &self.pol
    }

    pub fn is_strict_inequality(&self) -> bool {
        match self.relation_type {
            RelationType::StrictInequality => true,
            _ => false,
        }
    }

    pub fn is_nonstrict_inequality(&self) -> bool {
        match self.relation_type {
            RelationType::NonstrictInequality => true,
            _ => false,
        }
    }

    pub fn is_equation(&self) -> bool {
        match self.relation_type {
            RelationType::Equation => true,
            _ => false,
        }
    }

    pub fn is_inequation(&self) -> bool {
        match self.relation_type {
            RelationType::Inequation => true,
            _ => false,
        }
    }
}

impl DisplayLabel for Relation {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = String::default();
        let leading_linear_term = self
            .as_linear_polynomial()
            .iter(variable_map)
            .skip(1) // skip constant term
            .find(|x| x.coefficient != Constant(0.0));

        let sign;
        let left_side;
        match leading_linear_term {
            Some(t) if t.coefficient < Constant(0.0) => {
                // the leading linear coefficient is negative =>
                // we turn the inequality around, to make it positive
                sign = match self.relation_type {
                    RelationType::NonstrictInequality => " >= ",
                    RelationType::StrictInequality => " > ",
                    RelationType::Inequation => " =/= ",
                    RelationType::Equation => " = ",
                };
                left_side = -self.pol.clone();
            }
            _ => {
                sign = match self.relation_type {
                    RelationType::NonstrictInequality => " <= ",
                    RelationType::StrictInequality => " < ",
                    RelationType::Inequation => " =/= ",
                    RelationType::Equation => " = ",
                };
                left_side = self.pol.clone();
            }
        }
        let (left_side, right_side) = left_side.separate_constant_term();
        label.push_str(left_side.label(variable_map).as_str());
        label.push_str(sign);
        // neg because its separated from left side
        label.push_str((-right_side).label(variable_map).as_str());
        label
    }
}

impl Not for Relation {
    type Output = Self;

    fn not(mut self) -> Self::Output {
        self.pol = match self.relation_type {
            RelationType::StrictInequality => -self.pol,
            RelationType::NonstrictInequality => -self.pol,
            RelationType::Equation => self.pol,
            RelationType::Inequation => self.pol,
        };
        self.relation_type = match self.relation_type {
            RelationType::Equation => RelationType::Inequation,
            RelationType::StrictInequality => RelationType::NonstrictInequality,
            RelationType::NonstrictInequality => RelationType::StrictInequality,
            RelationType::Inequation => RelationType::Equation,
        };
        self
    }
}

#[cfg(test)]
impl Relation {
    pub fn mock(relation_type: RelationType, pol: LinearPolynomial) -> Self {
        Relation { relation_type, pol }
    }
}

#[cfg(test)]
mod tests {
    use super::{Relation, RelationSign};
    use crate::{
        misc::{setup_test_map, setup_test_polynomial},
        pts::{
            linear_polynomial::{constant::Constant, LinearPolynomial},
            relation::RelationType,
            system::System,
            variable_map::{Variable, VariableMap},
            DisplayLabel,
        },
    };

    #[test]
    fn new() {
        let map = setup_test_map();
        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(4.0),
                Constant(5.0),
                Constant(3.0),
                Constant(2.0),
            ),
            RelationSign::LT,
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
        assert!(cond.is_strict_inequality());

        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(0.0),
                Constant(2.0),
                Constant(2.0),
            ),
            RelationSign::GE,
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
        assert!(cond.is_nonstrict_inequality());

        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(0.0),
                Constant(2.0),
                Constant(2.0),
            ),
            RelationSign::EQ,
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
                Constant(-4.0),
                Constant(-1.0),
                Constant(-2.0),
                Constant(0.0)
            ))
        );
        assert!(cond.is_equation());

        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(4.0),
                Constant(5.0),
                Constant(3.0),
                Constant(2.0),
            ),
            RelationSign::NE,
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
        assert!(cond.is_inequation());
    }

    #[test]
    fn is() {
        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::LT,
            LinearPolynomial::default(),
        );
        assert!(!rel.is_inequation());
        assert!(!rel.is_equation());
        assert!(rel.is_strict_inequality());
        assert!(!rel.is_nonstrict_inequality());

        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::LE,
            LinearPolynomial::default(),
        );
        assert!(!rel.is_inequation());
        assert!(!rel.is_equation());
        assert!(!rel.is_strict_inequality());
        assert!(rel.is_nonstrict_inequality());

        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::GT,
            LinearPolynomial::default(),
        );
        assert!(!rel.is_inequation());
        assert!(!rel.is_equation());
        assert!(rel.is_strict_inequality());
        assert!(!rel.is_nonstrict_inequality());

        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::GE,
            LinearPolynomial::default(),
        );
        assert!(!rel.is_inequation());
        assert!(!rel.is_equation());
        assert!(!rel.is_strict_inequality());
        assert!(rel.is_nonstrict_inequality());

        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::EQ,
            LinearPolynomial::default(),
        );
        assert!(!rel.is_inequation());
        assert!(rel.is_equation());
        assert!(!rel.is_strict_inequality());
        assert!(!rel.is_nonstrict_inequality());

        let rel = Relation::new(
            LinearPolynomial::default(),
            RelationSign::NE,
            LinearPolynomial::default(),
        );
        assert!(rel.is_inequation());
        assert!(!rel.is_equation());
        assert!(!rel.is_strict_inequality());
        assert!(!rel.is_nonstrict_inequality());
    }

    #[test]
    fn not() {
        let map = setup_test_map();

        let mut system = System::default();
        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(4.0),
                Constant(-5.0),
                Constant(3.0),
                Constant(-2.0),
            ),
            RelationSign::LE,
            LinearPolynomial::default(),
        );
        system.push(cond);
        let cond = Relation::new(
            setup_test_polynomial(
                &map,
                Constant(1.0),
                Constant(0.0),
                Constant(2.0),
                Constant(-2.0),
            ),
            RelationSign::LT,
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

        assert!(&system.get(0).unwrap().is_strict_inequality());
        assert!(!&system.get(1).unwrap().is_strict_inequality());
    }

    #[test]
    fn label() {
        let ineq = Relation {
            relation_type: RelationType::NonstrictInequality,
            pol: LinearPolynomial::mock(vec![Constant(0.0)]),
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(ineq.label(&map), "0 <= 0");

        let ineq = Relation {
            relation_type: RelationType::StrictInequality,
            pol: LinearPolynomial::mock(vec![Constant(-5.0)]),
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(ineq.label(&map), "0 < 5");

        let ineq = Relation {
            relation_type: RelationType::StrictInequality,
            pol: LinearPolynomial::mock(vec![Constant(-5.0), Constant(-1.0)]),
        };
        let map = VariableMap::mock(vec![Variable::new("test")]);
        assert_eq!(ineq.label(&map), "test > -5");

        let ineq = Relation {
            relation_type: RelationType::NonstrictInequality,
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

        let ineq = Relation {
            relation_type: RelationType::NonstrictInequality,
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

        let ineq = Relation {
            relation_type: RelationType::NonstrictInequality,
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
}
