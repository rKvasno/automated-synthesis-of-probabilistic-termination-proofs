use crate::pts::linear_polynomial::LinearPolynomial;
use std::ops::Not;

use super::linear_polynomial::constant::Constant;
use super::variable_map::VariableMap;
use super::DisplayLabel;

#[macro_export]
macro_rules! relation_type {
    [ $sign:literal] => {
        {
            match $sign {
                "<=" | ">=" => $crate::pts::relation::RelationType::NonstrictInequality,
                "<" | ">" => $crate::pts::relation::RelationType::StrictInequality,
                "==" | "=" => $crate::pts::relation::RelationType::Equation,
                "=/=" | "!=" => $crate::pts::relation::RelationType::Inequation,
                _ => panic!("Invalid sign"),
            }
        }
    };
}

// test only, breaks interface
#[cfg(test)]
#[macro_export]
macro_rules! mock_relation {
   [ $sign:literal, $( $x:expr ),* $(,)?] => {
        {
            $crate::pts::relation::Relation::mock($crate::relation_type![$sign], $crate::mock_polynomial![$($x, )+])
        }
    };
}

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

    #[cfg(test)]
    pub fn mock(relation_type: RelationType, pol: LinearPolynomial) -> Self {
        Relation { relation_type, pol }
    }
}

impl DisplayLabel for Relation {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = String::default();
        let leading_linear_term = self
            .as_linear_polynomial()
            .iter_terms(variable_map)
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
mod tests {
    use crate::{
        mock_polynomial,
        pts::{
            linear_polynomial::LinearPolynomial,
            relation::{Relation, RelationSign},
        },
        system,
    };

    mod macros {
        use crate::{
            mock_polynomial,
            pts::relation::{Relation, RelationType},
        };

        mod relation_type {
            use crate::pts::relation::RelationType;

            #[test]
            fn equation() {
                assert_eq!(RelationType::Equation, relation_type!("="));
            }

            #[test]
            fn equation_alt() {
                assert_eq!(RelationType::Equation, relation_type!("=="));
            }

            #[test]
            fn inequation() {
                assert_eq!(RelationType::Inequation, relation_type!("=/="));
            }

            #[test]
            fn inequation_alt() {
                assert_eq!(RelationType::Inequation, relation_type!("!="));
            }

            #[test]
            fn strict() {
                assert_eq!(RelationType::StrictInequality, relation_type!("<"));
            }

            #[test]
            fn strict_alt() {
                assert_eq!(RelationType::StrictInequality, relation_type!(">"));
            }

            #[test]
            fn nonstrict() {
                assert_eq!(RelationType::NonstrictInequality, relation_type!("<="));
            }

            #[test]
            fn nonstrict_alt() {
                assert_eq!(RelationType::NonstrictInequality, relation_type!(">="));
            }

            #[test]
            #[should_panic]
            fn panic() {
                relation_type!("");
            }
        }
        #[test]
        fn relation() {
            assert_eq!(
                Relation {
                    relation_type: RelationType::StrictInequality,
                    pol: mock_polynomial!(1.0, -4.5, 0.0)
                },
                mock_relation!("<", 1.0, -4.5, 0.0)
            )
        }
    }

    mod new {
        use crate::{
            mock_polynomial,
            pts::relation::{Relation, RelationSign},
        };

        #[test]
        fn less_than() {
            let cond = Relation::new(
                mock_polynomial!(4.0, 5.0, 3.0, 2.0),
                RelationSign::LT,
                mock_polynomial!(1.0, 5.0, 4.0, 2.0),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &mock_polynomial!(3.0, 0.0, -1.0, 0.0)
            );
            assert!(cond.is_strict_inequality());
        }

        #[test]
        fn greater_eq() {
            let cond = Relation::new(
                mock_polynomial!(1.0, 0.0, 2.0, 2.0),
                RelationSign::GE,
                mock_polynomial!(5.0, 1.0, 4.0, 2.0,),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &mock_polynomial!(4.0, 1.0, 2.0, 0.0)
            );
            assert!(cond.is_nonstrict_inequality());
        }

        #[test]
        fn equal() {
            let cond = Relation::new(
                mock_polynomial!(1.0, 0.0, 2.0, 2.0,),
                RelationSign::EQ,
                mock_polynomial!(5.0, 1.0, 4.0, 2.0,),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &mock_polynomial!(-4.0, -1.0, -2.0, 0.0)
            );
            assert!(cond.is_equation());
        }

        #[test]
        fn not_equal() {
            let cond = Relation::new(
                mock_polynomial!(4.0, 5.0, 3.0, 2.0,),
                RelationSign::NE,
                mock_polynomial!(1.0, 5.0, 4.0, 2.0,),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &mock_polynomial!(3.0, 0.0, -1.0, 0.0)
            );
            assert!(cond.is_inequation());
        }
    }

    mod is {
        use crate::pts::{
            linear_polynomial::LinearPolynomial,
            relation::{Relation, RelationSign},
        };

        #[test]
        fn less_than() {
            let rel = Relation::new(
                LinearPolynomial::default(),
                RelationSign::LT,
                LinearPolynomial::default(),
            );
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn less_eq() {
            let rel = Relation::new(
                LinearPolynomial::default(),
                RelationSign::LE,
                LinearPolynomial::default(),
            );
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(rel.is_nonstrict_inequality());
        }

        #[test]
        fn greater_than() {
            let rel = Relation::new(
                LinearPolynomial::default(),
                RelationSign::GT,
                LinearPolynomial::default(),
            );
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn greater_eq() {
            let rel = Relation::new(
                LinearPolynomial::default(),
                RelationSign::GE,
                LinearPolynomial::default(),
            );
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(rel.is_nonstrict_inequality());
        }

        #[test]
        fn equal() {
            let rel = Relation::new(
                LinearPolynomial::default(),
                RelationSign::EQ,
                LinearPolynomial::default(),
            );
            assert!(!rel.is_inequation());
            assert!(rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn not_equal() {
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
    }

    #[test]
    fn not() {
        let mut system = system!(
            Relation::new(
                mock_polynomial!(4.0, -5.0, 3.0, -2.0,),
                RelationSign::LE,
                LinearPolynomial::default(),
            ),
            Relation::new(
                mock_polynomial!(1.0, 0.0, 2.0, -2.0,),
                RelationSign::LT,
                LinearPolynomial::default(),
            )
        );

        system = !system;

        assert_eq!(
            system.get(0).unwrap().as_linear_polynomial(),
            &mock_polynomial!(-4.0, 5.0, -3.0, 2.0)
        );
        assert_eq!(
            system.get(1).unwrap().as_linear_polynomial(),
            &mock_polynomial!(-1.0, 0.0, -2.0, 2.0)
        );

        assert!(&system.get(0).unwrap().is_strict_inequality());
        assert!(!&system.get(1).unwrap().is_strict_inequality());
    }

    mod label {
        use crate::{
            mock_polynomial, mock_varmap,
            pts::{
                relation::{Relation, RelationType},
                DisplayLabel,
            },
        };

        #[test]
        fn zero() {
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: mock_polynomial!(0.0),
            };
            let map = mock_varmap!();
            assert_eq!(ineq.label(&map), "0 <= 0");
        }

        #[test]
        fn neg_const() {
            let ineq = Relation {
                relation_type: RelationType::StrictInequality,
                pol: mock_polynomial!(-5.0),
            };
            let map = mock_varmap!();
            assert_eq!(ineq.label(&map), "0 < 5");
        }

        #[test]
        fn neg_const_neg_var() {
            let ineq = Relation {
                relation_type: RelationType::StrictInequality,
                pol: mock_polynomial!(-5.0, -1.0),
            };
            let map = mock_varmap!("test");
            assert_eq!(ineq.label(&map), "test > -5");
        }

        #[test]
        fn neg_leading_var() {
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: mock_polynomial!(0.0, -1.0, 0.0, 3.0,),
            };
            let map = mock_varmap!("a", "b", "c",);
            assert_eq!(ineq.label(&map), "a - 3c >= 0");
        }

        #[test]
        fn neg_const_pos_var() {
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: mock_polynomial!(-1.0, 1.0, 0.0, 0.0,),
            };
            let map = mock_varmap!("a", "b", "c",);
            assert_eq!(ineq.label(&map), "a <= 1");
        }

        #[test]
        fn pos_var() {
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: mock_polynomial!(0.0, 1.0, 0.0, 0.0,),
            };
            let map = mock_varmap!("a", "b", "c");
            assert_eq!(ineq.label(&map), "a <= 0");
        }
    }
}
