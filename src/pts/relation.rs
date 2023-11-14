use std::{fmt::Display, ops::Not, str::FromStr};

use super::{
    linear_polynomial::{
        coefficient::{Coefficient, Constant},
        Polynomial,
    },
    variable::{program_variable::ProgramVariable, Variable},
};

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

#[macro_export]
macro_rules! relation {
   [
       $sign:literal,
       $constant: expr $(
           , $varset:expr $(,
               $coeff:expr,
               $var:expr
            )*
        )? $(,)?
   ] => {
        {
            $crate::pts::relation::Relation::new(
                $crate::polynomial![
                    $constant
                    $(
                        ,
                        $varset
                        $(
                            ,
                            $coeff,
                            $var
                        )*
                    )?
                ],
                std::str::FromStr::from_str(
                    $sign
                ).unwrap(),
                $crate::polynomial!(0.0)
            )
        }
    };
}

#[macro_export]
macro_rules! state_relation {
   [
       $sign:literal,
       $constant: expr $(
           , $varset:expr $(,
               $coeff:expr,
               $var:expr
            )*
        )? $(,)?
   ] => {
       {
           $crate::relation![
               $sign,
               $constant $(
                   , $varset $(,
                       $coeff,
                       std::rc::Rc::from($var)
                    )*
               )?
           ]
       }
   }
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

impl FromStr for RelationSign {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            ">" => Ok(RelationSign::GT),
            ">=" => Ok(RelationSign::GE),
            "<" => Ok(RelationSign::LT),
            "<=" => Ok(RelationSign::LE),
            "=" | "==" => Ok(RelationSign::EQ),
            "!=" | "=/=" => Ok(RelationSign::NE),
            _ => Err(()),
        }
    }
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
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Relation<V: Variable, C: Coefficient> {
    relation_type: RelationType, // default NonstrictInequality
    pol: Polynomial<V, C>,       // default 0
}

pub type StateRelation = Relation<ProgramVariable, Constant>;

impl<V: Variable, C: Coefficient> Relation<V, C> {
    pub fn new(lhs: Polynomial<V, C>, sign: RelationSign, rhs: Polynomial<V, C>) -> Self {
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

    pub fn as_linear_polynomial<'b>(&'b self) -> &'b Polynomial<V, C> {
        &self.pol
    }

    pub fn get_relation_type(&self) -> &RelationType {
        &self.relation_type
    }

    pub fn get_relation_sign(&self) -> RelationSign {
        match self.relation_type {
            RelationType::Equation => RelationSign::EQ,
            RelationType::StrictInequality => RelationSign::LT,
            RelationType::Inequation => RelationSign::NE,
            RelationType::NonstrictInequality => RelationSign::LE,
        }
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

    pub fn split_constant(mut self) -> (Polynomial<V, C>, Polynomial<V, C>) {
        let mut new_pol = Polynomial::<V, C>::default();
        std::mem::swap(
            self.pol.get_coefficient_mut(None),
            new_pol.get_coefficient_mut(None),
        );
        (self.pol, -new_pol)
    }
}

impl<V: Variable + Display> std::fmt::Display for Relation<V, Constant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let leading_term = self.as_linear_polynomial().iter().find(|x| x.0.is_some());

        let sign;
        let (left_side, right_side) = self.to_owned().split_constant();
        match leading_term {
            Some((_, coeff)) if coeff.is_negative() => {
                // the leading coefficient is negative =>
                // we turn the inequality around, to make it positive
                sign = match self.relation_type {
                    RelationType::NonstrictInequality => " >= ",
                    RelationType::StrictInequality => " > ",
                    RelationType::Inequation => " =/= ",
                    RelationType::Equation => " = ",
                };
                write!(f, "{}{}{}", -left_side, sign, -right_side)
            }
            _ => {
                sign = match self.relation_type {
                    RelationType::NonstrictInequality => " <= ",
                    RelationType::StrictInequality => " < ",
                    RelationType::Inequation => " =/= ",
                    RelationType::Equation => " = ",
                };
                write!(f, "{}{}{}", left_side, sign, right_side)
            }
        }
    }
}

impl<V: Variable, C: Coefficient> Not for Relation<V, C> {
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
        pts::{
            linear_polynomial::State,
            relation::{Relation, RelationSign},
            variable::program_variable::ProgramVariables,
        },
        state, system, variables,
    };

    mod macros {
        use crate::{
            pts::{
                relation::{Relation, RelationType},
                variable::program_variable::ProgramVariables,
            },
            state, variables,
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
            let mut variables: ProgramVariables = variables!();
            let a = Relation {
                relation_type: RelationType::StrictInequality,
                pol: state!(1.0, &mut variables, -4.5, "c", 0.0, "a"),
            };
            let b = state_relation!("<", 1.0, &mut variables, -4.5, "c", 0.0, "a");
            assert_eq!(a, b)
        }
    }

    mod new {
        use crate::{
            pts::{
                relation::{Relation, RelationSign},
                variable::program_variable::ProgramVariables,
            },
            state, variables,
        };

        #[test]
        fn less_than() {
            let mut variables: ProgramVariables = variables!();
            let cond = Relation::new(
                state!(4.0, &mut variables, 5.0, "a", 3.0, "b", 2.0, "c"),
                RelationSign::LT,
                state!(1.0, &mut variables, 5.0, "a", 4.0, "b", 2.0, "c"),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &state!(3.0, &mut variables, 0.0, "a", -1.0, "b", 0.0, "c")
            );
            assert!(cond.is_strict_inequality());
        }

        #[test]
        fn greater_eq() {
            let mut variables: ProgramVariables = variables!();
            let cond = Relation::new(
                state!(1.0, &mut variables, 0.0, "a", 2.0, "b", 2.0, "c"),
                RelationSign::GE,
                state!(5.0, &mut variables, 1.0, "a", 4.0, "b", 2.0, "c",),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &state!(4.0, &mut variables, 1.0, "a", 2.0, "b", 0.0, "c")
            );
            assert!(cond.is_nonstrict_inequality());
        }

        #[test]
        fn equal() {
            let mut variables: ProgramVariables = variables!();
            let cond = Relation::new(
                state!(1.0, &mut variables, 0.0, "a", 2.0, "b", 2.0, "c",),
                RelationSign::EQ,
                state!(5.0, &mut variables, 1.0, "a", 4.0, "b", 2.0, "c",),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &state!(-4.0, &mut variables, -1.0, "a", -2.0, "b", 0.0, "c")
            );
            assert!(cond.is_equation());
        }

        #[test]
        fn not_equal() {
            let mut variables: ProgramVariables = variables!();
            let cond = Relation::new(
                state!(4.0, &mut variables, 5.0, "a", 3.0, "b", 2.0, "c",),
                RelationSign::NE,
                state!(1.0, &mut variables, 5.0, "a", 4.0, "b", 2.0, "c",),
            );
            assert_eq!(
                cond.as_linear_polynomial(),
                &state!(3.0, &mut variables, 0.0, "a", -1.0, "b", 0.0, "c")
            );
            assert!(cond.is_inequation());
        }
    }

    mod split {
        use crate::{pts::variable::program_variable::ProgramVariables, state, variables};

        #[test]
        fn constant() {
            let mut variables: ProgramVariables = variables!();
            let lhs = state_relation!("<=", 9.0, &mut variables, -3.0, "x", -5.0, "y", 4.0, "z");
            let (lhs, rhs) = lhs.split_constant();
            assert_eq!(
                lhs,
                state!(0.0, &mut variables, -3.0, "x", 4.0, "z", -5.0, "y")
            );
            assert_eq!(rhs, state!(-9.0));
        }
    }

    mod is {
        use crate::pts::{
            linear_polynomial::State,
            relation::{Relation, RelationSign},
        };

        #[test]
        fn less_than() {
            let rel = Relation::new(State::default(), RelationSign::LT, State::default());
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn less_eq() {
            let rel = Relation::new(State::default(), RelationSign::LE, State::default());
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(rel.is_nonstrict_inequality());
        }

        #[test]
        fn greater_than() {
            let rel = Relation::new(State::default(), RelationSign::GT, State::default());
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn greater_eq() {
            let rel = Relation::new(State::default(), RelationSign::GE, State::default());
            assert!(!rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(rel.is_nonstrict_inequality());
        }

        #[test]
        fn equal() {
            let rel = Relation::new(State::default(), RelationSign::EQ, State::default());
            assert!(!rel.is_inequation());
            assert!(rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }

        #[test]
        fn not_equal() {
            let rel = Relation::new(State::default(), RelationSign::NE, State::default());
            assert!(rel.is_inequation());
            assert!(!rel.is_equation());
            assert!(!rel.is_strict_inequality());
            assert!(!rel.is_nonstrict_inequality());
        }
    }

    #[test]
    fn not() {
        let mut variables: ProgramVariables = variables!();
        let mut system = system!(
            Relation::new(
                state!(4.0, &mut variables, -5.0, "a", 3.0, "b", -2.0, "c",),
                RelationSign::LE,
                State::default(),
            ),
            Relation::new(
                state!(1.0, &mut variables, 0.0, "a", 2.0, "b", -2.0, "c",),
                RelationSign::LT,
                State::default(),
            )
        );

        system = !system;

        assert_eq!(
            system.get(0).unwrap().as_linear_polynomial(),
            &state!(-4.0, &mut variables, 5.0, "a", -3.0, "b", 2.0, "c")
        );
        assert_eq!(
            system.get(1).unwrap().as_linear_polynomial(),
            &state!(-1.0, &mut variables, 0.0, "a", -2.0, "b", 2.0, "c")
        );

        assert!(&system.get(0).unwrap().is_strict_inequality());
        assert!(!&system.get(1).unwrap().is_strict_inequality());
    }

    mod label {
        use crate::{
            pts::{
                relation::{Relation, RelationType},
                variable::program_variable::ProgramVariables,
            },
            state, variables,
        };

        #[test]
        fn zero() {
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: state!(0.0),
            };
            assert_eq!(ineq.to_string(), "0 <= 0");
        }

        #[test]
        fn neg_const() {
            let ineq = Relation {
                relation_type: RelationType::StrictInequality,
                pol: state!(-5.0),
            };
            assert_eq!(ineq.to_string(), "0 < 5");
        }

        #[test]
        fn neg_const_neg_var() {
            let mut variables: ProgramVariables = variables!();
            let ineq = Relation {
                relation_type: RelationType::StrictInequality,
                pol: state!(-5.0, &mut variables, -1.0, "test"),
            };
            assert_eq!(ineq.to_string(), "test > -5");
        }

        #[test]
        fn neg_leading_var() {
            let mut variables: ProgramVariables = variables!();
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: state!(0.0, &mut variables, -1.0, "a", 0.0, "b", 3.0, "c"),
            };
            assert_eq!(ineq.to_string(), "a - 3c >= 0");
        }

        #[test]
        fn neg_const_pos_var() {
            let mut variables: ProgramVariables = variables!();
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: state!(-1.0, &mut variables, 1.0, "a", 0.0, "b", 0.0, "c",),
            };
            assert_eq!(ineq.to_string(), "a <= 1");
        }

        #[test]
        fn pos_var() {
            let mut variables: ProgramVariables = variables!();
            let ineq = Relation {
                relation_type: RelationType::NonstrictInequality,
                pol: state!(0.0, &mut variables, 1.0, "a", 0.0, "b", 0.0, "c"),
            };
            assert_eq!(ineq.to_string(), "a <= 0");
        }
    }
}
