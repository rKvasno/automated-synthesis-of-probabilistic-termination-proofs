use crate::state_system;
use std::borrow::Borrow;
use std::ops::{AddAssign, Mul, MulAssign, Not};

use super::system::StateSystem;

#[macro_export]
macro_rules! invariant {
    [
        $varset: expr,
        $(
            [
                $(
                   $sign:literal, $constant:expr $( ,$coeff:expr, $var:expr )*
                );*
            ]
        ),* $(,)?
    ] => {
        {
            $crate::pts::invariant::Invariant::from(
                vec![
                    $($crate::system![
                        $(
                            $crate::state_relation!
                            [
                                $sign,
                                $constant,
                                $varset
                                $(
                                    ,$coeff, $var
                                )*

                            ],
                        ) *
                    ], )*
                ]
            )
        }
    };
}

pub type PolyhedronID = usize; // index into Invariant

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct Invariant {
    disjunction: Vec<StateSystem>,
}

pub type InvariantWithIDIterator<'a> = std::iter::Enumerate<InvariantIter<'a>>;
pub type InvariantIter<'a> = std::slice::Iter<'a, StateSystem>;

impl Invariant {
    pub fn zero() -> Self {
        Self {
            disjunction: Vec::new(),
        }
    }

    pub fn one() -> Self {
        Self {
            disjunction: vec![state_system!()],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.disjunction.is_empty()
    }

    pub fn iter(&self) -> InvariantIter {
        self.disjunction.iter()
    }

    pub fn iter_with_ids(&self) -> InvariantWithIDIterator {
        self.iter().enumerate()
    }
}

impl AddAssign<Self> for Invariant {
    fn add_assign(&mut self, mut rhs: Self) {
        self.disjunction.append(&mut rhs.disjunction);
    }
}

impl AddAssign<StateSystem> for Invariant {
    fn add_assign(&mut self, rhs: StateSystem) {
        self.disjunction.push(rhs);
    }
}

impl MulAssign<&Self> for Invariant {
    fn mul_assign(&mut self, rhs: &Self) {
        *self = self.clone() * rhs;
    }
}

impl MulAssign<StateSystem> for Invariant {
    fn mul_assign(&mut self, mut rhs: StateSystem) {
        for lhs_polyhedron in self.disjunction.iter_mut() {
            lhs_polyhedron.append(&mut rhs);
        }
    }
}

impl Mul<&Self> for Invariant {
    type Output = Self;

    fn mul(self, rhs: &Self) -> Self::Output {
        let mut acc = Invariant::zero();

        for lhs_polyhedron in self.disjunction.iter() {
            for rhs_polyhedron in rhs.disjunction.iter() {
                let mut new_polyhedron = lhs_polyhedron.clone();
                new_polyhedron.append(&mut rhs_polyhedron.clone());
                acc += new_polyhedron;
            }
        }
        acc
    }
}

impl Not for Invariant {
    type Output = Invariant;
    fn not(self) -> Self::Output {
        let mut acc = Invariant::one();
        for polyhedron in self.disjunction.into_iter() {
            let old_acc = acc;
            acc = Invariant::zero();
            for predicate in polyhedron {
                let mut new = old_acc.clone();
                for new_polyhedron in new.disjunction.iter_mut() {
                    new_polyhedron.push(!predicate.clone());
                }
                acc += new;
            }
        }
        acc
    }
}

impl Borrow<Vec<StateSystem>> for Invariant {
    fn borrow(&self) -> &Vec<StateSystem> {
        &self.disjunction
    }
}

impl From<Vec<StateSystem>> for Invariant {
    fn from(value: Vec<StateSystem>) -> Self {
        Self { disjunction: value }
    }
}

impl FromIterator<StateSystem> for Invariant {
    fn from_iter<T: IntoIterator<Item = StateSystem>>(iter: T) -> Self {
        Self {
            disjunction: Vec::from_iter(iter),
        }
    }
}

impl IntoIterator for Invariant {
    type Item = StateSystem;
    type IntoIter = std::vec::IntoIter<StateSystem>;

    fn into_iter(self) -> Self::IntoIter {
        self.disjunction.into_iter()
    }
}

impl<'a> IntoIterator for &'a Invariant {
    type Item = &'a StateSystem;
    type IntoIter = std::slice::Iter<'a, StateSystem>;

    fn into_iter(self) -> Self::IntoIter {
        self.disjunction.iter()
    }
}

impl std::fmt::Display for Invariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.disjunction.iter();

        let assertion = iter.next();
        if assertion.is_some() {
            write!(f, "{}", assertion.unwrap())?;
            for assertion in iter {
                write!(f, "\n\n{}", assertion)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        pts::{invariant::Invariant, variable::program_variable::ProgramVariables},
        state_relation, system, variables,
    };

    #[test]
    fn invariant_macro() {
        let mut variables: ProgramVariables = variables!();
        assert_eq!(
            Invariant {
                disjunction: vec![
                    system!(
                        state_relation!(
                            ">",
                            0.0,
                            &mut variables,
                            12.4,
                            "a",
                            -3.5,
                            "b",
                            2.4,
                            "c",
                            -0.0,
                            "d"
                        ),
                        state_relation!("<=", -1.0, &mut variables, -2.4, "a"),
                        state_relation!("=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c"),
                        state_relation!("!=", 111.111)
                    ),
                    system!(
                        state_relation!("=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c"),
                        state_relation!("!=", 111.111)
                    )
                ]
            },
            invariant!(
                &mut variables,
                [
                    ">", 0.0, 12.4, "a", -3.5, "b", 2.4, "c", -0.0, "d";
                    "<=", -1.0, -2.4, "a";
                    "=", 0.0, 0.0, "a", 0.0, "b", 0.0, "c";
                    "!=", 111.111
                ],
                [
                    "=", 0.0, 0.0, "a", 0.0, "b", 0.0, "c";
                    "!=", 111.111
                ],
            ),
        );
    }

    #[test]
    fn mul() {
        let mut variables: ProgramVariables = variables!();
        let lhs = invariant!(
            &mut variables,
            [
                "<", 0.0, 1.0, "a";
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
            [
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
        );
        let rhs = invariant!(
            &mut variables,
            [
                "<", 0.0, 2.0, "a"
            ],
            [
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
        );

        assert_eq!(
            lhs * &rhs,
            invariant!(
                &mut variables,
                [
                    "<", 0.0, 1.0, "a";
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c";
                    "<", 0.0, 2.0, "a"
                ],
                [
                    "<", 0.0, 1.0, "a";
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c";
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c"
                ],
                [
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c";
                    "<", 0.0, 2.0, "a"
                ],
                [
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c";
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c"
                ],
            )
        )
    }

    #[test]
    fn not() {
        let mut variables: ProgramVariables = variables!();
        let tmp = invariant!(
            &mut variables,
            [
                "<", 0.0, 1.0, "a";
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
            [
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
        );
        assert_eq!(
            !tmp,
            invariant!(
                &mut variables,
                [
                    ">=", 0.0, 1.0, "a";
                    ">", 0.0, 1.0, "b"
                ],
                [
                    ">", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "b"
                ],
                [
                    "<=", 0.0, 1.0, "c";
                    ">", 0.0, 1.0, "b"
                ],
                [
                    ">=", 0.0, 1.0, "a";
                    "<=", 0.0, 1.0, "c"
                ],
                [
                    ">", 0.0, 1.0, "b";
                    "<=", 0.0, 1.0, "c"
                ],
                [
                    "<=", 0.0, 1.0, "c";
                    "<=", 0.0, 1.0, "c"
                ],
            )
        )
    }

    #[test]
    fn add() {
        let mut variables: ProgramVariables = variables!();
        let mut tmp = invariant!(
            &mut variables,
            [
                "<", 0.0, 1.0, "a";
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
            [
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
        );
        tmp += invariant!(
            &mut variables,
            [
                "<", 0.0, 2.0, "a"
            ],
            [
                "<=", 0.0, 1.0, "b";
                ">", 0.0, 1.0, "c"
            ],
        );

        assert_eq!(
            tmp,
            invariant!(
                &mut variables,
                [
                    "<", 0.0, 1.0, "a";
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c"
                ],
                [
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c"
                ],
                [
                    "<", 0.0, 2.0, "a"
                ],
                [
                    "<=", 0.0, 1.0, "b";
                    ">", 0.0, 1.0, "c"
                ],
            )
        )
    }
}
