use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
    iter::Sum,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
};

use indexmap::IndexMap;

use self::coefficient::{Coefficient, Constant};

use super::variable::{program_variable::ProgramVariable, Variable};

pub mod coefficient;

#[macro_export]
macro_rules! polynomial {
    [
        $constant:expr
        $(
            ,
            $varset:expr
            $(
                ,
                $coeff:expr,
                $var:expr
            )*
        )?
        $(,)?
    ] => {
        {
            let mut temp = $crate::pts::linear_polynomial::Polynomial::default();
            $(
                $(
                    temp.add_term($coeff, Some($crate::pts::variable::Variable::new($varset, $var)));
                )*
            )?
            temp.add_term( $constant, None);
            temp
        }
    };
    [] => {
        $crate::pts::linear_polynomial::Polynomial::default()
    };
}

#[macro_export]
macro_rules! state {
    [ $($constant:expr $(, $varset:expr, $( $coeff:expr, $var:expr),+)? $(,)?)? ] => {
        {
            let tmp: $crate::pts::linear_polynomial::State = $crate::polynomial![
                $(
                    $crate::pts::linear_polynomial::coefficient::Constant::from(
                        $constant
                    )
                    $(,
                        $varset,
                        $(
                            $crate::pts::linear_polynomial::coefficient::Constant::from(
                                $coeff
                            ),
                            $var
                        ), +
                    )?
                )?
            ];
            tmp

        }
    };
}

pub type Term<V, C> = (Option<V>, C);
pub type StateTerm = (ProgramVariable, Constant);
pub type State = Polynomial<ProgramVariable, Constant>;

#[derive(Debug, Clone)]
pub struct Polynomial<V: Variable, C: Coefficient> {
    data: IndexMap<Option<V>, C>,
}

impl<V: Variable, C: Coefficient> Polynomial<V, C> {
    pub fn with_capacity(n: usize) -> Self {
        Self {
            data: IndexMap::with_capacity(n),
        }
    }

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit()
    }

    pub fn add_term<K, T>(&mut self, coefficient: T, variable: K)
    where
        T: Into<C>,
        K: Into<Option<V>>,
    {
        let c = coefficient.into();
        self.data
            .entry(variable.into())
            .and_modify(|x| *x += c.clone())
            .or_insert(c);
    }

    pub fn remove_term<K>(&mut self, variable: &K) -> Option<(Option<V>, C)>
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq,
    {
        self.data.remove_entry(variable)
    }

    pub fn get_coefficient<K>(&self, variable: &K) -> Option<&C>
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq,
    {
        self.data.get(variable)
    }

    pub fn get_coefficient_mut<K>(&mut self, variable: K) -> &mut C
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq + Into<Option<V>>,
    {
        self.data.entry(Into::into(variable)).or_insert(C::zero())
    }

    pub fn get_coefficient_mut_owned<K>(&mut self, variable: &K) -> &mut C
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq + ToOwned<Owned = Option<V>>,
    {
        self.data.entry(variable.to_owned()).or_insert(C::zero())
    }

    pub fn get_term<K>(&self, variable: &K) -> Option<(&Option<V>, &C)>
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq,
    {
        self.data.get_key_value(variable)
    }

    pub fn get_variable<K>(&self, variable: &K) -> Option<&Option<V>>
    where
        Option<V>: Borrow<K>,
        K: Hash + Eq,
    {
        self.data.get_key_value(variable).map(|(k, _)| k)
    }

    pub fn iter(&self) -> TermIter<V, C> {
        self.data
            .iter()
            .filter(|(_, coeff): &(&Option<V>, &C)| !coeff.is_zero())
    }

    pub fn iter_mut(&mut self) -> TermIterMut<V, C> {
        self.data
            .iter_mut()
            .filter(|(_, coeff): &(&Option<V>, &mut C)| !coeff.is_zero())
    }

    pub fn mul_by_constant(&mut self, c: Constant) {
        self.iter_mut().for_each(|(_, x)| x.mul_by_constant(c));
    }

    pub fn is_constant(&self) -> bool {
        self.iter().find(|(v, _)| v.is_some()).is_none()
    }
}

type TermIter<'a, V, C> =
    std::iter::Filter<indexmap::map::Iter<'a, Option<V>, C>, fn(&(&'a Option<V>, &'a C)) -> bool>;

type TermIterMut<'a, V, C> = std::iter::Filter<
    indexmap::map::IterMut<'a, Option<V>, C>,
    fn(&(&'a Option<V>, &'a mut C)) -> bool,
>;

impl<V: Variable, C: Coefficient> Eq for Polynomial<V, C> {}

impl<V: Variable, C: Coefficient> PartialEq for Polynomial<V, C> {
    // is twice as slow as regular eq on HashMaps
    fn eq(&self, other: &Self) -> bool {
        self.iter()
            .all(|(var, coeff)| other.data.get(var).map_or(false, |c| *coeff == *c))
            && other
                .iter()
                .all(|(var, coeff)| self.data.get(var).map_or(false, |c| *coeff == *c))
    }
}

impl<V: Variable, C: Coefficient> IntoIterator for Polynomial<V, C> {
    type Item = (Option<V>, C);
    type IntoIter = indexmap::map::IntoIter<Option<V>, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, V: Variable, C: Coefficient> IntoIterator for &'a Polynomial<V, C> {
    type Item = (&'a Option<V>, &'a C);
    type IntoIter = indexmap::map::Iter<'a, Option<V>, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl<V: Variable, C: Coefficient> Borrow<IndexMap<Option<V>, C>> for Polynomial<V, C> {
    fn borrow(&self) -> &IndexMap<Option<V>, C> {
        &self.data
    }
}

impl<V: Variable, C: Coefficient> FromIterator<(Option<V>, C)> for Polynomial<V, C> {
    fn from_iter<T: IntoIterator<Item = (Option<V>, C)>>(iter: T) -> Self {
        let mut pol = Self::default();
        for (var, coeff) in iter {
            pol.add_term(coeff, var);
        }
        pol
    }
}

impl<V: Variable, C: Coefficient> Default for Polynomial<V, C> {
    fn default() -> Self {
        Self {
            data: IndexMap::default(),
        }
    }
}

impl<V: Variable, C: Coefficient> Sum for Polynomial<V, C> {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut acc = Self::default();
        iter.for_each(|pol| acc += pol);
        acc
    }
}

impl<V: Variable, C: Coefficient> From<C> for Polynomial<V, C> {
    fn from(value: C) -> Self {
        polynomial!(value)
    }
}

fn fmt_term<V: Variable + Display>(
    f: &mut std::fmt::Formatter<'_>,
    variable: &Option<V>,
    coefficient: &Constant,
) -> std::fmt::Result {
    if variable.is_none() {
        // Constant term
        write!(f, "{}", coefficient.to_string(),)
    } else {
        // Linear term
        if coefficient.is_one() {
            write!(f, "{}", variable.as_ref().unwrap())
        } else if coefficient.is_neg_one() {
            write!(f, "-{}", variable.as_ref().unwrap(),)
        } else if coefficient.is_zero() {
            write!(f, "",)
        } else {
            write!(
                f,
                "{}{}",
                coefficient.to_string(),
                variable.as_ref().unwrap(),
            )
        }
    }
}

impl<V: Variable + Display> Display for Polynomial<V, Constant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self
            .data
            .iter()
            .filter(|(var, _)| var.is_some())
            .chain(self.get_term(&None))
            .filter(|(_, var)| !var.is_zero());

        //let mut iter = self.data.iter();
        let first = iter.next();
        if first.is_none() {
            write!(f, "{}", Constant::zero())?;
        } else {
            let first = first.unwrap();
            fmt_term(f, first.0, first.1)?;
            for (v, c) in iter {
                if c.is_negative() {
                    write!(f, " - ")?;
                    fmt_term(f, v, &-c.to_owned())?;
                } else {
                    write!(f, " + ")?;
                    fmt_term(f, v, c)?;
                }
            }
        }
        Ok(())
    }
}

impl<V: Variable, C: Coefficient> AddAssign for Polynomial<V, C> {
    fn add_assign(&mut self, other: Self) {
        for (var, coeff) in other.data {
            *self.data.entry(var).or_insert(C::zero()) += coeff;
        }
    }
}

impl<V: Variable, C: Coefficient> Add for Polynomial<V, C> {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        self += other;
        self
    }
}

impl<V: Variable, C: Coefficient> SubAssign for Polynomial<V, C> {
    fn sub_assign(&mut self, other: Self) {
        for (var, coeff) in other.data {
            *self.data.entry(var).or_insert(C::zero()) -= coeff;
        }
    }
}

impl<V: Variable, C: Coefficient> Sub for Polynomial<V, C> {
    type Output = Self;

    fn sub(mut self, other: Self) -> Self::Output {
        self -= other;
        self
    }
}

impl<V: Variable, C: Coefficient> Neg for Polynomial<V, C> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            data: self
                .data
                .into_iter()
                .map(|(var, coeff)| (var, -coeff))
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    mod macros {
        use crate::{
            pts::{
                linear_polynomial::{coefficient::Constant, State},
                variable::{
                    program_variable::{ProgramVariable, ProgramVariables},
                    Variable,
                },
            },
            variables,
        };

        #[test]
        fn state() {
            let mut variables: ProgramVariables = variables!("a", "e", "i");
            let data = vec![
                (None, Constant(-5.0)),
                (
                    Some(ProgramVariable::new(&mut variables, "i")),
                    Constant(1.0),
                ),
                (
                    Some(ProgramVariable::new(&mut variables, "a")),
                    Constant(0.0),
                ),
                (
                    Some(ProgramVariable::new(&mut variables, "e")),
                    Constant(90.0),
                ),
            ];

            assert_eq!(variables, variables!("a", "e", "i"));
            assert_eq!(
                state!(-5.0, &mut variables, 0.0, "a", 90.0, "e", 1.0, "i"),
                State::from_iter(data)
            );

            let data = vec![(None, Constant(-5.0))];

            assert_eq!(state!(-5.0), State::from_iter(data));

            assert_eq!(state!(), State::default());
        }
    }

    mod eq {
        use crate::pts::linear_polynomial::State;

        #[test]
        fn ignore_zero() {
            assert_eq!(State::default(), state!(0.0));
        }
    }

    mod add {

        use crate::{
            pts::{
                linear_polynomial::State,
                variable::{
                    program_variable::{ProgramVariable, ProgramVariables},
                    Variable,
                },
            },
            variables,
        };

        #[test]
        fn variable() {
            let mut pol = State::default();
            let mut variables: ProgramVariables = variables!("a", "b", "c");
            let var = variables.get("b").unwrap().clone();
            pol.add_term(1.0, var.clone());
            assert_eq!(
                pol,
                state!(0.0, &mut variables, 0.0, "a", 1.0, "b", 0.0, "c")
            );
            pol.add_term(1.0, var.clone());
            assert_eq!(
                pol,
                state!(0.0, &mut variables, 0.0, "a", 2.0, "b", 0.0, "c")
            );
            pol.add_term(-2.0, var);
            assert_eq!(
                pol,
                state!(0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c")
            );
        }

        #[test]
        fn constant() {
            let mut pol = State::default();
            let mut variables: ProgramVariables = variables!("a", "b", "c");
            pol.add_term(1.0, None);
            assert_eq!(
                pol,
                state!(1.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c")
            );
            pol.add_term(1.0, None);
            assert_eq!(
                pol,
                state!(2.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c")
            );
        }

        #[test]
        fn out_of_bounds() {
            let mut pol = State::default();
            let mut variables: ProgramVariables = variables!("a", "b", "c");
            pol.add_term(1.0, ProgramVariable::new(&mut variables, "e"));
            assert_eq!(pol, state!(0.0, &mut variables, 1.0, "e"));
        }
    }

    mod ops {
        use crate::{pts::variable::program_variable::ProgramVariables, variables};

        #[test]
        fn add() {
            let mut variables: ProgramVariables = variables!();
            let mut lhs = state!(1.0, &mut variables, 2.0, "a", 3.0, "b", 4.0, "c");
            let rhs = state!(4.0, &mut variables, 3.0, "a", 2.0, "b", 1.0, "c");
            let sum = lhs.to_owned() + rhs.to_owned();
            assert_eq!(
                sum,
                state!(5.0, &mut variables, 5.0, "a", 5.0, "b", 5.0, "c")
            );
            lhs += rhs;
            assert_eq!(
                lhs,
                state!(5.0, &mut variables, 5.0, "a", 5.0, "b", 5.0, "c")
            );
        }

        #[test]
        fn sub() {
            let mut variables: ProgramVariables = variables!();
            let mut lhs = state!(2.0, &mut variables, 3.0, "a", 4.0, "b", 5.0, "c");
            let rhs = state!(1.0, &mut variables, 1.0, "a", 1.0, "b", 1.0, "c");
            let diff = lhs.to_owned() - rhs.to_owned();
            assert_eq!(
                diff,
                state!(1.0, &mut variables, 2.0, "a", 3.0, "b", 4.0, "c")
            );
            lhs -= rhs;
            assert_eq!(
                lhs,
                state!(1.0, &mut variables, 2.0, "a", 3.0, "b", 4.0, "c")
            );
        }

        #[test]
        fn neg() {
            let mut variables: ProgramVariables = variables!();
            let lhs = state!(0.0, &mut variables, -3.0, "a", 4.0, "b", -5.0, "c");
            let lhs = -lhs;
            assert_eq!(
                lhs,
                state!(0.0, &mut variables, 3.0, "a", -4.0, "b", 5.0, "c")
            );
        }
    }

    mod fmt {

        use crate::{
            pts::{linear_polynomial::State, variable::program_variable::ProgramVariables},
            variables,
        };

        mod term {
            use crate::{
                pts::{
                    linear_polynomial::{coefficient::Constant, fmt_term},
                    variable::{
                        program_variable::{ProgramVariable, ProgramVariables},
                        Variable,
                    },
                },
                variables,
            };

            struct TestTermDisplay(Constant, Option<ProgramVariable>);

            impl TestTermDisplay {
                pub fn new<C, V>(coeff: C, var: V) -> Self
                where
                    C: Into<Constant>,
                    V: Into<Option<ProgramVariable>>,
                {
                    Self(coeff.into(), var.into())
                }
            }

            impl std::fmt::Display for TestTermDisplay {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    fmt_term(f, &self.1, &self.0)
                }
            }

            #[test]
            fn coeff_var() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(50.0, ProgramVariable::new(&mut variables, "Test"))
                        .to_string(),
                    "50Test"
                );
            }

            #[test]
            fn neg_coeff_var() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(-0.5, ProgramVariable::new(&mut variables, "a"))
                        .to_string(),
                    "-0.5a"
                );
            }

            #[test]
            fn neg_var() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(-1.0, ProgramVariable::new(&mut variables, "a"))
                        .to_string(),
                    "-a"
                );
            }

            #[test]
            fn pos_var() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(1.0, ProgramVariable::new(&mut variables, "a"))
                        .to_string(),
                    "a"
                );
            }

            #[test]
            fn zero_coeff() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(0.0, ProgramVariable::new(&mut variables, "test"))
                        .to_string(),
                    ""
                );
            }

            #[test]
            fn neg_zero_coeff() {
                let mut variables: ProgramVariables = variables!();
                assert_eq!(
                    TestTermDisplay::new(0.0, ProgramVariable::new(&mut variables, "test"))
                        .to_string(),
                    ""
                );
            }

            #[test]
            fn zero() {
                assert_eq!(TestTermDisplay::new(0.0, None).to_string(), "0");
            }

            #[test]
            fn neg_zero() {
                assert_eq!(TestTermDisplay::new(0.0, None).to_string(), "0");
            }

            #[test]
            fn neg_const() {
                assert_eq!(TestTermDisplay::new(-5.0, None).to_string(), "-5");
            }

            #[test]
            fn neg_one() {
                assert_eq!(TestTermDisplay::new(-1.0, None).to_string(), "-1");
            }

            #[test]
            fn one() {
                assert_eq!(TestTermDisplay::new(1.0, None).to_string(), "1");
            }
        }

        #[test]
        fn zero() {
            let pol = state!();
            assert_eq!(pol.to_string(), "0");
        }

        #[test]
        fn negative() {
            let pol = state!(-5.0);
            assert_eq!(pol.to_string(), "-5");
        }

        #[test]
        fn variable() {
            let mut variables: ProgramVariables = variables!("test");
            let pol = state![0.0, &mut variables, 1.0, "test"];
            assert_eq!(pol.to_string(), "test");
        }

        #[test]
        fn starts_negative() {
            let variables: ProgramVariables = variables!("a", "b", "c",);
            let mut pol = State::default();

            pol.add_term(-5.0, None);
            pol.add_term(-1.0, variables.get("a").unwrap().clone());
            pol.add_term(0.0, variables.get("b").unwrap().clone());
            pol.add_term(2.0, variables.get("c").unwrap().clone());

            assert_eq!(pol.to_string(), "-a + 2c - 5");
        }

        #[test]
        fn only_constant() {
            let mut variables: ProgramVariables = variables!("a", "b", "c",);
            let pol = state!(-5.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c");

            assert_eq!(pol.to_string(), "-5");
        }
    }
}
