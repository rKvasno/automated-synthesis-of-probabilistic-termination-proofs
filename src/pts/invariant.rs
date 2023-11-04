use std::borrow::Borrow;

use super::system::StateSystem;

#[macro_export]
macro_rules! invariant {
    [
        $(
            $varset: expr,
            $(
                [
                    $(
                       $sign:literal, $constant:expr $( ,$coeff:expr, $var:expr )*
                    );*
                ]
            ),* $(,)?
        )?
    ] => {


        {
            $(
                $crate::pts::invariant::Invariant::from(
                    vec![
                        $($crate::system![
                            $(
                                $crate::relation!
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
            )?
        }
    };
}

pub type PolyhedronID = usize; // index into Invariant

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct Invariant {
    data: Vec<StateSystem>,
}

pub type InvariantWithIDIterator<'a> = std::iter::Enumerate<InvariantIter<'a>>;
pub type InvariantIter<'a> = std::slice::Iter<'a, StateSystem>;

impl Invariant {
    pub fn tighten(&mut self, restriction: &StateSystem) {
        for assertion in self.data.iter_mut() {
            assertion.append(&mut restriction.to_owned());
        }
    }

    pub fn iter(&self) -> InvariantIter {
        self.data.iter()
    }

    pub fn iter_with_ids(&self) -> InvariantWithIDIterator {
        // two counters but its a lot simpler like this
        self.iter().enumerate()
    }
}

impl Borrow<Vec<StateSystem>> for Invariant {
    fn borrow(&self) -> &Vec<StateSystem> {
        &self.data
    }
}

impl From<Vec<StateSystem>> for Invariant {
    fn from(value: Vec<StateSystem>) -> Self {
        Self { data: value }
    }
}

impl FromIterator<StateSystem> for Invariant {
    fn from_iter<T: IntoIterator<Item = StateSystem>>(iter: T) -> Self {
        Self {
            data: Vec::from_iter(iter),
        }
    }
}

impl IntoIterator for Invariant {
    type Item = StateSystem;
    type IntoIter = std::vec::IntoIter<StateSystem>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a> IntoIterator for &'a Invariant {
    type Item = &'a StateSystem;
    type IntoIter = std::slice::Iter<'a, StateSystem>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl std::fmt::Display for Invariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.data.iter();

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
    use crate::{pts::invariant::Invariant, relation, system, variables};

    #[test]
    fn invariant() {
        let mut variables = variables!();
        assert_eq!(
            Invariant {
                data: vec![system!(
                    relation!(
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
                    relation!("<=", -1.0, &mut variables, -2.4, "a"),
                    relation!("=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c"),
                    relation!("!=", 111.111)
                )]
            },
            invariant!(
                &mut variables,
                [
                    ">", 0.0, 12.4, "a", -3.5, "b", 2.4, "c", -0.0, "d";
                    "<=", -1.0, -2.4, "a";
                    "=", 0.0, 0.0, "a", 0.0, "b", 0.0, "c";
                    "!=", 111.111
                ]
            ),
        );
    }
}
