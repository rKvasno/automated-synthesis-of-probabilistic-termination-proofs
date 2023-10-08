use std::fmt;

use crate::pts::{linear_polynomial, system, transition};
use linear_polynomial::constant::Constant;
use system::System;
use transition::Transition;

use super::{variable_map::VariableMap, DisplayLabel};

#[macro_export]
macro_rules! guards {
    [ $t:expr ] => {
        {
            $crate::pts::guard::Guards::Unguarded(std::boxed::Box::new($t))
        }
    };
    [ P: $( $p:expr, $t:expr ), + $(,)?] => {
        {
            $crate::pts::guard::Guards::Probabilistic(std::vec![$(($crate::pts::linear_polynomial::constant::Constant($p), $t),)*])
        }
    };
    [ L: $( $c:expr, $t:expr ), + $(,)?] => {
        {
            $crate::pts::guard::Guards::Logic(std::vec![$(($c, $t),)*])
        }
    };
    [ $( $t:expr ), + $(,)?] => {
        {
            $crate::pts::guard::Guards::Nondeterministic(std::vec![$($t,)*])
        }
    };
}

pub type Probability = Constant;

// 32 bytes
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum Guards {
    Logic(Vec<(System, Transition)>),
    Probabilistic(Vec<(Probability, Transition)>),
    Nondeterministic(Vec<Transition>),
    Unguarded(Box<Transition>),
}

impl Guards {
    pub fn is_empty(&self) -> bool {
        match self {
            Guards::Logic(data) => data.is_empty(),
            Guards::Probabilistic(data) => data.is_empty(),
            Guards::Nondeterministic(data) => data.is_empty(),
            Guards::Unguarded(_) => false,
        }
    }

    pub fn iter(&self) -> GuardsIterator {
        GuardsIterator(self, 0)
    }
}

impl Default for Guards {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GuardsError {
    TerminatingLocation,
    Empty,
}

impl fmt::Display for GuardsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GuardsError::Empty => write!(f, "Guards can't be empty!"),
            GuardsError::TerminatingLocation => write!(
                f,
                "The terminating location can't have outgoing transitions!"
            ),
        }
    }
}

pub enum GuardedTransition<'a> {
    Logic(&'a (System, Transition)),
    Probabilistic(&'a (Probability, Transition)),
    Nondeterministic(&'a Transition),
    Unguarded(&'a Transition),
}

impl<'a> GuardedTransition<'a> {
    pub fn as_transition(&'a self) -> &'a Transition {
        match self {
            Self::Logic((_, t)) => t,
            Self::Probabilistic((_, t)) => t,
            Self::Nondeterministic(t) => t,
            Self::Unguarded(t) => t,
        }
    }
}

impl<'a> DisplayLabel for GuardedTransition<'a> {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = match self {
            Self::Logic((condition, _)) => condition.label(variable_map),
            Self::Probabilistic((probability, _)) => format!("{}", probability),
            Self::Nondeterministic(_) => String::default(),
            Self::Unguarded(_) => String::default(),
        };
        for assign in self.as_transition().assignments.iter() {
            label.push_str("\n");
            label.push_str(assign.label(variable_map).as_str());
        }
        label
    }
}

pub struct GuardsIterator<'a>(&'a Guards, usize);

impl<'a> Iterator for GuardsIterator<'a> {
    type Item = GuardedTransition<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.0 {
            &Guards::Logic(ref branches) => {
                branches.get(self.1).map(|x| GuardedTransition::Logic(x))
            }
            &Guards::Probabilistic(ref branches) => branches
                .get(self.1)
                .map(|x| GuardedTransition::Probabilistic(x)),
            &Guards::Nondeterministic(ref branches) => branches
                .get(self.1)
                .map(|x| GuardedTransition::Nondeterministic(x)),
            &Guards::Unguarded(ref boxed_transition) => {
                if self.1 == 0 {
                    Some(GuardedTransition::Unguarded(boxed_transition))
                } else {
                    None
                }
            }
        };
        self.1 += 1;
        item
    }
}

#[cfg(test)]
mod tests {

    mod macros {

        mod guards {
            use crate::{
                mock_relation, mock_transition,
                pts::{guard::Guards, linear_polynomial::constant::Constant},
                system,
            };
            #[test]
            fn unguarded() {
                assert_eq!(
                    guards!(mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0)),
                    Guards::Unguarded(std::boxed::Box::new(
                        mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0)
                    ))
                );
            }

            #[test]
            fn nondeterministic() {
                assert_eq!(
                    guards!(
                        mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0),
                        mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8),
                        mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                    ),
                    Guards::Nondeterministic(vec![
                        mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0),
                        mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8),
                        mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                    ])
                );
            }

            #[test]
            fn logic() {
                assert_eq!(
                    guards!(L:
                        system!(mock_relation!(">", 0.1, -2.3)),
                        mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0),
                        system!(
                            mock_relation!("<=", -0.1, 2.3),
                            mock_relation!(">=", 0.0, 3.8),
                        ),
                        mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8),
                        system!(
                            mock_relation!("<=", -0.1, 2.3),
                            mock_relation!("<", 0.0, -3.8),
                        ),
                        mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                    ),
                    Guards::Logic(vec![
                        (
                            system!(mock_relation!(">", 0.1, -2.3)),
                            mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0)
                        ),
                        (
                            system!(
                                mock_relation!("<=", -0.1, 2.3),
                                mock_relation!(">=", 0.0, 3.8),
                            ),
                            mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8)
                        ),
                        (
                            system!(
                                mock_relation!("<=", -0.1, 2.3),
                                mock_relation!("<", 0.0, -3.8),
                            ),
                            mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                        )
                    ])
                );
            }

            #[test]
            fn probabilistic() {
                assert_eq!(
                    guards!(P:
                        0.0,
                        mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0),
                        0.3,
                        mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8),
                        0.7,
                        mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                    ),
                    Guards::Probabilistic(vec![
                        (
                            Constant(0.0),
                            mock_transition!(Some(23); 1, 2.4, 1.3; 2, 1.0, -1.0)
                        ),
                        (
                            Constant(0.3),
                            mock_transition!(None; 1, 1.3, 2.4; 2, 13.0, -1.8)
                        ),
                        (
                            Constant(0.7),
                            mock_transition!(Some(21); 1, -20.4, 1.3; 2, 0.0, 0.0)
                        )
                    ])
                );
            }
        }
    }
    mod label {
        use crate::{
            mock_relation, mock_transition, mock_varmap,
            pts::{guard::GuardedTransition, linear_polynomial::constant::Constant, DisplayLabel},
            system,
        };

        #[test]
        fn logic() {
            let data = (system![mock_relation!("<", 1.0)], mock_transition!(None));
            let guarded_transition = GuardedTransition::Logic(&data);
            let map = mock_varmap!();
            assert_eq!(guarded_transition.label(&map), "0 < -1");

            let data = (
                system!(
                    mock_relation!("<=", 0.0, 1.0),
                    mock_relation!("<", 1.0, 2.0, 1.0)
                ),
                mock_transition!(None; 1, 0.0),
            );
            let guarded_transition = GuardedTransition::Logic(&data);
            let map = mock_varmap!("a", "len");
            assert_eq!(
                guarded_transition.label(&map),
                "a <= 0\n2a + len < -1\na = 0"
            );
        }

        #[test]
        fn probabilistic() {
            let data = (
                Constant(0.0),
                mock_transition!(None;
                    1, 0.0, 0.0;
                    2, 2.0, 1.0, 2.0, 0.0;
                    1, -4.0, 0.0, 0.0, 1.0
                ),
            );
            let guarded_transition = GuardedTransition::Probabilistic(&data);
            let map = mock_varmap!("a", "b", "c",);
            assert_eq!(
                guarded_transition.label(&map),
                "0\na = 0\nb = a + 2b + 2\na = c - 4"
            );

            let data = (Constant(0.2222), mock_transition!(None));
            let guarded_transition = GuardedTransition::Probabilistic(&data);
            let map = mock_varmap!();
            assert_eq!(guarded_transition.label(&map), "0.2222");
        }

        #[test]
        fn nondeterministic() {
            let data = mock_transition!(None);
            let guarded_transition = GuardedTransition::Nondeterministic(&data);
            let map = mock_varmap!();
            assert_eq!(guarded_transition.label(&map), "");
        }

        #[test]
        fn unguarded() {
            let data = mock_transition!(None);
            let guarded_transition = GuardedTransition::Unguarded(&data);
            let map = mock_varmap!(Default::default());
            assert_eq!(guarded_transition.label(&map), "");
        }
    }
}
