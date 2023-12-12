use super::{
    linear_polynomial::coefficient::Constant, system::StateSystem, transition::Transition,
};

#[macro_export]
macro_rules! guards {
    [ $t:expr ] => {
        {
            $crate::pts::guard::Guards::Unguarded(std::boxed::Box::new($t))
        }
    };
    [ P: $( $p:expr, $t:expr ), + $(,)?] => {
        {
            $crate::pts::guard::Guards::Probabilistic(std::vec![$(($crate::pts::linear_polynomial::coefficient::Constant($p), $t),)*])
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

pub type TransitionID = usize; // index into Guards

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub enum Guards {
    // it is the programmers responsibility to make sure probabilities are non-negative and their
    // sum is 1.0 (as close to 1.0 as f64 allows)
    Probabilistic(Vec<(Probability, Transition)>),
    Logic(Vec<(StateSystem, Transition)>),
    Nondeterministic(Vec<Transition>),
    Unguarded(Box<Transition>),
}

impl Guards {
    pub fn is_empty(&self) -> bool {
        match self {
            Guards::Probabilistic(data) => data.is_empty(),
            Guards::Logic(data) => data.is_empty(),
            Guards::Nondeterministic(data) => data.is_empty(),
            Guards::Unguarded(_) => false,
        }
    }

    pub fn iter(&self) -> TransitionsIterator {
        TransitionsIterator(self, 0)
    }

    pub fn iter_with_ids(&self) -> TransitionsWithIDIterator {
        // two counters but its a lot simpler like this
        self.iter().enumerate()
    }
}

impl Default for Guards {
    fn default() -> Self {
        Guards::Unguarded(Box::new(Transition::default()))
    }
}

impl From<Vec<(Probability, Transition)>> for Guards {
    fn from(value: Vec<(Probability, Transition)>) -> Self {
        Self::Probabilistic(value)
    }
}

impl From<Vec<(StateSystem, Transition)>> for Guards {
    fn from(value: Vec<(StateSystem, Transition)>) -> Self {
        Self::Logic(value)
    }
}

impl From<Vec<Transition>> for Guards {
    fn from(value: Vec<Transition>) -> Self {
        Self::Nondeterministic(value)
    }
}

impl From<Transition> for Guards {
    fn from(value: Transition) -> Self {
        Self::Unguarded(Box::new(value))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GuardsError {
    StateTerminatingLocation,
    Empty,
}

impl std::fmt::Display for GuardsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GuardsError::Empty => write!(f, "guards can't be empty"),
            GuardsError::StateTerminatingLocation => {
                write!(f, "terminating location can't have outgoing transitions")
            }
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum GuardedTransition<'a> {
    Logic(&'a (StateSystem, Transition)),
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

impl<'a> std::fmt::Display for GuardedTransition<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Logic((condition, _)) => write!(f, "{}", condition)?,
            Self::Probabilistic((probability, _)) => write!(f, "{}", probability)?,
            _ => (),
        };
        for op in self.as_transition().update_function.iter() {
            write!(f, "\n{}", op)?;
        }
        Ok(())
    }
}

pub struct TransitionsIterator<'a>(&'a Guards, usize);
pub type TransitionsWithIDIterator<'a> = std::iter::Enumerate<TransitionsIterator<'a>>;

impl<'a> Iterator for TransitionsIterator<'a> {
    type Item = GuardedTransition<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let item = match self.0 {
            &Guards::Logic(ref branches) => branches.get(self.1).map(GuardedTransition::Logic),
            &Guards::Probabilistic(ref branches) => {
                branches.get(self.1).map(GuardedTransition::Probabilistic)
            }
            &Guards::Nondeterministic(ref branches) => branches
                .get(self.1)
                .map(GuardedTransition::Nondeterministic),
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
                program_variables,
                pts::{
                    guard::Guards, linear_polynomial::coefficient::Constant,
                    variable::program_variable::ProgramVariables,
                },
                state_assignment, state_system, transition,
            };
            #[test]
            fn unguarded() {
                let mut variables: ProgramVariables = program_variables!();
                assert_eq!(
                    guards!(transition!(
                        Some(23),
                        state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                        state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                    )),
                    Guards::Unguarded(std::boxed::Box::new(transition!(
                        Some(23),
                        state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                        state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                    )))
                );
            }

            #[test]
            fn nondeterministic() {
                let mut variables: ProgramVariables = program_variables!();
                assert_eq!(
                    guards!(
                        transition!(
                            Some(23),
                            state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                            state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                        ),
                        transition!(
                            None,
                            state_assignment!(&mut variables, "a", 1.3, 2.4, "a"),
                            state_assignment!(&mut variables, "b", 13.0, -1.8, "a")
                        ),
                        transition!(
                            Some(21),
                            state_assignment!(&mut variables, "a", -20.4, 1.3, "a"),
                            state_assignment!(&mut variables, "b", 0.0, 0.0, "a")
                        )
                    ),
                    Guards::Nondeterministic(vec![
                        transition!(
                            Some(23),
                            state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                            state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                        ),
                        transition!(
                            None,
                            state_assignment!(&mut variables, "a", 1.3, 2.4, "a"),
                            state_assignment!(&mut variables, "b", 13.0, -1.8, "a")
                        ),
                        transition!(
                            Some(21),
                            state_assignment!(&mut variables, "a", -20.4, 1.3, "a"),
                            state_assignment!(&mut variables, "b", 0.0, 0.0, "a")
                        )
                    ])
                );
            }

            #[test]
            fn logic() {
                let mut variables: ProgramVariables = program_variables!();
                assert_eq!(
                    guards!(L:
                        state_system!(&mut variables; ">", 0.1, -2.3, "a"),
                        transition!(Some(23), state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),state_assignment!(&mut variables,  "b", 1.0, -1.0, "a")),
                        state_system!(&mut variables;
                            "<=", -0.1, 2.3, "a";
                            ">=", 0.0, 3.8, "a"
                        ),
                        transition!(None, state_assignment!(&mut variables, "a", 1.3, 2.4, "a"),state_assignment!(&mut variables, "b", 13.0, -1.8, "a")),
                        state_system!(&mut variables;
                            "<=", -0.1, 2.3, "a";
                            "<", 0.0, -3.8, "a"
                        ),
                        transition!(Some(21), state_assignment!(&mut variables, "a", -20.4, 1.3, "a"),state_assignment!(&mut variables, "b", 0.0, 0.0, "a"))
                    ),
                    Guards::Logic(vec![
                        (
                            state_system!(&mut variables;">", 0.1, -2.3, "a"),
                            transition!(
                                Some(23),
                                state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                                state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                            )
                        ),
                        (
                            state_system!(&mut variables;"<=", -0.1, 2.3, "a"; ">=", 0.0, 3.8, "a"),
                            transition!(
                                None,
                                state_assignment!(&mut variables, "a", 1.3, 2.4, "a"),
                                state_assignment!(&mut variables, "b", 13.0, -1.8, "a")
                            )
                        ),
                        (
                            state_system!(&mut variables;"<=", -0.1, 2.3, "a"; "<", 0.0, -3.8, "a"),
                            transition!(
                                Some(21),
                                state_assignment!(&mut variables, "a", -20.4, 1.3, "a"),
                                state_assignment!(&mut variables, "b", 0.0, 0.0, "a")
                            )
                        )
                    ])
                );
            }

            #[test]
            fn probabilistic() {
                let mut variables: ProgramVariables = program_variables!();
                assert_eq!(
                    guards!(P:
                        0.0,
                        transition!(Some(23), state_assignment!(&mut variables, "a", 2.4, 1.3, "a"), state_assignment!(&mut variables, "b", 1.0, -1.0, "a")),
                        0.3,
                        transition!(None, state_assignment!(&mut variables, "a", 1.3, 2.4, "a"), state_assignment!(&mut variables, "b", 13.0, -1.8, "a")),
                        0.7,
                        transition!(Some(21), state_assignment!(&mut variables, "a", -20.4, 1.3, "a"), state_assignment!(&mut variables, "b", 0.0, 0.0, "a"))
                    ),
                    Guards::Probabilistic(vec![
                        (
                            Constant(0.0),
                            transition!(
                                Some(23),
                                state_assignment!(&mut variables, "a", 2.4, 1.3, "a"),
                                state_assignment!(&mut variables, "b", 1.0, -1.0, "a")
                            )
                        ),
                        (
                            Constant(0.3),
                            transition!(
                                None,
                                state_assignment!(&mut variables, "a", 1.3, 2.4, "a"),
                                state_assignment!(&mut variables, "b", 13.0, -1.8, "a")
                            )
                        ),
                        (
                            Constant(0.7),
                            transition!(
                                Some(21),
                                state_assignment!(&mut variables, "a", -20.4, 1.3, "a"),
                                state_assignment!(&mut variables, "b", 0.0, 0.0, "a")
                            )
                        )
                    ])
                );
            }
        }
    }
    mod label {

        use crate::{
            program_variables,
            pts::{
                guard::GuardedTransition, linear_polynomial::coefficient::Constant,
                variable::program_variable::ProgramVariables,
            },
            relation, state_assignment, state_system, system, transition,
        };

        #[test]
        fn logic() {
            let data = (system![relation!("<", 1.0)], transition!(None));
            let guarded_transition = GuardedTransition::Logic(&data);
            assert_eq!(guarded_transition.to_string(), "0 < -1");

            let mut variables: ProgramVariables = program_variables!();

            let data = (
                state_system!(&mut variables;
                    "<=", 0.0, 1.0, "a";
                    "<", 1.0, 2.0, "a", 1.0, "len"
                ),
                transition!(None, state_assignment!(&mut variables, "a", 0.0)),
            );
            let guarded_transition = GuardedTransition::Logic(&data);
            assert_eq!(
                guarded_transition.to_string(),
                "a <= 0\n2a + len < -1\na = 0"
            );
        }

        #[test]
        fn probabilistic() {
            let mut variables: ProgramVariables = program_variables!("a", "b", "c",);
            let data = (
                Constant(0.0),
                transition!(
                    None,
                    state_assignment!(&mut variables, "a", 0.0, 0.0, "a"),
                    state_assignment!(&mut variables, "b", 2.0, 1.0, "a", 2.0, "b", 0.0, "c"),
                    state_assignment!(&mut variables, "a", -4.0, 0.0, "a", 0.0, "b", 1.0, "c")
                ),
            );
            let guarded_transition = GuardedTransition::Probabilistic(&data);
            assert_eq!(
                guarded_transition.to_string(),
                "0\na = 0\nb = a + 2b + 2\na = c - 4"
            );

            let data = (Constant(0.2222), transition!(None));
            let guarded_transition = GuardedTransition::Probabilistic(&data);
            assert_eq!(guarded_transition.to_string(), "0.2222");
        }

        #[test]
        fn nondeterministic() {
            let data = transition!(None);
            let guarded_transition = GuardedTransition::Nondeterministic(&data);
            assert_eq!(guarded_transition.to_string(), "");
        }

        #[test]
        fn unguarded() {
            let data = transition!(None);
            let guarded_transition = GuardedTransition::Unguarded(&data);
            assert_eq!(guarded_transition.to_string(), "");
        }
    }
}
