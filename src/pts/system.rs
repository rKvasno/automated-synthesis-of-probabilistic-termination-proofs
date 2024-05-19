use std::{
    borrow::{Borrow, BorrowMut},
    fmt::Display,
    //ops::Not,
};

use super::{
    linear_polynomial::coefficient::{Coefficient, Constant},
    relation::Relation,
    variable::{program_variable::ProgramVariable, Variable},
};

#[macro_export]
macro_rules! system {
    [ $( $x:expr ), * $(,)?] => {
        {
            $crate::pts::system::System::from(std::vec![$($x, )*])
        }
    };
}

#[macro_export]
macro_rules! system_append {
    [ $( $x:expr ), * $(,)?] => {
        {
            let mut temp_system = $crate::pts::system::System::default();
            $(
                temp_system.append($x);
            )*
            temp_system
        }
    };
}

#[macro_export]
macro_rules! state_system {
    [
        $(
            $varset:expr; $(
                $sign:literal, $constant: expr $(
                    , $coeff:expr, $var:expr
                )*
            );* $(;)?
        )?] => {
        {
            let temp: $crate::pts::system::StateSystem = $crate::system!(
                $(
                    $(
                        $crate::state_relation!(
                            $sign,
                            $constant,
$varset
                            $(
                                ,
                                $coeff,
                                $var
                            )*



                        ),
                    )*
                )?

            );
            temp
        }
    };
}

pub type RelationIterator<'a, V, C> = std::slice::Iter<'a, Relation<V, C>>;
pub type RelationID = usize;
pub type RelationWithIDIterator<'a, V, C> = std::iter::Enumerate<RelationIterator<'a, V, C>>;

pub type StateSystem = System<ProgramVariable, Constant>;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub struct System<V: Variable, C: Coefficient> {
    conjunction: Vec<Relation<V, C>>,
}

impl<V: Variable, C: Coefficient> System<V, C> {
    pub fn push(&mut self, relation: Relation<V, C>) {
        self.conjunction.push(relation);
    }

    pub fn append(&mut self, system: &mut System<V, C>) {
        self.conjunction.append(&mut system.conjunction);
    }

    pub fn len(&self) -> usize {
        self.conjunction.len()
    }

    pub fn is_empty(&self) -> bool {
        self.conjunction.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Relation<V, C>> {
        self.conjunction.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Relation<V, C>> {
        self.conjunction.get_mut(index)
    }

    pub fn iter(&self) -> RelationIterator<V, C> {
        self.conjunction.iter()
    }

    pub fn iter_with_ids(&self) -> RelationWithIDIterator<V, C> {
        self.iter().enumerate()
    }
}

impl<V: Variable, C: Coefficient> Borrow<Vec<Relation<V, C>>> for System<V, C> {
    fn borrow(&self) -> &Vec<Relation<V, C>> {
        &self.conjunction
    }
}

impl<V: Variable, C: Coefficient> BorrowMut<Vec<Relation<V, C>>> for System<V, C> {
    fn borrow_mut(&mut self) -> &mut Vec<Relation<V, C>> {
        &mut self.conjunction
    }
}

impl<V: Variable, C: Coefficient> From<Vec<Relation<V, C>>> for System<V, C> {
    fn from(value: Vec<Relation<V, C>>) -> Self {
        Self { conjunction: value }
    }
}

impl<V: Variable, C: Coefficient> FromIterator<Relation<V, C>> for System<V, C> {
    fn from_iter<T: IntoIterator<Item = Relation<V, C>>>(iter: T) -> Self {
        Self {
            conjunction: Vec::<Relation<V, C>>::from_iter(iter),
        }
    }
}

impl<V: Variable, C: Coefficient> IntoIterator for System<V, C> {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.conjunction.into_iter()
    }
}

impl<'a, V: Variable, C: Coefficient> IntoIterator for &'a System<V, C> {
    type IntoIter = std::slice::Iter<'a, Relation<V, C>>;
    type Item = &'a Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.conjunction.iter()
    }
}

impl<'a, V: Variable, C: Coefficient> IntoIterator for &'a mut System<V, C> {
    type IntoIter = std::slice::IterMut<'a, Relation<V, C>>;
    type Item = &'a mut Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.conjunction.iter_mut()
    }
}

impl<V: Variable, C: Coefficient> Default for System<V, C> {
    fn default() -> Self {
        Self {
            conjunction: Default::default(),
        }
    }
}

impl<V: Variable + Display> std::fmt::Display for System<V, Constant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //relation (\n relation)*
        let mut iter = self.conjunction.iter();
        match iter.next() {
            Some(line) => write!(f, "{}", line)?,
            _ => (),
        }
        for line in iter {
            write!(f, "\n{}", line)?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    mod macros {
        use crate::{
            program_variables,
            pts::{system::StateSystem, variable::program_variable::ProgramVariables},
            state_relation,
        };

        #[test]
        fn system() {
            let mut variables = program_variables!();

            let mut system = StateSystem::default();
            system.push(state_relation!(
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
                "d",
            ));

            system.push(state_relation!("<=", -1.0, &mut variables, -2.4, "a"));

            system.push(state_relation!(
                "=",
                0.0,
                &mut variables,
                0.0,
                "a",
                0.0,
                "b",
                0.0,
                "c"
            ));

            system.push(state_relation!("!=", 111.111));

            assert_eq!(
                system,
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
            );
        }

        #[test]
        fn system_append() {
            let mut variables: ProgramVariables = program_variables!();
            let a: StateSystem = system!(
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
            );
            let b = system_append!(
                &mut system!(state_relation!(
                    ">",
                    0.0,
                    &mut variables,
                    12.4,
                    "a",
                    -3.5,
                    "b",
                    2.4,
                    "c",
                    0.0,
                    "d"
                ),),
                &mut system!(
                    state_relation!("<=", -1.0, &mut variables, -2.4, "a"),
                    state_relation!("=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c"),
                ),
                &mut system!(),
                &mut system!(state_relation!("!=", 111.111))
            );
            assert_eq!(a, b);
        }

        #[test]
        fn state_system() {
            let mut variables: ProgramVariables = program_variables!();
            assert_eq!(
                state_system!(
                    &mut variables;
                    ">", 0.0, 12.4, "a", -3.5, "b", 2.4, "c", -0.0, "d";
                    "<=", -1.0, -2.4, "a";
                    "=", 0.0, 0.0, "a", 0.0, "b", 0.0, "c";
                    "!=", 111.111
                ),
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
            );
        }
    }

    mod label {
        use crate::{
            program_variables,
            pts::{system::StateSystem, variable::program_variable::ProgramVariables},
            relation,
        };

        #[test]
        fn zeroes() {
            let system: StateSystem = system!(relation!("<=", 0.0), relation!("<=", 0.0),);
            assert_eq!(system.to_string(), "0 <= 0\n0 <= 0");
        }
        #[test]
        fn vars() {
            let mut variables: ProgramVariables = program_variables!("a");
            let system = state_system!(&mut variables;
                "<=", 0.0, 1.0, "a";
                "<=", 0.0, -1.0, "a";
            );
            assert_eq!(system.to_string(), "a <= 0\na >= 0");
        }
        #[test]
        fn neg() {
            let mut variables: ProgramVariables = program_variables!("test1", "test2", "test3",);
            let system = state_system!(
                &mut variables;
                "<",
                0.0,
                -1.0,
                "test1",
                0.0,
                "test2",
                0.0,
                "test3"
            );
            assert_eq!(system.to_string(), "test1 > 0");
        }
    }
}
