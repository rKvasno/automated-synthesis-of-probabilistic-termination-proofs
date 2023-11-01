use std::{
    borrow::{Borrow, BorrowMut},
    ops::Not,
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
                        $crate::relation!(
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

pub type RelationIter<'a, V, C> = std::slice::Iter<'a, Relation<V, C>>;
pub type StateSystem = System<ProgramVariable, Constant>;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Clone)]
pub struct System<V: Variable, C: Coefficient> {
    data: Vec<Relation<V, C>>,
}

impl<V: Variable, C: Coefficient> System<V, C> {
    pub fn push(&mut self, inequality: Relation<V, C>) {
        self.data.push(inequality);
    }

    pub fn append(&mut self, system: &mut System<V, C>) {
        self.data.append(&mut system.data);
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn get(&self, index: usize) -> Option<&Relation<V, C>> {
        self.data.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Relation<V, C>> {
        self.data.get_mut(index)
    }

    pub fn iter(&self) -> RelationIter<V, C> {
        self.data.iter()
    }
}

impl<V: Variable, C: Coefficient> Borrow<Vec<Relation<V, C>>> for System<V, C> {
    fn borrow(&self) -> &Vec<Relation<V, C>> {
        &self.data
    }
}

impl<V: Variable, C: Coefficient> BorrowMut<Vec<Relation<V, C>>> for System<V, C> {
    fn borrow_mut(&mut self) -> &mut Vec<Relation<V, C>> {
        &mut self.data
    }
}

impl<V: Variable, C: Coefficient> From<Vec<Relation<V, C>>> for System<V, C> {
    fn from(value: Vec<Relation<V, C>>) -> Self {
        Self { data: value }
    }
}

impl<V: Variable, C: Coefficient> FromIterator<Relation<V, C>> for System<V, C> {
    fn from_iter<T: IntoIterator<Item = Relation<V, C>>>(iter: T) -> Self {
        Self {
            data: Vec::<Relation<V, C>>::from_iter(iter),
        }
    }
}

impl<V: Variable, C: Coefficient> IntoIterator for System<V, C> {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, V: Variable, C: Coefficient> IntoIterator for &'a System<V, C> {
    type IntoIter = std::slice::Iter<'a, Relation<V, C>>;
    type Item = &'a Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl<'a, V: Variable, C: Coefficient> IntoIterator for &'a mut System<V, C> {
    type IntoIter = std::slice::IterMut<'a, Relation<V, C>>;
    type Item = &'a mut Relation<V, C>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.iter_mut()
    }
}

impl<V: Variable, C: Coefficient> Default for System<V, C> {
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<V: Variable, C: Coefficient> std::fmt::Display for System<V, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //ineq (\n ineq)*
        let mut iter = self.data.iter();
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

impl<V: Variable, C: Coefficient> Not for System<V, C> {
    type Output = Self;
    fn not(self) -> Self::Output {
        System {
            data: self.data.into_iter().map(|x| !x).collect(),
        }
    }
}

#[cfg(test)]
mod tests {

    mod macros {
        use crate::{pts::system::StateSystem, relation, variables};

        #[test]
        fn system() {
            let mut variables = variables!();

            let mut system = StateSystem::default();
            system.push(relation!(
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

            system.push(relation!("<=", -1.0, &mut variables, -2.4, "a"));

            system.push(relation!(
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

            system.push(relation!("!=", 111.111));

            assert_eq!(
                system,
                system!(
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
                ),
            );
        }

        #[test]
        fn system_append() {
            let mut variables = variables!();
            let a: StateSystem = system!(
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
            );
            let b = system_append!(
                &mut system!(relation!(
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
                    relation!("<=", -1.0, &mut variables, -2.4, "a"),
                    relation!("=", 0.0, &mut variables, 0.0, "a", 0.0, "b", 0.0, "c"),
                ),
                &mut system!(),
                &mut system!(relation!("!=", 111.111))
            );
            assert_eq!(a, b);
        }

        #[test]
        fn state_system() {
            let mut variables = variables!();
            assert_eq!(
                state_system!(
                    &mut variables;
                    ">", 0.0, 12.4, "a", -3.5, "b", 2.4, "c", -0.0, "d";
                    "<=", -1.0, -2.4, "a";
                    "=", 0.0, 0.0, "a", 0.0, "b", 0.0, "c";
                    "!=", 111.111
                ),
                system!(
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
                ),
            );
        }
    }

    mod label {
        use crate::{pts::system::StateSystem, relation, variables};

        #[test]
        fn zeroes() {
            let system: StateSystem = system!(relation!("<=", 0.0), relation!("<=", 0.0),);
            assert_eq!(system.to_string(), "0 <= 0\n0 <= 0");
        }
        #[test]
        fn vars() {
            let mut variables = variables!("a");
            let system = state_system!(&mut variables;
                "<=", 0.0, 1.0, "a";
                "<=", 0.0, -1.0, "a";
            );
            assert_eq!(system.to_string(), "a <= 0\na >= 0");
        }
        #[test]
        fn neg() {
            let mut variables = variables!("test1", "test2", "test3",);
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
