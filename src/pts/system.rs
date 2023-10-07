use std::ops::Not;
use std::slice::Iter;

use super::relation::Relation;
use super::variable_map::VariableMap;
use super::DisplayLabel;

// Rust Book 19.5 Macros: example vec! macro
#[macro_export]
macro_rules! system {
    [ $( $x:expr ), * $(,)?] => {
        {
            let mut temp_system = $crate::pts::system::System::default();
            $(
                temp_system.push($x);
            )*
            temp_system
        }
    };
}

// test only, uses mock_relation
#[cfg(test)]
#[macro_export]
macro_rules! mock_invariant {
    [ $( $sign:literal, $( $x:expr ),* );* $(;)?] => {
        {
            $crate::system![$( $crate::mock_relation![$sign, $($x, )*], )*]
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

pub type RelationIter<'a> = Iter<'a, Relation>;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct System {
    relations: Vec<Relation>,
}

impl System {
    pub fn push(&mut self, inequality: Relation) {
        self.relations.push(inequality);
    }

    pub fn append(&mut self, system: &mut System) {
        self.relations.append(&mut system.relations);
    }

    pub fn len(&self) -> usize {
        self.relations.len()
    }

    pub fn get(&self, index: usize) -> Option<&Relation> {
        self.relations.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Relation> {
        self.relations.get_mut(index)
    }

    pub fn iter<'a>(&'a self) -> RelationIter<'a> {
        self.relations.iter()
    }
}

impl DisplayLabel for System {
    fn label(&self, variable_map: &VariableMap) -> String {
        //ineq (\n ineq)*
        let mut label = String::default();
        let mut iter = self.relations.iter().map(|x| x.label(variable_map));
        match iter.next() {
            Some(line) => label.push_str(line.as_str()),
            _ => (),
        }
        for line in iter {
            label.push_str("\n");
            label.push_str(line.as_str());
        }
        label
    }
}

impl Not for System {
    type Output = Self;
    fn not(self) -> Self::Output {
        System {
            relations: self.relations.into_iter().map(|x| !x).collect(),
        }
    }
}

#[cfg(test)]
mod tests {

    mod macros {
        use crate::{mock_relation, pts::system::System};

        #[test]
        fn system() {
            let mut system = System::default();
            system.push(mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0));

            system.push(mock_relation!("<=", -1.0, -2.4));

            system.push(mock_relation!("=", 0.0, 0.0, 0.0, 0.0));

            system.push(mock_relation!("!=", 111.111));

            assert_eq!(
                system,
                system!(
                    mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0),
                    mock_relation!("<=", -1.0, -2.4),
                    mock_relation!("=", 0.0, 0.0, 0.0, 0.0),
                    mock_relation!("!=", 111.111)
                ),
            );
        }

        #[test]
        fn system_append() {
            assert_eq!(
                system!(
                    mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0),
                    mock_relation!("<=", -1.0, -2.4),
                    mock_relation!("=", 0.0, 0.0, 0.0, 0.0),
                    mock_relation!("!=", 111.111)
                ),
                system_append!(
                    &mut system!(mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0),),
                    &mut system!(
                        mock_relation!("<=", -1.0, -2.4),
                        mock_relation!("=", 0.0, 0.0, 0.0, 0.0),
                    ),
                    &mut system!(),
                    &mut system!(mock_relation!("!=", 111.111))
                )
            );
        }

        #[test]
        fn mock_invariant() {
            assert_eq!(
                system!(
                    mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0),
                    mock_relation!("<=", -1.0, -2.4),
                    mock_relation!("=", 0.0, 0.0, 0.0, 0.0),
                    mock_relation!("!=", 111.111)
                ),
                mock_invariant!(
                    ">", 0.0, 12.4, -3.5, 2.4, -0.0;
                    "<=", -1.0, -2.4;
                    "=", 0.0, 0.0, 0.0, 0.0;
                    "!=", 111.111
                ),
            );
        }
    }

    mod label {
        use crate::{mock_relation, mock_varmap, pts::DisplayLabel};

        #[test]
        fn zeroes() {
            let system = system!(mock_relation!("<=", 0.0), mock_relation!("<=", 0.0),);
            let map = mock_varmap!();
            assert_eq!(system.label(&map), "0 <= 0\n0 <= 0");
        }
        #[test]
        fn vars() {
            let system = system!(
                mock_relation!("<=", 0.0, 1.0),
                mock_relation!("<=", 0.0, -1.0),
            );
            let map = mock_varmap!("a", "b");
            assert_eq!(system.label(&map), "a <= 0\na >= 0");
        }
        #[test]
        fn neg() {
            let system = system!(mock_relation!("<", 0.0, -1.0, 0.0, 0.0));
            let map = mock_varmap!("test1", "test2", "test3",);
            assert_eq!(system.label(&map), "test1 > 0");
        }
    }
}
