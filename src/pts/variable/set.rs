use std::{
    borrow::Borrow,
    collections::{hash_map::RandomState, HashSet},
    hash::{BuildHasher, Hash},
};

use super::Variable;

#[macro_export]
macro_rules! variables{
    [ $( $x:expr ),+ $(,)?] => {
        {
            let mut tmp = $crate::pts::variable::set::VariableSet::default();
            $(
                tmp.insert($x);
            )+
            tmp
        }
    };
    [] => {
        {
            $crate::pts::variable::set::VariableSet::default()
        }
    }
}

#[derive(Debug, Eq, Clone)]
pub struct VariableSet<V: Variable, S: BuildHasher = RandomState> {
    data: HashSet<V, S>,
}

impl<V: Variable, S: BuildHasher> PartialEq for VariableSet<V, S> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<V: Variable, S: BuildHasher + Default> VariableSet<V, S> {
    pub fn with_capacity(n: usize) -> Self {
        Self {
            data: HashSet::with_capacity_and_hasher(n, S::default()),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit()
    }

    pub fn insert<T: Into<V>>(&mut self, variable: T) -> bool {
        self.data.insert(variable.into())
    }

    pub fn get_or_insert<K>(&mut self, variable: K) -> V
    where
        K: Into<V> + ToOwned + ?Sized,
    {
        // TODO uprade to Indexset::get_or_insert if it becomes stable
        let tmp: V = variable.into();
        self.insert(tmp.to_owned());
        self.data.get(&tmp).unwrap().clone()
    }

    pub fn get<K>(&self, variable: &K) -> Option<&V>
    where
        V: Borrow<K>,
        K: Hash + Eq + ?Sized,
    {
        self.data.get(variable)
    }

    pub fn contains<K>(&self, variable: &K) -> bool
    where
        V: Borrow<K>,
        K: Hash + Eq + ?Sized,
    {
        self.data.contains(variable)
    }
}

impl<V: Variable, S: BuildHasher + Default> Default for VariableSet<V, S> {
    fn default() -> Self {
        Self {
            data: HashSet::default(),
        }
    }
}

// Only immut borrow, since we don't want elements removed
impl<V: Variable, S: BuildHasher + Default> Borrow<HashSet<V, S>> for VariableSet<V, S> {
    fn borrow(&self) -> &HashSet<V, S> {
        &self.data
    }
}

impl<V: Variable, S: BuildHasher + Default> From<HashSet<V, S>> for VariableSet<V, S> {
    fn from(value: HashSet<V, S>) -> Self {
        Self { data: value }
    }
}

impl<V: Variable, S: BuildHasher + Default> FromIterator<V> for VariableSet<V, S> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self {
            data: HashSet::from_iter(iter.into_iter().map(Into::into)),
        }
    }
}

impl<V: Variable, S: BuildHasher> IntoIterator for VariableSet<V, S> {
    type Item = V;
    type IntoIter = std::collections::hash_set::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, V: Variable, S: BuildHasher> IntoIterator for &'a VariableSet<V, S> {
    type Item = &'a V;
    type IntoIter = std::collections::hash_set::Iter<'a, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{program_variables, pts::variable::program_variable::ProgramVariables};

    mod macros {

        mod variables {
            use std::{borrow::Borrow, collections::HashSet, rc::Rc};

            use crate::{
                program_var, program_variables,
                pts::variable::program_variable::{ProgramVariable, ProgramVariables},
            };

            #[test]
            fn non_empty() {
                let mut variables: ProgramVariables = ProgramVariables::default();
                program_var!(&mut variables, "test");
                program_var!(&mut variables, "apple");
                program_var!(&mut variables, "testington");
                program_var!(&mut variables, "apple");
                program_var!(&mut variables, "testington");
                assert_eq!(variables, program_variables!("test", "apple", "testington"));

                let mut variables: ProgramVariables = HashSet::<ProgramVariable>::default().into();
                variables.insert(Rc::<str>::from("testington"));
                variables.insert(Rc::<str>::from("test"));
                variables.insert(Rc::<str>::from("apple"));
                assert_eq!(
                    &variables,
                    program_variables!("test", "apple", "testington").borrow()
                );
            }

            #[test]
            fn empty() {
                let tmp: ProgramVariables = program_variables!();
                assert_eq!(ProgramVariables::default(), tmp);
            }
        }
    }

    #[test]
    fn insert() {
        let mut variables: ProgramVariables = ProgramVariables::default();
        assert_eq!(variables, program_variables!());
        assert!(variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, program_variables!("testington"));
        assert!(!variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, program_variables!("testington"));
        assert!(variables.insert(Rc::<str>::from("test")));
        assert_eq!(variables, program_variables!("test", "testington"));
        assert!(variables.insert(Rc::<str>::from("apple")));
        assert_eq!(variables, program_variables!("test", "apple", "testington"));
        assert!(!variables.insert(Rc::<str>::from("apple")));
        assert_eq!(variables, program_variables!("test", "apple", "testington"));
    }

    #[test]
    fn get() {
        let set: ProgramVariables = program_variables!();
        assert!(set.get("a").is_none());
        assert!(set.get("b").is_none());

        let set: ProgramVariables = program_variables!("test", "x", "variable");
        assert!(set.get("a").is_none());
        assert!(set.get("b").is_none());

        let set: ProgramVariables = program_variables!("a");
        assert_eq!(&**set.get("a").unwrap(), "a");
        assert!(set.get("b").is_none());

        let set: ProgramVariables = program_variables!("test", "b", "variable");
        assert!(set.get("a").is_none());
        assert_eq!(&**set.get("b").unwrap(), "b");
    }

    #[test]
    fn contains() {
        let set: ProgramVariables = program_variables!();
        assert!(!set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = program_variables!("test", "x", "variable");
        assert!(!set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = program_variables!("a");
        assert!(set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = program_variables!("test", "b", "variable");
        assert!(!set.contains("a"));
        assert!(set.contains("b"));
    }

    #[test]
    fn get_or_insert() {
        let mut variables: ProgramVariables = ProgramVariables::default();
        assert_eq!(variables, program_variables!());
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("testington")),
            Rc::<str>::from("testington").into()
        );
        assert_eq!(variables, program_variables!("testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("testington")),
            Rc::<str>::from("testington").into()
        );
        assert!(!variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, program_variables!("testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("test")),
            Rc::<str>::from("test").into()
        );
        assert_eq!(variables, program_variables!("test", "testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("apple")),
            Rc::<str>::from("apple").into()
        );
        assert_eq!(variables, program_variables!("test", "apple", "testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("apple")),
            Rc::<str>::from("apple").into()
        );
        assert_eq!(variables, program_variables!("test", "apple", "testington"));
    }
}
