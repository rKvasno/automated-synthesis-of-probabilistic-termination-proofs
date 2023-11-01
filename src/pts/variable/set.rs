use std::{borrow::Borrow, collections::HashSet, hash::Hash};

use super::Variable;

#[macro_export]
macro_rules! variables{
    [ $( $x:expr ),+ $(,)?] => {
        {
            let mut tmp = $crate::pts::variable::set::VariableSet::default();
            $(
                $crate::pts::variable::Variable::new(&mut tmp, $x);
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableSet<V: Variable> {
    data: HashSet<V>,
}

impl<V: Variable> VariableSet<V> {
    pub fn with_capacity(n: usize) -> Self {
        Self {
            data: HashSet::with_capacity(n),
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
        // TODO uprade to Hashset::get_or_insert if it becomes stable
        let tmp: V = variable.into();
        self.insert(tmp.to_owned());
        self.data.get(tmp.borrow()).unwrap().clone()
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

impl<V: Variable> Default for VariableSet<V> {
    fn default() -> Self {
        Self {
            data: HashSet::default(),
        }
    }
}

// Only immut borrow, since we don't want elements removed
impl<V: Variable> Borrow<HashSet<V>> for VariableSet<V> {
    fn borrow(&self) -> &HashSet<V> {
        &self.data
    }
}

impl<V: Variable> From<HashSet<V>> for VariableSet<V> {
    fn from(value: HashSet<V>) -> Self {
        Self { data: value }
    }
}

impl<V: Variable> FromIterator<V> for VariableSet<V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self {
            data: HashSet::from_iter(iter.into_iter().map(Into::into)),
        }
    }
}

impl<V: Variable> IntoIterator for VariableSet<V> {
    type Item = V;
    type IntoIter = std::collections::hash_set::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, V: Variable> IntoIterator for &'a VariableSet<V> {
    type Item = &'a V;
    type IntoIter = std::collections::hash_set::Iter<'a, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::pts::variable::program_variable::ProgramVariables;

    mod macros {

        mod variables {
            use std::{borrow::Borrow, collections::HashSet, rc::Rc};

            use crate::pts::variable::{
                program_variable::{ProgramVariable, ProgramVariables},
                Variable,
            };

            #[test]
            fn non_empty() {
                let mut variables = ProgramVariables::default();
                ProgramVariable::new(&mut variables, "test");
                ProgramVariable::new(&mut variables, "apple");
                ProgramVariable::new(&mut variables, "testington");
                ProgramVariable::new(&mut variables, "apple");
                ProgramVariable::new(&mut variables, "testington");
                assert_eq!(variables, variables!("test", "apple", "testington"));

                let mut variables = HashSet::<ProgramVariable>::default();
                variables.insert(Rc::<str>::from("testington").into());
                variables.insert(Rc::<str>::from("test").into());
                variables.insert(Rc::<str>::from("apple").into());
                assert_eq!(
                    &variables,
                    variables!("test", "apple", "testington").borrow()
                );
            }

            #[test]
            fn empty() {
                let tmp: ProgramVariables = variables!();
                assert_eq!(ProgramVariables::default(), tmp);
            }
        }
    }

    #[test]
    fn insert() {
        let mut variables = ProgramVariables::default();
        assert_eq!(variables, variables!());
        assert!(variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, variables!("testington"));
        assert!(!variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, variables!("testington"));
        assert!(variables.insert(Rc::<str>::from("test")));
        assert_eq!(variables, variables!("test", "testington"));
        assert!(variables.insert(Rc::<str>::from("apple")));
        assert_eq!(variables, variables!("test", "apple", "testington"));
        assert!(!variables.insert(Rc::<str>::from("apple")));
        assert_eq!(variables, variables!("test", "apple", "testington"));
    }

    #[test]
    fn get() {
        let set: ProgramVariables = variables!();
        assert!(set.get("a").is_none());
        assert!(set.get("b").is_none());

        let set: ProgramVariables = variables!("test", "x", "variable");
        assert!(set.get("a").is_none());
        assert!(set.get("b").is_none());

        let set: ProgramVariables = variables!("a");
        assert_eq!(&**set.get("a").unwrap(), "a");
        assert!(set.get("b").is_none());

        let set: ProgramVariables = variables!("test", "b", "variable");
        assert!(set.get("a").is_none());
        assert_eq!(&**set.get("b").unwrap(), "b");
    }

    #[test]
    fn contains() {
        let set: ProgramVariables = variables!();
        assert!(!set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = variables!("test", "x", "variable");
        assert!(!set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = variables!("a");
        assert!(set.contains("a"));
        assert!(!set.contains("b"));

        let set: ProgramVariables = variables!("test", "b", "variable");
        assert!(!set.contains("a"));
        assert!(set.contains("b"));
    }

    #[test]
    fn get_or_insert() {
        let mut variables = ProgramVariables::default();
        assert_eq!(variables, variables!());
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("testington")),
            Rc::<str>::from("testington").into()
        );
        assert_eq!(variables, variables!("testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("testington")),
            Rc::<str>::from("testington").into()
        );
        assert!(!variables.insert(Rc::<str>::from("testington")));
        assert_eq!(variables, variables!("testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("test")),
            Rc::<str>::from("test").into()
        );
        assert_eq!(variables, variables!("test", "testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("apple")),
            Rc::<str>::from("apple").into()
        );
        assert_eq!(variables, variables!("test", "apple", "testington"));
        assert_eq!(
            variables.get_or_insert(Rc::<str>::from("apple")),
            Rc::<str>::from("apple").into()
        );
        assert_eq!(variables, variables!("test", "apple", "testington"));
    }
}
