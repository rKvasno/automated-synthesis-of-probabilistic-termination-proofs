use std::fmt;

// test only, breaks interface
#[cfg(test)]
#[macro_export]
macro_rules! mock_varmap{
    [ $( $x:expr ),* $(,)?] => {
        {
            $crate::pts::variable_map::VariableMap::mock(std::vec![$($crate::pts::variable_map::Variable::new($x),)*])
        }
    };
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    name: Box<str>,
}

impl Variable {
    pub fn new(name: &str) -> Self {
        Variable {
            name: name.to_string().into_boxed_str(),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// linear search in Vec is faster than logarithmic for small number of elements,
// mostly due to caching
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct VariableMap {
    variables: Vec<Variable>,
}

impl VariableMap {
    pub fn len(&self) -> usize {
        self.variables.len()
    }

    // Return the variable with specified index in a polynomial
    pub fn get_variable(&self, index: usize) -> Option<Option<&Variable>> {
        if index == 0 {
            Some(None)
        } else {
            self.variables.get(index - 1).map(Some)
        }
    }

    // Find variable and return its index, if not found, add it
    pub fn get_or_push(&mut self, var: &Option<Variable>) -> usize {
        let index = self.get_index(&var);
        if index.is_none() {
            self.variables.push(var.as_ref().unwrap().clone());
            // -1 because its after push, +1 because the polynomial indexing is shifted
            self.variables.len() - 1 + 1
        } else {
            index.unwrap()
        }
    }

    // Find variable and return its index, if not found, return None
    pub fn get_index(&self, var: &Option<Variable>) -> Option<usize> {
        if var.is_none() {
            Some(0)
        } else {
            self.variables
                .iter()
                .enumerate()
                .find(|(_, element)| *element == var.as_ref().unwrap())
                .map(|(index, _)| index + 1)
        }
    }

    pub fn iter(&self) -> VariableIterator {
        self.variables.iter()
    }

    #[cfg(test)]
    pub fn mock(variables: Vec<Variable>) -> Self {
        VariableMap { variables }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableError(Variable);

impl VariableError {
    pub fn new(var: &Variable) -> Self {
        VariableError(var.clone())
    }
}

impl fmt::Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\" is not a program variable!", self.0)
    }
}

type VariableIterator<'a> = std::slice::Iter<'a, Variable>;

#[cfg(test)]
mod tests {
    use super::{Variable, VariableMap};

    mod macros {
        mod varmap {
            use crate::pts::variable_map::{Variable, VariableMap};

            #[test]
            fn non_empty() {
                let mut varmap = VariableMap::default();
                varmap.get_or_push(&Some(Variable::new("test")));
                varmap.get_or_push(&Some(Variable::new("apple")));
                varmap.get_or_push(&Some(Variable::new("testington")));
                varmap.get_or_push(&Some(Variable::new("apple")));
                varmap.get_or_push(&Some(Variable::new("testington")));
                assert_eq!(varmap, mock_varmap!("test", "apple", "testington"));
            }

            #[test]
            fn empty() {
                assert_eq!(VariableMap::default(), mock_varmap!());
            }
        }
    }

    #[test]
    fn out_of_bounds() {
        let mut map = VariableMap::default();
        let a = Some(Variable::new("a"));
        assert_eq!(map.get_index(&a), None);
        assert_eq!(map.get_variable(0), Some(None));
        assert_eq!(map.get_variable(1), None);
        map.get_or_push(&Some(Variable::new("b")));
        assert_eq!(map.get_index(&a), None);
        assert_eq!(map.get_variable(0), Some(None));
        assert_eq!(map.get_variable(2), None);
    }

    #[test]
    fn basic() {
        let mut map = VariableMap::default();
        let a = Some(Variable::new("a"));
        assert_eq!(map.get_or_push(&a), 1);
        assert_eq!(map.get_variable(1), Some(a.as_ref()));
        assert_eq!(map.get_or_push(&a), 1);
        assert_eq!(map.get_variable(1), Some(a.as_ref()));
        let b = Some(Variable::new("b"));
        assert_eq!(map.get_or_push(&b), 2);
        let c = Some(Variable::new("c"));
        assert_eq!(map.get_or_push(&c), 3);
        assert_eq!(map.get_variable(1), Some(a.as_ref()));
        assert_eq!(map.get_variable(2), Some(b.as_ref()));
        assert_eq!(map.get_variable(3), Some(c.as_ref()));
    }
}
