use std::fmt;

// LinearPolynomial index
pub type VariableID = usize;

pub const CONSTANT_ID: VariableID = 0;

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
    // Returns None when theres no Variable with such ID
    pub fn get_variable(&self, id: VariableID) -> Option<&Variable> {
        if id == 0 {
            None
        } else {
            self.variables.get(id - 1)
        }
    }

    // Find variable and return its id, if not found, add it
    pub fn get_or_push(&mut self, var: &Variable) -> VariableID {
        let index = self.get_id(var);
        if index.is_none() {
            self.variables.push(var.clone());
            // -1 because its after push, +1 because the polynomial indexing is shifted
            self.variables.len() - 1 + 1
        } else {
            index.unwrap()
        }
    }

    // Find variable and return its id, if not found, return None
    pub fn get_id(&self, var: &Variable) -> Option<VariableID> {
        self.variables
            .iter()
            .enumerate()
            .find(|(_, element)| *element == var)
            .map(|(index, _)| index + 1)
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
                varmap.get_or_push(&Variable::new("test"));
                varmap.get_or_push(&Variable::new("apple"));
                varmap.get_or_push(&Variable::new("testington"));
                varmap.get_or_push(&Variable::new("apple"));
                varmap.get_or_push(&Variable::new("testington"));
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
        let a = Variable::new("a");
        assert_eq!(map.get_id(&a), None);
        assert_eq!(map.get_variable(0), None);
        assert_eq!(map.get_variable(1), None);
        map.get_or_push(&Variable::new("b"));
        assert_eq!(map.get_id(&a), None);
        assert_eq!(map.get_variable(0), None);
        assert_eq!(map.get_variable(2), None);
    }

    #[test]
    fn basic() {
        let mut map = VariableMap::default();
        let a = Variable::new("a");
        assert_eq!(map.get_or_push(&a), 1);
        assert_eq!(map.get_variable(1), Some(&a));
        assert_eq!(map.get_or_push(&a), 1);
        assert_eq!(map.get_variable(1), Some(&a));
        let b = Variable::new("b");
        assert_eq!(map.get_or_push(&b), 2);
        let c = Variable::new("c");
        assert_eq!(map.get_or_push(&c), 3);
        assert_eq!(map.get_variable(1), Some(&a));
        assert_eq!(map.get_variable(2), Some(&b));
        assert_eq!(map.get_variable(3), Some(&c));
    }
}
