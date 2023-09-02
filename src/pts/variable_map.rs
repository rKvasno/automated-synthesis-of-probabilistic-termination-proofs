use std::fmt;

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
    pub fn get_variable(&self, index: usize) -> Option<&Variable> {
        if index == 0 {
            None
        } else {
            self.variables.get(index - 1)
        }
    }

    // Find variable and return its index, if not found, add it
    pub fn find_or_add(&mut self, var: Variable) -> usize {
        let index = self.get_index(&var);
        if index.is_none() {
            self.variables.push(Variable::new(var.as_str()));
            // -1 because its after push, +1 because the polynomial indexing is shifted
            self.variables.len() - 1 + 1
        } else {
            index.unwrap()
        }
    }

    // Find variable and return its index, if not found, return None
    pub fn get_index(&self, var: &Variable) -> Option<usize> {
        self.variables
            .iter()
            .enumerate()
            .find(|(_, element)| element == &var)
            .map(|(index, _)| index + 1)
    }
}

#[cfg(test)]
impl VariableMap {
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

#[cfg(test)]
mod tests {
    use super::{Variable, VariableMap};

    #[test]
    fn out_of_bounds() {
        let mut map = VariableMap::default();
        let a = Variable::new("a");
        assert_eq!(map.get_index(&a), None);
        assert_eq!(map.get_variable(0), None);
        assert_eq!(map.get_variable(1), None);
        map.find_or_add(Variable::new("b"));
        assert_eq!(map.get_index(&a), None);
        assert_eq!(map.get_variable(0), None);
        assert_eq!(map.get_variable(2), None);
    }

    #[test]
    fn basic() {
        let mut map = VariableMap::default();
        let a = Variable::new("a");
        assert_eq!(map.find_or_add(a.clone()), 1);
        assert_eq!(map.get_variable(1), Some(&a));
        assert_eq!(map.find_or_add(a.clone()), 1);
        assert_eq!(map.get_variable(1), Some(&a));
        let b = Variable::new("b");
        assert_eq!(map.find_or_add(b.clone()), 2);
        let c = Variable::new("c");
        assert_eq!(map.find_or_add(c.clone()), 3);
        assert_eq!(map.get_variable(1), Some(&a));
        assert_eq!(map.get_variable(2), Some(&b));
        assert_eq!(map.get_variable(3), Some(&c));
    }
}
