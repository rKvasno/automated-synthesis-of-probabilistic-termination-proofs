use std::fmt;

#[derive(Clone, Debug)]
pub struct Variable {
    name: Box<str>
}

// linear search in Vec is faster than logarithmic for small number of elements,
// mostly due to caching
#[derive(Debug)]
pub struct VariableMap {
    variables: Vec<Variable>
}

impl Variable {
    pub fn new(name: &str) -> Self {
        Variable{ name: name.to_string().into_boxed_str() }
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

impl VariableMap {
    pub fn new() -> Self {
        VariableMap { variables: vec!() }
    }

    pub fn len(&self) -> usize {
        self.variables.len()
    }

    // Return the variable with specified index in a polynomial
    pub fn get_variable(&self, index: usize) -> Option<&Variable> {
        self.variables.get(index - 1)
    }

    // Find variable and return its index, if not found, add it
    pub fn find_or_add(&mut self, var: Variable) -> usize {
        let index = self.variables.iter().enumerate().find(|x| x.1.name.as_ref() == var.as_str()).map(|x| x.0 + 1);
        if index.is_none() {
            self.variables.push(Variable::new(var.as_str()));
            // -1 because its after push, +1 because the polynomial indexing is shifted
            self.variables.len() - 1 + 1
        }
        else {
            index.unwrap()
        }
    }


    // Find variable and return its index, if not found, return None 
    pub fn get_index(&self, var: &Variable) -> Option<usize> {
        self.variables.iter().enumerate().find(|x| x.1.name.as_ref() == var.as_str()).map(|x| x.0 + 1)
    }
}

#[derive(Clone, Debug)]
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

