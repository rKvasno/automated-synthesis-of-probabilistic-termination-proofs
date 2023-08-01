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
    pub fn index_to_variable(&self, index: usize) -> Option<&str> {
        if index - 1 < self.variables.len() {
            Some(&self.variables[index - 1].name)
        }
        else {
            None
        }
    }

    // Find variable and return its index, if not found, add it
    pub fn find_or_add(&mut self, var: &str) -> usize {
        let index = self.variables.iter().enumerate().find(|x| x.1.name.as_ref() == var).map(|x| x.0 + 1);
        if index.is_none() {
            self.variables.push(Variable::new(var));
            // -1 because its after push, +1 because the polynomial indexing is shifted
            self.variables.len() - 1 + 1
        }
        else {
            index.unwrap()
        }
    }


    // Find variable and return its index, if not found, return None 
    pub fn variable_to_index(&self, variable: &str) -> Option<usize> {
        self.variables.iter().enumerate().find(|x| x.1.name.as_ref() == variable).map(|x| x.0 + 1)

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

