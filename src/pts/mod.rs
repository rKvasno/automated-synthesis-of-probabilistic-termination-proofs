mod guard;
mod location;
mod transition;

use crate::pts::location::{Location, LocationHandle};
use std::cell::RefCell;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Variable {
    name: Box<str>
}

#[derive(Debug)]
pub struct LinearPolynomial {
    coefficients: Vec<f64>
}

// linear search in Vec is faster than logarithmic for small number of elements,
// mostly due to caching
#[derive(Debug)]
pub struct VariableMap {
    variables: Vec<Variable>
}

#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct PTS<'a> {
    locations: Vec<RefCell<Location<'a>>>,
    variables: VariableMap,
    initial: Option<LocationHandle<'a>>,
}

impl<'a> PTS<'a> {
    fn new(initial: Option<LocationHandle<'a>>) -> Self {
        PTS { locations: vec!(), variables: VariableMap::new(), initial }
    }
}



impl Variable {
    fn new(name: &str) -> Self {
        Variable{ name: name.to_string().into_boxed_str() }
    }

    fn as_str(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl LinearPolynomial {
    fn new() -> Self {
        LinearPolynomial { coefficients: vec!() }
    }

    fn add_term(&mut self, 
                map: &VariableMap,
                var: &Variable,
                coefficient: f64) -> Result<(), VariableError>{
        let index = map.variable_to_index(var.as_str());

        // if variable is not in the map, its not a program variable
        if index.is_none() {
            Err(VariableError::new(var))   
        }
        else {
            if self.coefficients.len() < map.variables.len() {
                self.coefficients.resize(map.variables.len(), 0.0);
            }
            self.coefficients[index.unwrap()] += coefficient;
            Ok(())
        }
    }
}

impl VariableMap {
    fn new() -> Self {
        VariableMap { variables: vec!() }
    }

    // Return the variable with specified index in a polynomial
    fn index_to_variable(&self, index: usize) -> Option<&str> {
        if index - 1 < self.variables.len() {
            Some(&self.variables[index - 1].name)
        }
        else {
            None
        }
    }

    // Find variable and return its index, if not found, add it
    fn find_or_add(&mut self, var: &str) -> usize {
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
    fn variable_to_index(&self, variable: &str) -> Option<usize> {
        self.variables.iter().enumerate().find(|x| x.1.name.as_ref() == variable).map(|x| x.0 + 1)

    }
}

#[derive(Clone, Debug)]
struct VariableError(Variable);

impl VariableError {
    fn new(var: &Variable) -> Self {
        VariableError(var.clone())
    }
}

impl fmt::Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\" is not a program variable!", self.0)
    }
}



// #[cfg(test)]
// mod tests {
//     use std::mem;
//     use super::PTS;
//     
//     #[test]
//     fn align_pts() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<PTS>(), 32);
//         }
//     }
//     
//     #[test]
//     fn size_pts() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<PTS>(), 32);
//         }
//     }
// }

