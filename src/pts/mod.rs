mod guard;
mod location;
mod transition;

use crate::pts::location::{Location, LocationHandle};
use std::cell::RefCell;

pub struct Variable {
    name: Box<str>
}

pub struct LinearPolynomial {
    coefficients: Vec<f64>
}

// linear search in Vec is faster than logarithmic for small number of elements,
// mostly due to caching
pub struct VariableMap {
    variables: Vec<Variable>
}

#[repr(align(32))] // 32 bytes
pub struct PTS<'a> {
    locations: Vec<RefCell<Location<'a>>>,
    variables: VariableMap,
    initial: Option<LocationHandle<'a>>,
}

impl<'a> PTS<'a> {
    fn new() -> Self {
        PTS { locations: vec!(), variables: VariableMap::new(), initial: None}
    }
}

impl Variable {
    fn new(name: &str) -> Self {
        Variable{ name: name.to_string().into_boxed_str() }
    }
}

impl LinearPolynomial {
    fn new() -> Self {
        LinearPolynomial { coefficients: vec!() }
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

