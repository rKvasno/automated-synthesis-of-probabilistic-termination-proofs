mod guard;
mod location;
mod transition;

use crate::pts::location::{Location, LocationHandle};
use std::{cell::RefCell, borrow::Borrow};

pub struct Variable(Box<str>);
pub struct LinearPolynomial(Vec<f64>);

// linear search in Vec is faster than logarithmic for small number of elements,
// mostly due to caching
pub struct VariableMap(Vec<Variable>);

#[repr(align(32))] // 32 bytes
pub struct PTS<'a> {
    locations: Vec<RefCell<Location<'a>>>,
    variables: VariableMap,
    initial: LocationHandle<'a>,
}

impl VariableMap {
    // Return the variable with specified index in a polynomial
    fn index_to_variable(&self, index: usize) -> Option<&str> {
        if index - 1 < self.0.len() {
            Some(&self.0[index - 1].0)
        }
        else {
            None
        }
    }

    // Find variable and return its index, if not found, add it
    fn find_or_add(&mut self, variable: &str) -> usize {
        let index = self.0.iter().enumerate().find(|x| x.1.0.as_ref() == variable).map(|x| x.0 + 1);
        if index.is_none() {
            self.0.push(Variable(variable.to_string().into_boxed_str()));
            self.0.len()
        }
        else {
            index.unwrap()
        }
    }


    // Find variable and return its index, if not found, return None 
    fn variable_to_index(&self, variable: &str) -> Option<usize> {
        self.0.iter().enumerate().find(|x| x.1.0.as_ref() == variable).map(|x| x.0 + 1)

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

