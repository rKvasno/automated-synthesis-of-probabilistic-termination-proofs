mod guard;
mod location;
mod transition;
mod variables;

use crate::pts::location::{Location, LocationHandle};
use crate::pts::variables::{Variable, VariableMap, VariableError};
use std::cell::RefCell;

#[derive(Debug)]
pub struct LinearPolynomial {
    coefficients: Vec<f64>
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

impl LinearPolynomial {
    pub fn new() -> Self {
        LinearPolynomial { coefficients: vec!() }
    }

    fn len(&self) -> usize {
        self.coefficients.len()
    }

    pub fn add_term(&mut self, 
                map: &VariableMap,
                var: &Variable,
                coefficient: f64) -> Result<(), VariableError>{
        let index = map.variable_to_index(var.as_str());

        // if variable is not in the map, its not a program variable
        if index.is_none() {
            Err(VariableError::new(var))   
        }
        else {
            if self.len() < map.len() {
                self.coefficients.resize(map.len(), 0.0);
            }
            self.coefficients[index.unwrap()] += coefficient;
            Ok(())
        }
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

