use crate::pts::{location, variable_map, linear_polynomial};
use location::LocationHandle;
use variable_map::Variable;
use linear_polynomial::LinearPolynomial;


#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct Assignment(pub Variable, pub LinearPolynomial);

impl Assignment {
    pub fn new(var: Variable, pol: LinearPolynomial) -> Self {
        Assignment( var, pol )
    }
}

#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct Transition<'a> { 
    pub assignments: Vec<Assignment>,
    pub target: LocationHandle<'a>
}

// #[cfg(test)]
// mod tests {
//     use std::mem;
//     use super::{Assignment, Transition};
//     
//     #[test]
//     fn align_transition() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<Transition>(), 32);
//         }
//     }
//     
//     #[test]
//     fn size_transition() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<Transition>(), 32);
//         }
//     }
//     
//     #[test]
//     fn align_assignment() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<Assignment>(), 32);
//         }
//     }
//     #[test]
//     fn size_assignment() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<Assignment>(), 32);
//         }
//     }
// }

