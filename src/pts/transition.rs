use crate::pts::{location, variable_map, linear_polynomial};
use location::LocationHandle;
use variable_map::Variable;
use linear_polynomial::LinearPolynomial;


#[derive(Debug)]
#[repr(align(32))] // 32 bytes
struct Assignment(Variable, LinearPolynomial);

#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct Transition<'a> { 
    assignments: Vec<Assignment>,
    target: Option<LocationHandle<'a>>
}

impl<'a> Transition<'a> {
    fn new(target: Option<LocationHandle<'a>>) -> Self {
        Transition { assignments: vec!(),  target }
    }
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

