mod guard;
mod location;
pub mod transition;
pub mod variable_map;
pub mod linear_polynomial;

use location::{Location, LocationHandle};
use variable_map::VariableMap;

use std::cell::RefCell;

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

