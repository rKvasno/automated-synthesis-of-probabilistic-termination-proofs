pub mod guard;
pub mod location;
pub mod transition;
pub mod inequality;
pub mod variable_map;
pub mod linear_polynomial;

use location::{Location, LocationHandle};
use variable_map::VariableMap;

use std::cell::RefCell;

#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct PTS<'a> {
    locations: Vec<RefCell<Location<'a>>>,
    variables: VariableMap,
    initial: Option<LocationHandle<'a>>,
}

impl<'a> PTS<'a> {
    pub fn new_location(&'a mut self) -> LocationHandle<'a> {
        self.locations.push(RefCell::<Location>::default());
        self.locations.last()
    }

    pub fn add_location(&'a mut self, location: Location<'a>) -> LocationHandle<'a> {
        self.locations.push(RefCell::new(location));
        self.locations.last()
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

