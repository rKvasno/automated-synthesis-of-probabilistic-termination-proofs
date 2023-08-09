use crate::pts::{guard, linear_polynomial};
use guard::Guards;
use linear_polynomial::LinearPolynomial;

use std::cell::RefCell;

#[derive(Debug)]
#[repr(align(64))] // 64 bytes
pub struct Location<'a> {
    pub invariant: Vec<LinearPolynomial>,
    pub outgoing: Guards<'a>,
}

pub type LocationHandle<'a> = &'a RefCell<Location<'a>>;

#[cfg(test)]
mod tests {
    use std::mem;
    use super::{Location, LocationHandle};
    
    #[test]
    fn align_location_handler() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<LocationHandle>(), 8);
        }
    }
    
    #[test]
    fn size_location_handler() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<LocationHandle>(), 8);
        }
    }
    
    #[test]
    fn align_location() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<Location>(), 64);
        }
    }
    
    #[test]
    fn size_location() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Location>(), 64);
        }
    }
}

