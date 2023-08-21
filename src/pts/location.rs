use crate::pts;
use pts::guard::Guards;
use pts::inequality::InequalitySystem;

#[derive(Debug, Default)]
#[repr(align(64))] // 64 bytes
pub struct Location {
    pub invariant: InequalitySystem,
    pub outgoing: Guards,
}

#[derive(Debug, Default)]
pub struct Locations {
    data: Vec<Location>,
}


impl<'a> Locations {
    pub fn new_location(&mut self) -> LocationHandle {
        self.data.push(Location::default());
        Some(self.data.len()-1)
    }

    pub fn terminating_location(&self) -> LocationHandle {
        None
    }
}



pub type LocationHandle = Option<usize>;

