use crate::pts;
use pts::guard::Guards;
use pts::inequality::InequalitySystem;

use std::cell::RefCell;

#[derive(Debug)]
#[repr(align(64))] // 64 bytes
pub struct Location<'a> {
    pub invariant: InequalitySystem,
    pub outgoing: Guards<'a>,
}

pub type LocationHandle<'a> = Option<&'a RefCell<Location<'a>>>;

