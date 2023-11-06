use std::{borrow::Borrow, hash::BuildHasher, ops::Deref, rc::Rc};

use super::{set::VariableSet, Variable};

pub type ProgramVariables = VariableSet<ProgramVariable>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ProgramVariable {
    ptr: Rc<str>,
}

impl Variable for ProgramVariable {
    type DATA = str;
    fn new<T: AsRef<Self::DATA> + ?Sized, S: BuildHasher + Default>(
        variables: &mut crate::pts::variable::set::VariableSet<Self, S>,
        data: &T,
    ) -> Self {
        variables.get_or_insert(Self {
            ptr: Rc::from(data.as_ref().to_owned()),
        })
    }
}

impl std::fmt::Display for ProgramVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.ptr, f)
    }
}

impl Deref for ProgramVariable {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl Borrow<Rc<str>> for ProgramVariable {
    fn borrow(&self) -> &Rc<str> {
        &self.ptr
    }
}

impl Borrow<str> for ProgramVariable {
    fn borrow(&self) -> &str {
        &self.ptr
    }
}

impl From<Rc<str>> for ProgramVariable {
    fn from(value: Rc<str>) -> Self {
        Self { ptr: value }
    }
}

impl Into<Rc<str>> for ProgramVariable {
    fn into(self) -> Rc<str> {
        self.ptr
    }
}
