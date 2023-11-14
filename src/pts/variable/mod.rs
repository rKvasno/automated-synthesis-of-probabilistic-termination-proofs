use std::{fmt::Debug, hash::Hash};

pub mod program_variable;
pub mod set;

#[macro_export]
macro_rules! var{
    [$variables:expr, $name: expr $(,)?]=> {
        {
            $crate::pts::variable::set::VariableSet::get_or_insert($variables, $name)
        }
    }
}
pub trait Variable: Debug + Clone + Hash + PartialEq + Eq {}
