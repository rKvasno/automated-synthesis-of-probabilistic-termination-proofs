use std::{borrow::Borrow, ops::Deref, rc::Rc};

use super::{set::VariableSet, Variable};

#[macro_export]
macro_rules! program_var{
    [$variables:expr, $name: expr $(,)?]=> {
        {
            $crate::var![$variables, std::rc::Rc::from($name)]
        }
    }
}

#[macro_export]
macro_rules! program_variables{
    [ $( $x:expr ),+ $(,)?] => {
        {
            $crate::variables![$(std::rc::Rc::from($x), )+]
        }
    };
    [] => {
        {
            $crate::pts::variable::program_variable::ProgramVariables::default()
        }
    }
}

pub type ProgramVariables = VariableSet<ProgramVariable>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ProgramVariable {
    ptr: Rc<str>,
}

impl Variable for ProgramVariable {}

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

#[cfg(test)]
mod tests {
    mod macros {
        use std::rc::Rc;

        use crate::pts::variable::program_variable::{ProgramVariable, ProgramVariables};

        #[test]
        fn program_var() {
            let mut variables: ProgramVariables = Default::default();
            assert!(variables.len() == 0);
            let var = program_var!(&mut variables, "a");
            assert_eq!(var, ProgramVariable { ptr: Rc::from("a") });
            assert!(variables.len() == 1);
            let var = program_var!(&mut variables, "a");
            assert_eq!(var, ProgramVariable { ptr: Rc::from("a") });
            assert!(variables.len() == 1);
            let var = program_var!(&mut variables, "b");
            assert_eq!(var, ProgramVariable { ptr: Rc::from("b") });
            assert!(variables.len() == 2);
        }

        #[test]
        fn program_variables() {
            assert_eq!(program_variables!(), Default::default());
            let mut variables: ProgramVariables = Default::default();
            variables.insert(ProgramVariable { ptr: Rc::from("a") });
            assert!(variables.len() == 1);
            assert_eq!(program_variables!("a"), variables);
            variables.insert(ProgramVariable { ptr: Rc::from("a") });
            assert!(variables.len() == 1);
            assert_eq!(program_variables!("a"), variables);
            variables.insert(ProgramVariable { ptr: Rc::from("b") });
            assert!(variables.len() == 2);
            assert_eq!(program_variables!("a", "b"), variables);
        }
    }
}
