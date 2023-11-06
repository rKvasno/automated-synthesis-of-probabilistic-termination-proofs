use std::{
    borrow::Borrow,
    fmt::Debug,
    hash::{BuildHasher, Hash},
    ops::Deref,
    rc::Rc,
};

pub mod program_variable;
pub mod set;

pub trait Variable:
    Clone + Hash + PartialEq + Eq + Borrow<Rc<Self::DATA>> + Deref<Target = Self::DATA>
{
    type DATA: ToOwned + Debug + PartialEq + Eq + Hash + ?Sized;
    fn new<T: AsRef<Self::DATA> + ?Sized, S: BuildHasher + Default>(
        variables: &mut crate::pts::variable::set::VariableSet<Self, S>,
        data: &T,
    ) -> Self;
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        pts::variable::{
            program_variable::{ProgramVariable, ProgramVariables},
            Variable,
        },
        variables,
    };

    #[test]
    fn new() {
        let mut variables: ProgramVariables = variables!("test");
        let var = ProgramVariable::new(&mut variables, "testing");
        assert_eq!(variables, variables!("test", "testing"));
        assert_eq!(var, Rc::<str>::from("testing").into());
        let var = ProgramVariable::new(&mut variables, "test");
        assert_eq!(variables, variables!("test", "testing"));
        assert_eq!(var, Rc::<str>::from("test").into());
    }
}
