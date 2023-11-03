use std::{borrow::Borrow, ops::Deref, rc::Rc};

use crate::pts::{
    guard::PolyhedraID,
    invariant::TransitionID,
    linear_polynomial::{
        coefficient::{Coefficient, Constant},
        Polynomial,
    },
    location::LocationID,
    variable::{program_variable::ProgramVariable, Variable},
};

use super::Generator;

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
enum TemplateVariableData {
    _Eps,
    _UpperBound,
    _LowerBound,
    _A(LocationID, ProgramVariable),
    _B(LocationID),
    _X(LocationID, PolyhedraID, ProgramVariable, TransitionID),
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
struct TemplateVariable {
    ptr: Rc<TemplateVariableData>,
}

impl Deref for TemplateVariable {
    type Target = TemplateVariableData;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl Borrow<Rc<TemplateVariableData>> for TemplateVariable {
    fn borrow(&self) -> &Rc<TemplateVariableData> {
        &self.ptr
    }
}

impl Variable for TemplateVariable {
    type DATA = TemplateVariableData;
    fn new<T: AsRef<Self::DATA> + ?Sized>(
        variables: &mut crate::pts::variable::set::VariableSet<Self>,
        data: &T,
    ) -> Self {
        variables.get_or_insert(Self {
            ptr: Rc::from(data.as_ref().to_owned()),
        })
    }
}

type _Template = Polynomial<ProgramVariable, Polynomial<TemplateVariable, Constant>>;

impl Coefficient for Polynomial<TemplateVariable, Constant> {
    fn zero() -> Self {
        Default::default()
    }

    fn mul_by_constant(&mut self, n: Constant) {
        self.iter_mut().for_each(|(_, c)| c.mul_by_constant(n))
    }
}

pub struct FarkasBasedGenerator;

impl Generator for FarkasBasedGenerator {
    fn generate_problem<S: super::linear_solvers::Solver>(
        &self,
        _pts: &crate::pts::PTS,
    ) -> super::linear_solvers::Problem {
        todo!()
    }
    fn build_ranking_function<Solution: Iterator<Item = (ProgramVariable, Constant)>>(
        &self,
        _pts: crate::pts::PTS,
        _solution: Solution,
    ) -> Result<super::RankedPTS, super::GeneratorError> {
        todo!()
    }
}
