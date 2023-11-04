use std::{borrow::Borrow, ops::Deref, rc::Rc};

use crate::pts::{
    guard::{Guards, TransitionID},
    invariant::{Invariant, PolyhedraID},
    linear_polynomial::{
        coefficient::{Coefficient, Constant},
        Polynomial,
    },
    location::{LocationHandle, LocationID},
    relation::{Relation, RelationSign},
    system::{StateSystem, System},
    transition::Transition,
    variable::{
        program_variable::{ProgramVariable, ProgramVariables},
        set::VariableSet,
        Variable,
    },
    PTS,
};

use super::Generator;

type _TemplateVariables = VariableSet<_TemplateVariable>;

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
enum _TemplateVariableData {
    _Eps,
    _UpperBound,
    _LowerBound,
    // TODO rename
    _A(LocationID, ProgramVariable),
    _B(LocationID),
    _X1(LocationID, PolyhedraID, ProgramVariable),
    _X2(LocationID, PolyhedraID, TransitionID, ProgramVariable),
}

impl AsRef<_TemplateVariableData> for _TemplateVariableData {
    // The docs for AsRef state that AsRef should be reflexive for any T,
    // however that blanket implementation is not possible due to technical
    // restrictions.
    fn as_ref(&self) -> &_TemplateVariableData {
        &self
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
struct _TemplateVariable {
    ptr: Rc<_TemplateVariableData>,
}

impl Deref for _TemplateVariable {
    type Target = _TemplateVariableData;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl Borrow<Rc<_TemplateVariableData>> for _TemplateVariable {
    fn borrow(&self) -> &Rc<_TemplateVariableData> {
        &self.ptr
    }
}

impl Variable for _TemplateVariable {
    type DATA = _TemplateVariableData;
    fn new<T: AsRef<Self::DATA> + ?Sized>(
        variables: &mut crate::pts::variable::set::VariableSet<Self>,
        data: &T,
    ) -> Self {
        variables.get_or_insert(Self {
            ptr: Rc::from(data.as_ref().to_owned()),
        })
    }
}

type _Template = Polynomial<ProgramVariable, Polynomial<_TemplateVariable, Constant>>;

type _Predicate = System<_TemplateVariable, Constant>;

fn _farkas_assertion(
    _program_variables: &ProgramVariables,
    _location_id: LocationID,
    _polyhedra_id: PolyhedraID,
    _transition_id: Option<TransitionID>,
    _polyhedra: &StateSystem,
    _template: Relation<ProgramVariable, Polynomial<_TemplateVariable, Constant>>,
) -> Relation<_TemplateVariable, Constant> {
    todo!()
}

fn _martingale_difference(
    pts: &PTS,
    template_variables: &mut _TemplateVariables,
    location: LocationHandle,
) -> _Predicate {
    assert!(!pts.locations.is_terminating_location(location));
    match pts.locations.get_outgoing(location).unwrap() {
        Guards::Logic(transitions) => _logic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
        ),
        Guards::Unguarded(boxed_transition) => _nondeterministic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            std::iter::once(&(**boxed_transition)).enumerate(),
        ),
        Guards::Probabilistic(transitions) => _probabilistic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
        ),
        Guards::Nondeterministic(transitions) => _nondeterministic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
        ),
    }
}

fn _probabilistic_martingale_difference<'a, I>(
    pts: &PTS,
    template_variables: &mut _TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
) -> _Predicate
where
    I: Iterator<Item = (TransitionID, &'a (Constant, Transition))> + Clone,
{
    let mut result = _Predicate::default();
    for (polyhedron_id, polyhedron) in invariant.iter_with_ids() {
        let mut lhs: _Template = Default::default();
        for (_, (probability, transition)) in branches.clone() {
            let mut template = _generate_template(
                template_variables,
                &pts.variables,
                // handl is valid => unwrap
                pts.locations.get_id(transition.target).unwrap(),
            );

            for assignment in transition.assignments.iter() {
                template = assignment.apply(template);
            }

            template.mul_by_constant(probability.to_owned());
            lhs += template;
        }

        let rhs = _generate_template(template_variables, &pts.variables, location_id);
        lhs.add_term(
            _generate_template_expression(template_variables, &_TemplateVariableData::_Eps),
            None,
        );

        result.push(_farkas_assertion(
            &pts.variables,
            location_id,
            polyhedron_id,
            None,
            polyhedron,
            Relation::new(lhs, RelationSign::LE, rhs),
        ))
    }
    result
}

fn _logic_martingale_difference<'a, I>(
    pts: &PTS,
    template_variables: &mut _TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
) -> _Predicate
where
    I: Iterator<Item = (TransitionID, &'a (StateSystem, Transition))> + Clone,
{
    let mut result = _Predicate::default();
    for (polyhedron_id, polyhedron) in invariant.iter_with_ids() {
        for (transition_id, (system, transition)) in branches.clone() {
            let mut lhs: _Template = Default::default();
            let mut template = _generate_template(
                template_variables,
                &pts.variables,
                // handl is valid => unwrap
                pts.locations.get_id(transition.target).unwrap(),
            );

            for assignment in transition.assignments.iter() {
                template = assignment.apply(template);
            }

            lhs += template;

            let rhs = _generate_template(template_variables, &pts.variables, location_id);
            lhs.add_term(
                _generate_template_expression(template_variables, &_TemplateVariableData::_Eps),
                None,
            );
            let mut conditioned_polyhedra = polyhedron.to_owned();

            conditioned_polyhedra.append(&mut system.to_owned());

            result.push(_farkas_assertion(
                &pts.variables,
                location_id,
                polyhedron_id,
                Some(transition_id),
                &conditioned_polyhedra,
                Relation::new(lhs, RelationSign::LE, rhs),
            ))
        }
    }
    result
}

fn _nondeterministic_martingale_difference<'a, I>(
    pts: &PTS,
    template_variables: &mut _TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
) -> _Predicate
where
    I: Iterator<Item = (TransitionID, &'a Transition)> + Clone,
{
    let mut result = _Predicate::default();
    for (polyhedron_id, polyhedron) in invariant.iter_with_ids() {
        for (transition_id, transition) in branches.clone() {
            let mut lhs: _Template = Default::default();
            let mut template = _generate_template(
                template_variables,
                &pts.variables,
                // handle is valid => unwrap
                pts.locations.get_id(transition.target).unwrap(),
            );

            for assignment in transition.assignments.iter() {
                template = assignment.apply(template);
            }

            lhs += template;

            let rhs = _generate_template(template_variables, &pts.variables, location_id);
            lhs.add_term(
                _generate_template_expression(template_variables, &_TemplateVariableData::_Eps),
                None,
            );

            result.push(_farkas_assertion(
                &pts.variables,
                location_id,
                polyhedron_id,
                Some(transition_id),
                polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
            ))
        }
    }
    result
}

fn _generate_template(
    template_variables: &mut _TemplateVariables,
    program_variables: &ProgramVariables,
    location_id: LocationID,
) -> _Template {
    let mut template = _Template::default();
    for var in program_variables.into_iter() {
        template.add_term(
            _generate_template_expression(
                template_variables,
                &_TemplateVariableData::_A(location_id, var.to_owned()),
            ),
            var.to_owned(),
        );
    }

    template.add_term(
        _generate_template_expression(template_variables, &_TemplateVariableData::_B(location_id)),
        None,
    );
    template
}

fn _generate_template_expression(
    template_variables: &mut _TemplateVariables,
    data: &_TemplateVariableData,
) -> Polynomial<_TemplateVariable, Constant> {
    let template_variable = _TemplateVariable::new(template_variables, data);
    let mut template_expression = Polynomial::<_TemplateVariable, Constant>::default();
    template_expression.add_term(1.0, template_variable);
    template_expression
}

impl Coefficient for Polynomial<_TemplateVariable, Constant> {
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
