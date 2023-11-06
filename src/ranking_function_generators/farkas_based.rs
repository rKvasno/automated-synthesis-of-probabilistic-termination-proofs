use std::{borrow::Borrow, hash::BuildHasher, ops::Deref, rc::Rc};

use crate::{
    pts::{
        guard::{Guards, TransitionID},
        invariant::{Invariant, PolyhedronID},
        linear_polynomial::{
            coefficient::{Coefficient, Constant},
            Polynomial,
        },
        location::{LocationHandle, LocationID},
        relation::{Relation, RelationSign},
        system::{RelationID, StateSystem, System},
        transition::Transition,
        variable::{program_variable::ProgramVariable, set::VariableSet, Variable},
        PTS,
    },
    ranking_function_generators::linear_solvers::Interval,
};

#[macro_export]
macro_rules! predicate {
    [
        $(
            $varset:expr; $(
                $sign:literal, $constant: expr $(
                    , $coeff:expr, $var:expr
                )*
            );* $(;)?
        )?] => {
        {
            let temp: $crate::ranking_function_generators::farkas_based::_Predicate = $crate::system!(
                $(
                    $(
                        $crate::relation!(
                            $sign,
                            $constant,
$varset
                            $(
                                ,
                                $coeff,
                                $var
                            )*



                        ),
                    )*
                )?

            );
            temp
        }
    };
}

use super::{linear_solvers::DomainMap, Generator};
pub type TemplateDomains = DomainMap<TemplateVariable>;

pub type TemplateVariables = VariableSet<TemplateVariable>;

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub enum TemplateVariableData {
    _Eps,
    _UpperBound,
    _LowerBound,
    _RankingCoefficient(LocationID, ProgramVariable),
    _RankingConstant(LocationID),
    _FarkasVariable(LocationID, PolyhedronID, RelationID, Option<TransitionID>),
}

impl AsRef<TemplateVariableData> for TemplateVariableData {
    // The docs for AsRef state that AsRef should be reflexive for any T,
    // however that blanket implementation is not possible due to technical
    // restrictions.
    fn as_ref(&self) -> &TemplateVariableData {
        &self
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
pub struct TemplateVariable {
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
    fn new<T: AsRef<Self::DATA> + ?Sized, S: BuildHasher + Default>(
        variables: &mut crate::pts::variable::set::VariableSet<Self, S>,
        data: &T,
    ) -> Self {
        variables.get_or_insert(Self {
            ptr: Rc::from(data.as_ref().to_owned()),
        })
    }
}

type _Template = Polynomial<ProgramVariable, Polynomial<TemplateVariable, Constant>>;

type _Predicate = System<TemplateVariable, Constant>;

fn _farkas_assertion<S: BuildHasher + Default>(
    program_variables: &VariableSet<ProgramVariable, S>,
    template_variables: &mut TemplateVariables,
    location_id: LocationID,
    polyhedron_id: PolyhedronID,
    transition_id: Option<TransitionID>,
    polyhedron: &StateSystem,
    template: Relation<ProgramVariable, Polynomial<TemplateVariable, Constant>>,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) {
    assert!(template.is_nonstrict_inequality());
    // this splitting is not necessary, but this way its slightly easier to comprehend
    let (c, d) = template.split_constant();

    let mut acc: _Template = Default::default();
    for (halfspace_id, halfspace) in polyhedron.iter_with_ids() {
        // can be strict or non-strict due to conditional branching
        assert!(halfspace.is_nonstrict_inequality() || halfspace.is_strict_inequality());
        let (a, b) = halfspace.clone().split_constant();
        let y = TemplateVariable::new(
            template_variables,
            &TemplateVariableData::_FarkasVariable(
                location_id,
                polyhedron_id,
                halfspace_id,
                transition_id,
            ),
        );
        domains.insert(y.clone(), Interval(0.0, f64::INFINITY));

        for var in program_variables {
            //a
            let a_coefficient = a
                .get_coefficient(&Some(var.clone()))
                .cloned()
                .unwrap_or(Constant::zero());
            let mut tmp: Polynomial<TemplateVariable, Constant> = Default::default();
            tmp.add_term(a_coefficient, y.clone());
            acc.add_term(tmp, Some(var.clone()));
        }
        //b
        let b_coefficient = b
            .get_coefficient(&None)
            .cloned()
            .unwrap_or(Constant::zero());
        let mut tmp: Polynomial<TemplateVariable, Constant> = Default::default();
        tmp.add_term(b_coefficient, y.clone());
        acc.add_term(tmp, None);
    }
    for var in program_variables {
        let lhs = acc
            .get_coefficient(&Some(var.clone()))
            .cloned()
            .unwrap_or(Coefficient::zero());
        let rhs = c
            .get_coefficient(&Some(var.clone()))
            .cloned()
            .unwrap_or(Coefficient::zero());
        restrictions.push(Relation::new(lhs, RelationSign::EQ, rhs));
    }
    let lhs = acc
        .get_coefficient(&None)
        .cloned()
        .unwrap_or(Coefficient::zero());
    let rhs = d
        .get_coefficient(&None)
        .cloned()
        .unwrap_or(Coefficient::zero());
    restrictions.push(Relation::new(lhs, RelationSign::LE, rhs));
}

fn _terminating_negativity<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) {
    let invariant = pts.locations.get_invariant(None).unwrap();
    assert!(!invariant.is_empty());
    // location is valid => unwrap()
    let location_id = pts.locations.get_id(None).unwrap();

    // location is valid => unwrap()
    for (polyhedron_id, polyhedron) in invariant.iter_with_ids() {
        //upper
        let template = Relation::new(
            _generate_template(template_variables, &pts.variables, location_id),
            RelationSign::LE,
            _generate_constant_template(template_variables, &TemplateVariableData::_UpperBound),
        );
        _farkas_assertion(
            &pts.variables,
            template_variables,
            location_id,
            polyhedron_id,
            None,
            polyhedron,
            template,
            domains,
            restrictions,
        );

        // lower
        let template = Relation::new(
            _generate_template(template_variables, &pts.variables, location_id),
            RelationSign::GE,
            _generate_constant_template(template_variables, &TemplateVariableData::_LowerBound),
        );
        _farkas_assertion(
            &pts.variables,
            template_variables,
            location_id,
            polyhedron_id,
            None,
            polyhedron,
            template,
            domains,
            restrictions,
        );
    }
}

fn _non_terminating_non_negativity<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) {
    let invariant = pts.locations.get_invariant(location).unwrap();
    assert!(!invariant.is_empty());

    assert!(pts.locations.is_nonterminating_location(location));
    // location is valid => unwrap()
    let location_id = pts.locations.get_id(location).unwrap();

    // location is valid => unwrap()
    for (polyhedron_id, polyhedron) in invariant.iter_with_ids() {
        let template = Relation::new(
            _generate_template(template_variables, &pts.variables, location_id),
            RelationSign::GE,
            Default::default(),
        );
        _farkas_assertion(
            &pts.variables,
            template_variables,
            location_id,
            polyhedron_id,
            None,
            polyhedron,
            template,
            domains,
            restrictions,
        );
    }
}

fn _martingale_difference<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) {
    assert!(!pts.locations.is_terminating_location(location));
    let guard = pts.locations.get_outgoing(location).unwrap();
    assert!(!guard.is_empty());
    match guard {
        Guards::Logic(transitions) => _logic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
            domains,
            restrictions,
        ),
        Guards::Unguarded(boxed_transition) => _nondeterministic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            std::iter::once(&(**boxed_transition)).enumerate(),
            domains,
            restrictions,
        ),
        Guards::Probabilistic(transitions) => _probabilistic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
            domains,
            restrictions,
        ),
        Guards::Nondeterministic(transitions) => _nondeterministic_martingale_difference(
            pts,
            template_variables,
            // handle is valid => unwrap
            pts.locations.get_id(location).unwrap(),
            // handle is valid => unwrap
            pts.locations.get_invariant(location).unwrap(),
            transitions.iter().enumerate(),
            domains,
            restrictions,
        ),
    }
}

fn _probabilistic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) where
    I: Iterator<Item = (TransitionID, &'a (Constant, Transition))> + Clone,
{
    assert!(!invariant.is_empty());
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
            _generate_template_expression(template_variables, &TemplateVariableData::_Eps),
            None,
        );

        _farkas_assertion(
            &pts.variables,
            template_variables,
            location_id,
            polyhedron_id,
            None,
            polyhedron,
            Relation::new(lhs, RelationSign::LE, rhs),
            domains,
            restrictions,
        )
    }
}

fn _logic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) where
    I: Iterator<Item = (TransitionID, &'a (StateSystem, Transition))> + Clone,
{
    assert!(!invariant.is_empty());
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
                _generate_template_expression(template_variables, &TemplateVariableData::_Eps),
                None,
            );
            let mut conditioned_polyhedron = polyhedron.to_owned();

            conditioned_polyhedron.append(&mut system.to_owned());

            _farkas_assertion(
                &pts.variables,
                template_variables,
                location_id,
                polyhedron_id,
                Some(transition_id),
                &conditioned_polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                domains,
                restrictions,
            )
        }
    }
}

fn _nondeterministic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location_id: LocationID,
    invariant: &Invariant,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut _Predicate,
) where
    I: Iterator<Item = (TransitionID, &'a Transition)> + Clone,
{
    assert!(!invariant.is_empty());
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
                _generate_template_expression(template_variables, &TemplateVariableData::_Eps),
                None,
            );

            _farkas_assertion(
                &pts.variables,
                template_variables,
                location_id,
                polyhedron_id,
                Some(transition_id),
                polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                domains,
                restrictions,
            )
        }
    }
}

fn _generate_template<S: BuildHasher + Default>(
    template_variables: &mut TemplateVariables,
    program_variables: &VariableSet<ProgramVariable, S>,
    location_id: LocationID,
) -> _Template {
    let mut template = _Template::default();
    for var in program_variables.into_iter() {
        template.add_term(
            _generate_template_expression(
                template_variables,
                &TemplateVariableData::_RankingCoefficient(location_id, var.to_owned()),
            ),
            var.to_owned(),
        );
    }

    template.add_term(
        _generate_template_expression(
            template_variables,
            &TemplateVariableData::_RankingConstant(location_id),
        ),
        None,
    );
    template
}

fn _generate_constant_template(
    template_variables: &mut TemplateVariables,
    data: &TemplateVariableData,
) -> _Template {
    let mut template = _Template::default();
    template.add_term(
        _generate_template_expression(template_variables, data),
        None,
    );

    template
}

fn _generate_template_expression(
    template_variables: &mut TemplateVariables,
    data: &TemplateVariableData,
) -> Polynomial<TemplateVariable, Constant> {
    let template_variable = TemplateVariable::new(template_variables, data);
    let mut template_expression = Polynomial::<TemplateVariable, Constant>::default();
    template_expression.add_term(1.0, template_variable);
    template_expression
}

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
    type VAR = TemplateVariable;
    fn generate_problem<S: super::linear_solvers::Solver>(
        &self,
        _pts: &crate::pts::PTS,
    ) -> super::linear_solvers::Problem<TemplateVariable> {
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

#[cfg(test)]
mod tests {
    use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};

    use crate::{
        domains, invariant,
        pts::{
            variable::{program_variable::ProgramVariable, Variable},
            PTS,
        },
        ranking_function_generators::farkas_based::{
            TemplateVariableData, _non_terminating_non_negativity,
        },
        variables,
    };

    use super::_terminating_negativity;

    #[test]
    fn terminating_negativity() {
        let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
        pts.locations
            .set_invariant(
                None,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
            )
            .unwrap();
        let mut template_variables = Default::default();
        let mut domains = Default::default();
        let mut restrictions = Default::default();
        _terminating_negativity(
            &pts,
            &mut template_variables,
            &mut domains,
            &mut restrictions,
        );
        assert_eq!(
            template_variables,
            variables!(
                &TemplateVariableData::_RankingCoefficient(
                    0,
                    ProgramVariable::new(&mut pts.variables, "a")
                ),
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                &TemplateVariableData::_RankingConstant(0),
                &TemplateVariableData::_UpperBound,
                &TemplateVariableData::_LowerBound,
            )
        );

        assert_eq!(
            domains,
            domains!(
                &mut template_variables,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                0.0,
                f64::INFINITY
            )
        );
        assert_eq!(
            restrictions,
            predicate!(
                &mut template_variables;
                "==",
                0.0,
                -1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                -2.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None);

                "<=",
                0.0,
                -5.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                1.0,
                &TemplateVariableData::_RankingConstant(0),
                -1.0,
                &TemplateVariableData::_UpperBound;

                "==",
                0.0,
                1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                -2.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None);

                "<=",
                0.0,
                -5.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                -1.0,
                &TemplateVariableData::_RankingConstant(0),
                1.0,
                &TemplateVariableData::_LowerBound;


                "==",
                0.0,
                -1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a"));

                "<=",
                0.0,
                1.0,
                &TemplateVariableData::_RankingConstant(0),
                -1.0,
                &TemplateVariableData::_UpperBound;

                "==",
                0.0,
                1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a"));

                "<=",
                0.0,
                -1.0,
                &TemplateVariableData::_RankingConstant(0),
                1.0,
                &TemplateVariableData::_LowerBound;
            )
        );
    }

    #[test]
    fn nonterminating_nonnegativity() {
        let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
        let location = pts.locations.new_location();
        pts.locations
            .set_invariant(
                location,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
            )
            .unwrap();
        let mut template_variables = Default::default();
        let mut domains = Default::default();
        let mut restrictions = Default::default();
        _non_terminating_non_negativity(
            &pts,
            &mut template_variables,
            location,
            &mut domains,
            &mut restrictions,
        );
        assert_eq!(
            template_variables,
            variables!(
                &TemplateVariableData::_RankingCoefficient(
                    0,
                    ProgramVariable::new(&mut pts.variables, "a")
                ),
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                &TemplateVariableData::_RankingConstant(0),
            )
        );

        assert_eq!(
            domains,
            domains!(
                &mut template_variables,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                0.0,
                f64::INFINITY
            )
        );
        assert_eq!(
            restrictions,
            predicate!(
                &mut template_variables;
                "==",
                0.0,
                1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                -2.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None);

                "<=",
                0.0,
                -5.0,
                &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                -1.0,
                &TemplateVariableData::_RankingConstant(0);


                "==",
                0.0,
                1.0,
                &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a"));

                "<=",
                0.0,
                -1.0,
                &TemplateVariableData::_RankingConstant(0);

            )
        );
    }

    mod martingale_difference {
        use crate::{
            domains, guards, invariant,
            pts::{
                variable::{program_variable::ProgramVariable, Variable},
                PTS,
            },
            ranking_function_generators::farkas_based::{
                TemplateVariableData, _martingale_difference,
            },
            state_system, transition, variables,
        };
        use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};

        #[test]
        fn probabilistic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations
                .set_invariant(
                    start_location,
                    invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
                )
                .unwrap();
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(P:
                            0.25,
                            transition!(intermediate_location, &mut pts.variables; "a", 3.0, 7.0, "a"),
                            0.75,
                            transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(
                template_variables,
                variables!(
                    &TemplateVariableData::_RankingCoefficient(
                        0,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        1,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        2,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                    &TemplateVariableData::_RankingConstant(0),
                    &TemplateVariableData::_RankingConstant(1),
                    &TemplateVariableData::_RankingConstant(2),
                    &TemplateVariableData::_Eps,
                )
            );

            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                    0.0,
                    f64::INFINITY
                )
            );
            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.75,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -0.75,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, None);

                    "<=",
                    0.0,
                    0.75,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, None),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    0.25,
                    &TemplateVariableData::_RankingConstant(1),
                    0.75,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.75,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -0.75,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a"));

                    "<=",
                    0.0,
                    0.75,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    0.25,
                    &TemplateVariableData::_RankingConstant(1),
                    0.75,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps


                )
            );
        }

        #[test]
        fn logic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations
                .set_invariant(
                    start_location,
                    invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
                )
                .unwrap();
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(L:
                            state_system!(&mut pts.variables; "<=", 11.0, -1.0, "a"),
                            transition!(intermediate_location, &mut pts.variables; "a", 3.0, 7.0, "a"),
                            state_system!(&mut pts.variables; ">", 11.0, -1.0, "a"),
                            transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(
                template_variables,
                variables!(
                    &TemplateVariableData::_RankingCoefficient(
                        0,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        1,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        2,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(0)),
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(0)),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(1)),
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(1)),
                    &TemplateVariableData::_RankingConstant(0),
                    &TemplateVariableData::_RankingConstant(1),
                    &TemplateVariableData::_RankingConstant(2),
                    &TemplateVariableData::_Eps,
                )
            );

            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(0)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(0)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(1)),
                    0.0,
                    f64::INFINITY,
                )
            );
            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    -1.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(0));

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    -11.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(0)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    1.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(1));

                    "<=",
                    0.0,
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    11.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 1, Some(1)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(0));

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -11.0,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(0)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a")),
                    1.0,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(1));

                    "<=",
                    0.0,
                    11.0,
                    &TemplateVariableData::_FarkasVariable(0, 1, 0, Some(1)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps;
                )
            );
        }

        #[test]
        fn nondeterministic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations
                .set_invariant(
                    start_location,
                    invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
                )
                .unwrap();
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(
                        transition!(intermediate_location, &mut pts.variables; "a", 3.0, 7.0, "a"),
                        transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(
                template_variables,
                variables!(
                    &TemplateVariableData::_RankingCoefficient(
                        0,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        1,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        2,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    &TemplateVariableData::_RankingConstant(0),
                    &TemplateVariableData::_RankingConstant(1),
                    &TemplateVariableData::_RankingConstant(2),
                    &TemplateVariableData::_Eps,
                )
            );

            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    0.0,
                    f64::INFINITY
                )
            );
            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0));

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(0)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1));

                    "<=",
                    0.0,
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, Some(1)),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps;



                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a"));

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(2, ProgramVariable::new(&mut pts.variables, "a"));

                    "<=",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(2),
                    1.0,
                    &TemplateVariableData::_Eps;
                )
            );
        }

        #[test]
        fn single() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations
                .set_invariant(
                    start_location,
                    invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
                )
                .unwrap();
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(
                        transition!(intermediate_location, &mut pts.variables; "a", 3.0, 7.0, "a"),
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(
                template_variables,
                variables!(
                    &TemplateVariableData::_RankingCoefficient(
                        0,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_RankingCoefficient(
                        1,
                        ProgramVariable::new(&mut pts.variables, "a")
                    ),
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, start_location),
                    &TemplateVariableData::_RankingConstant(0),
                    &TemplateVariableData::_RankingConstant(1),
                    &TemplateVariableData::_Eps,
                )
            );

            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, start_location),
                    0.0,
                    f64::INFINITY
                )
            );
            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -2.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, start_location);

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -5.0,
                    &TemplateVariableData::_FarkasVariable(0, 0, 0, start_location),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps;

                    "==",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(0, ProgramVariable::new(&mut pts.variables, "a")),
                    -7.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a"));

                    "<=",
                    0.0,
                    3.0,
                    &TemplateVariableData::_RankingCoefficient(1, ProgramVariable::new(&mut pts.variables, "a")),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(0),
                    1.0,
                    &TemplateVariableData::_RankingConstant(1),
                    1.0,
                    &TemplateVariableData::_Eps
                )
            );
        }
    }

    mod farkas_assertion {
        use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};

        use crate::{
            domains,
            pts::{
                relation::{Relation, RelationSign},
                variable::{
                    program_variable::{ProgramVariable, ProgramVariables},
                    set::VariableSet,
                },
            },
            ranking_function_generators::farkas_based::{
                TemplateVariableData, _farkas_assertion, _generate_constant_template,
                _generate_template,
            },
            state_system, variables,
        };

        #[test]
        fn empty() {
            {
                // no restrictions
                let program_variables: ProgramVariables = variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let lhs = Default::default();
                let rhs = Default::default();
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                _farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    5,
                    3,
                    Some(1),
                    &state_system!(),
                    Relation::new(lhs, RelationSign::LE, rhs),
                    &mut domains,
                    &mut restrictions,
                );
                assert_eq!(domains, Default::default());

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0
                    )
                );
            }

            {
                // All restrictions true
                let program_variables: ProgramVariables = variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let lhs = Default::default();
                let rhs = Default::default();
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                _farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    5,
                    3,
                    Some(1),
                    &state_system!(
                        &mut program_variables;
                        "<=", 0.0;
                        ">=", 0.0;
                        "<=", 0.0),
                    Relation::new(lhs, RelationSign::LE, rhs),
                    &mut domains,
                    &mut restrictions,
                );
                assert_eq!(domains.len(), 3);
                assert_eq!(
                    domains,
                    domains!(
                        &mut template_variables,
                        &TemplateVariableData::_FarkasVariable(5, 3, 0, Some(1)),
                        0.0,
                        f64::INFINITY,
                        &TemplateVariableData::_FarkasVariable(5, 3, 1, Some(1)),
                        0.0,
                        f64::INFINITY,
                        &TemplateVariableData::_FarkasVariable(5, 3, 2, Some(1)),
                        0.0,
                        f64::INFINITY
                    )
                );

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0
                    )
                );
            }
        }

        #[test]
        fn constant() {
            {
                // Eps on LHS
                let program_variables: ProgramVariables = variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let eps_pol = _generate_constant_template(
                    &mut template_variables,
                    &TemplateVariableData::_Eps,
                );
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                _farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    5,
                    3,
                    Some(1),
                    &state_system!(),
                    Relation::new(eps_pol.clone(), RelationSign::LE, Default::default()),
                    &mut domains,
                    &mut restrictions,
                );
                assert_eq!(domains, Default::default());

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0, 1.0, &TemplateVariableData::_Eps
                    )
                );
            }
            {
                // Eps on RHS
                let program_variables: ProgramVariables = variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let eps_pol = _generate_constant_template(
                    &mut template_variables,
                    &TemplateVariableData::_Eps,
                );
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                _farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    5,
                    3,
                    Some(1),
                    &state_system!(),
                    Relation::new(Default::default(), RelationSign::LE, eps_pol),
                    &mut domains,
                    &mut restrictions,
                );
                assert_eq!(domains, Default::default());

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0, -1.0, &TemplateVariableData::_Eps
                    )
                );
            }
        }

        #[test]
        fn template() {
            let program_variables: VariableSet<ProgramVariable, BuildHasherDefault<DefaultHasher>> =
                variables!("a", "b", "c");
            let mut template_variables = Default::default();
            let lhs = _generate_template(&mut template_variables, &program_variables, 5);
            let rhs = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _farkas_assertion(
                &program_variables,
                &mut template_variables,
                5,
                3,
                Some(1),
                &state_system!(),
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(domains, Default::default());

            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("c").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("b").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("a").unwrap().clone(),
                    );

                    "<=",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingConstant(
                        5,
                    );
                )
            );
        }

        #[test]
        fn two_templates() {
            let program_variables: VariableSet<ProgramVariable, BuildHasherDefault<DefaultHasher>> =
                variables!("a", "b", "c");
            let mut template_variables = Default::default();
            let lhs = _generate_template(&mut template_variables, &program_variables, 5);
            let rhs = _generate_template(&mut template_variables, &program_variables, 6);
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _farkas_assertion(
                &program_variables,
                &mut template_variables,
                5,
                3,
                Some(1),
                &state_system!(),
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(domains, Default::default());

            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("c").unwrap().clone(),
                    ),
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        6,
                        program_variables.get("c").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("b").unwrap().clone(),
                    ),
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        6,
                        program_variables.get("b").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("a").unwrap().clone(),
                    ),
                    1.0,
                    &TemplateVariableData::_RankingCoefficient(
                        6,
                        program_variables.get("a").unwrap().clone(),
                    );

                    "<=",
                    0.0,
                    1.0,
                    &TemplateVariableData::_RankingConstant(
                        5,
                    ),
                    -1.0,
                    &TemplateVariableData::_RankingConstant(
                        6,
                    );
                )
            );
        }

        #[test]
        fn empty_restricted() {
            let (program_variables, polyhedron) = {
                let program_variables_original: VariableSet<
                    ProgramVariable,
                    BuildHasherDefault<DefaultHasher>,
                > = variables!("a", "b", "c");
                let mut program_variables = program_variables_original.clone();
                let polyhedron = state_system!(&mut program_variables;
                    "<=",
                    0.0,
                    1.0,
                    "a",
                    2.0,
                    "b";
                    "<",
                    -1.0,
                    5.0,
                    "c";
                    "<=",
                    10.0,
                    0.5,
                    "a");
                assert_eq!(program_variables, program_variables_original);
                (program_variables, polyhedron)
            };

            let mut template_variables = Default::default();
            let lhs = Default::default();
            let rhs = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _farkas_assertion(
                &program_variables,
                &mut template_variables,
                5,
                3,
                Some(1),
                &polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(domains.len(), 3);
            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(5, 3, 0, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(5, 3, 1, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(5, 3, 2, Some(1)),
                    0.0,
                    f64::INFINITY
                )
            );

            assert_eq!(
                restrictions,
                predicate!(
                &mut template_variables;
                "==",
                0.0,
                2.0,
                &TemplateVariableData::_FarkasVariable(5, 3, 0, Some(1));
                "==",
                0.0,
                1.0,
                &TemplateVariableData::_FarkasVariable(5, 3, 0, Some(1)),
                0.5,
                &TemplateVariableData::_FarkasVariable(5, 3, 2, Some(1));
                "==",
                0.0,
                5.0,
                &TemplateVariableData::_FarkasVariable(5, 3, 1, Some(1));
                "<=",
                0.0,
                1.0,
                &TemplateVariableData::_FarkasVariable(5, 3, 1, Some(1)),
                -10.0,
                &TemplateVariableData::_FarkasVariable(5, 3, 2, Some(1));
                )
            );
        }

        #[test]
        fn template_restricted() {
            let (program_variables, polyhedron) = {
                let program_variables_original: VariableSet<
                    ProgramVariable,
                    BuildHasherDefault<DefaultHasher>,
                > = variables!("a", "b", "c");
                let mut program_variables = program_variables_original.clone();
                let polyhedron = state_system!(&mut program_variables;
                    "<=",
                    0.0,
                    1.0,
                    "a",
                    2.0,
                    "b";
                    "<",
                    -1.0,
                    5.0,
                    "c";
                    "<=",
                    10.0,
                    0.5,
                    "a");
                assert_eq!(program_variables, program_variables_original);
                (program_variables, polyhedron)
            };
            let mut template_variables = Default::default();
            let lhs = {
                let mut tmp = _generate_template(&mut template_variables, &program_variables, 5);
                tmp.mul_by_constant(0.1.into());
                tmp
            };
            let rhs = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            _farkas_assertion(
                &program_variables,
                &mut template_variables,
                5,
                3,
                Some(1),
                &polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
            );
            assert_eq!(domains.len(), 3);
            assert_eq!(
                domains,
                domains!(
                    &mut template_variables,
                    &TemplateVariableData::_FarkasVariable(5, 3, 0, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(5, 3, 1, Some(1)),
                    0.0,
                    f64::INFINITY,
                    &TemplateVariableData::_FarkasVariable(5, 3, 2, Some(1)),
                    0.0,
                    f64::INFINITY
                )
            );

            assert_eq!(
                restrictions,
                predicate!(
                    &mut template_variables;
                    "==",
                    0.0,
                    -0.1,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("b").unwrap().clone(),
                    ),
                    2.0,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        0,
                        Some(1)
                    );

                    "==",
                    0.0,
                    -0.1,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("a").unwrap().clone(),
                    ),
                    1.0,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        0,
                        Some(1)
                    ),
                    0.5,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        2,
                        Some(1)
                    );

                    "==",
                    0.0,
                    -0.1,
                    &TemplateVariableData::_RankingCoefficient(
                        5,
                        program_variables.get("c").unwrap().clone(),
                    ),
                    5.0,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        1,
                        Some(1)
                    );

                    "<=",
                    0.0,
                    1.0,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        1,
                        Some(1)
                    ),
                    -10.0,
                    &TemplateVariableData::_FarkasVariable(
                        5,
                        3,
                        2,
                        Some(1)
                    ),
                    0.1,
                    &TemplateVariableData::_RankingConstant(5)
                )
            );
        }
    }
}
