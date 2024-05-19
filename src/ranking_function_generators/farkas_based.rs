use std::{borrow::Borrow, hash::BuildHasher, ops::Deref, rc::Rc};

use crate::{
    pts::{
        guard::Guards,
        invariant::Invariant,
        linear_polynomial::{
            coefficient::{Coefficient, Constant},
            Polynomial,
        },
        location::LocationHandle,
        relation::{Relation, RelationSign},
        system::{StateSystem, System},
        transition::{Transition, UpdateOperation},
        variable::{program_variable::ProgramVariable, set::VariableSet, Variable},
        PTS,
    },
    ranking_function_generators::linear_solvers::Interval,
    system, variables,
};

// cant be exported - uses private interface
macro_rules! template_var{
    [$variables:expr, $name: expr $(,)?]=> {
        {
            $crate::var!($variables, $crate::ranking_function_generators::farkas_based::TemplateVariable{ptr: std::rc::Rc::from($name)})
        }
    }
}

// cant be exported - uses template_var
macro_rules! template_domains {
    [
        $varset:expr
        $(
            ,$var:expr
            ,$lower:expr
            ,$upper:expr
        )*
        $(,)?
    ] => {
        {
            $crate::domains![$varset $(, $crate::ranking_function_generators::farkas_based::TemplateVariable{ptr: std::rc::Rc::from($var)}, $lower, $upper)*]
        }
    }
}

use super::{
    linear_solvers::{DomainMap, Goal, Problem, Solution, SolverError},
    Generator, GeneratorError, RankedPTS, RankingFunction,
};
pub type TemplateDomains = DomainMap<TemplateVariable>;

pub type TemplateVariables = VariableSet<TemplateVariable>;

#[derive(Hash, Clone, PartialEq, Eq)]
pub enum TemplateVariableData {
    Eps,
    UpperBound,
    LowerBound,
    RankingCoefficient(LocationHandle, ProgramVariable),
    RankingConstant(LocationHandle),
    FarkasVariable(usize),
}

impl TemplateVariableData
{
    fn unique_farkas_variable(seed: &mut usize) -> Self{
        let result = TemplateVariableData::FarkasVariable(seed.clone());
        *seed += 1;
        result
    }
}




fn option_debug_string<T: std::fmt::Display>(opt: &Option<T>) -> String {
    match opt {
        Some(x) => x.to_string(),
        None => "∅".to_string(),
    }
}

impl std::fmt::Debug for TemplateVariableData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateVariableData::Eps => write!(f, "ɛ"),
            TemplateVariableData::UpperBound => write!(f, "K'"),
            TemplateVariableData::LowerBound => write!(f, "K"),
            TemplateVariableData::FarkasVariable(id) => {
                write!(
                    f,
                    "x_{}_x",
                    id,
                )
            }
            TemplateVariableData::RankingCoefficient(a, var) => {
                write!(f, "a_{}_{}_a", option_debug_string(a), var)
            }
            TemplateVariableData::RankingConstant(a) => {
                write!(f, "b_{}_b", option_debug_string(a))
            }
        }
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

impl Variable for TemplateVariable {}

type Template = Polynomial<ProgramVariable, Polynomial<TemplateVariable, Constant>>;

type Predicate = System<TemplateVariable, Constant>;

fn farkas_assertion<S: BuildHasher + Default>(
    program_variables: &VariableSet<ProgramVariable, S>,
    template_variables: &mut TemplateVariables,
    polyhedron: &StateSystem,
    template: Relation<ProgramVariable, Polynomial<TemplateVariable, Constant>>,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize
) {
    assert!(template.is_nonstrict_inequality());
    // this splitting is not necessary, but this way its closer to standard farkas lemma notation
    let (c, d) = template.split_constant();

    let mut acc: Template = Default::default();
    for halfspace in polyhedron.iter() {
        // can be strict or non-strict due to conditional branching
        assert!(halfspace.is_nonstrict_inequality() || halfspace.is_strict_inequality());
        let (a, b) = halfspace.clone().split_constant();
        let y = template_var!(
            template_variables,
            
            TemplateVariableData::unique_farkas_variable(
                seed
            )
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

fn terminating_negativity<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize
) {
    let invariant = pts.locations.get_invariant(None).unwrap();
    assert!(!invariant.is_empty());
    let location = pts.locations.get_terminating_location();

    // location is valid => unwrap()
    for polyhedron in invariant.iter() {
        //upper
        let template = Relation::new(
            generate_template(template_variables, &pts.variables, location),
            RelationSign::LE,
            generate_constant_template(template_variables, TemplateVariableData::UpperBound),
        );
        farkas_assertion(
            &pts.variables,
            template_variables,
            polyhedron,
            template,
            domains,
            restrictions,
            seed
        );

        // lower
        let template = Relation::new(
            generate_template(template_variables, &pts.variables, location),
            RelationSign::GE,
            generate_constant_template(template_variables, TemplateVariableData::LowerBound),
        );
        farkas_assertion(
            &pts.variables,
            template_variables,
            polyhedron,
            template,
            domains,
            restrictions,
            seed
        );
    }
}

fn non_terminating_non_negativity<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
            seed:&mut usize
) {
    let invariant = pts.locations.get_invariant(location).unwrap();
    assert!(!invariant.is_empty());

    assert!(pts.locations.is_nonterminating_location(location));

    // location is valid => unwrap()
    for polyhedron in invariant.iter() {
        let template = Relation::new(
            generate_template(template_variables, &pts.variables, location),
            RelationSign::GE,
            Default::default(),
        );
        farkas_assertion(
            &pts.variables,
            template_variables,
            polyhedron,
            template,
            domains,
            restrictions,
            seed
        );
    }
}

fn martingale_difference<S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize) {
    assert!(!pts.locations.is_terminating_location(location));

    let guard = pts.locations.get_outgoing(location).unwrap();
    assert!(!guard.is_empty());

    match guard {
        Guards::Logic(transitions) => logic_martingale_difference(
            pts,
            template_variables,
            location,
            // handle is valid => unwrap
            transitions.iter(),
            domains,
            restrictions,
            seed
        ),
        Guards::Unguarded(boxed_transition) => nondeterministic_martingale_difference(
            pts,
            template_variables,
            location,
            // handle is valid => unwrap
            std::iter::once(&(**boxed_transition)),
            domains,
            restrictions,
            seed
        ),
        Guards::Probabilistic(transitions) => probabilistic_martingale_difference(
            pts,
            template_variables,
            location,
            // handle is valid => unwrap
            transitions.iter(),
            domains,
            restrictions,
            seed
        ),
        Guards::Nondeterministic(transitions) => nondeterministic_martingale_difference(
            pts,
            template_variables,
            location,
            // handle is valid => unwrap
            transitions.iter(),
            domains,
            restrictions,
            seed
        ),
    }
}

fn probabilistic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize
) where
    I: Iterator<Item = &'a (Constant, Transition)> + Clone,
{
    let invariant = pts.locations.get_invariant(location).unwrap();
    assert!(!invariant.is_empty());

    for polyhedron in invariant.iter() {
        let mut lhs: Template = Default::default();
        for (probability, transition) in branches.clone() {
            if !probability.is_zero() {
                let mut template = generate_template(
                    template_variables,
                    &pts.variables,
                    // handl is valid => unwrap
                    transition.target,
                );

                for op in transition.update_function.iter() {
                    match op {
                        UpdateOperation::Assignment(assignment) => {
                            template = assignment.apply(template)
                        }
                        UpdateOperation::Sampling(sampling) => {
                            template = sampling.expectation().apply(template)
                        }
                    }
                }

                template.mul_by_constant(probability.to_owned());
                lhs += template;
            }
        }

        let rhs = generate_template(template_variables, &pts.variables, location);
        lhs.add_term(
            generate_template_expression(template_variables, TemplateVariableData::Eps),
            None,
        );

        farkas_assertion(
            &pts.variables,
            template_variables,
            polyhedron,
            Relation::new(lhs, RelationSign::LE, rhs),
            domains,
            restrictions,
            seed
        )
    }
}

fn logic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize
) where
    I: Iterator<Item = &'a (Invariant, Transition)> + Clone,
{
    let invariant = pts.locations.get_invariant(location).unwrap();
    assert!(!invariant.is_empty());

    for (guards_invariant, transition) in branches.clone() {
        let mut lhs: Template = Default::default();
        let mut template = generate_template(
            template_variables,
            &pts.variables,
            // handl is valid => unwrap
            transition.target,
        );

        for op in transition.update_function.iter() {
            match op {
                UpdateOperation::Assignment(assignment) => template = assignment.apply(template),
                UpdateOperation::Sampling(sampling) => {
                    template = sampling.expectation().apply(template)
                }
            }
        }

        lhs += template;

        let rhs = generate_template(template_variables, &pts.variables, location);
        lhs.add_term(
            generate_template_expression(template_variables, TemplateVariableData::Eps),
            None,
        );

        let conditioned_polyhedra = invariant.clone() * &guards_invariant;

        for conditioned_polyhedron in conditioned_polyhedra.iter() {
            farkas_assertion(
                &pts.variables,
                template_variables,
                conditioned_polyhedron,
                Relation::new(lhs.clone(), RelationSign::LE, rhs.clone()),
                domains,
                restrictions,
                seed
            )
        }
    }
}

fn nondeterministic_martingale_difference<'a, I, S: BuildHasher + Default>(
    pts: &PTS<S>,
    template_variables: &mut TemplateVariables,
    location: LocationHandle,
    branches: I,
    domains: &mut TemplateDomains,
    restrictions: &mut Predicate,
    seed: &mut usize
) where
    I: Iterator<Item = &'a Transition> + Clone,
{
    let invariant = pts.locations.get_invariant(location).unwrap();
    assert!(!invariant.is_empty());

    for polyhedron in invariant.iter() {
        for transition in branches.clone() {
            let mut lhs: Template = Default::default();
            let mut template = generate_template(
                template_variables,
                &pts.variables,
                // handle is valid => unwrap
                transition.target,
            );

            for op in transition.update_function.iter() {
                match op {
                    UpdateOperation::Assignment(assignment) => {
                        template = assignment.apply(template)
                    }
                    UpdateOperation::Sampling(sampling) => {
                        template = sampling.expectation().apply(template)
                    }
                }
            }

            lhs += template;

            let rhs = generate_template(template_variables, &pts.variables, location);
            lhs.add_term(
                generate_template_expression(template_variables, TemplateVariableData::Eps),
                None,
            );

            farkas_assertion(
                &pts.variables,
                template_variables,
                polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                domains,
                restrictions,
                seed
            )
        }
    }
}

fn generate_template<S: BuildHasher + Default>(
    template_variables: &mut TemplateVariables,
    program_variables: &VariableSet<ProgramVariable, S>,
    location: LocationHandle,
) -> Template {
    let mut template = Template::default();
    for var in program_variables.into_iter() {
        template.add_term(
            generate_template_expression(
                template_variables,
                TemplateVariableData::RankingCoefficient(location, var.to_owned()),
            ),
            var.to_owned(),
        );
    }

    template.add_term(
        generate_template_expression(
            template_variables,
            TemplateVariableData::RankingConstant(location),
        ),
        None,
    );
    template
}

fn generate_constant_template(
    template_variables: &mut TemplateVariables,
    data: TemplateVariableData,
) -> Template {
    let mut template = Template::default();
    template.add_term(generate_template_expression(template_variables, data), None);

    template
}

fn generate_template_expression(
    template_variables: &mut TemplateVariables,
    data: TemplateVariableData,
) -> Polynomial<TemplateVariable, Constant> {
    let template_variable = template_var!(template_variables, data);
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

impl std::fmt::Display for TemplateVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.ptr, f)
    }
}

impl Generator for FarkasBasedGenerator {
    type VAR = TemplateVariable;
    fn generate_problem<S: super::linear_solvers::Solver>(
        pts: &crate::pts::PTS,
    ) -> Result<super::linear_solvers::Problem<TemplateVariable>, GeneratorError> {
        let mut template_variables = variables!();
        let mut domains = template_domains!(
            &mut template_variables,
            TemplateVariableData::Eps,
            1.0,
            f64::INFINITY,
            TemplateVariableData::UpperBound,
            f64::NEG_INFINITY,
            -1.0,
            TemplateVariableData::LowerBound,
            f64::NEG_INFINITY,
            -1.0,
        );
        let mut restrictions = system!();
        let mut seed:usize = 0;

        for location in pts.locations.iter() {
            let invariant = pts.locations.get_invariant(location).unwrap();
            for polyhedron in invariant.iter() {
                match S::is_empty(polyhedron) {
                    Ok(false) => (),
                    Ok(true) => {
                        return Err(GeneratorError::PolyhedronIsEmpty(
                            invariant.clone(),
                            polyhedron.clone(),
                        ))
                    }
                    Err(SolverError::InvalidRelationType) => {
                        return Err(GeneratorError::InvalidInvariant(invariant.clone()))
                    }
                    _ => unreachable!(),
                }
            }
            if pts.locations.is_nonterminating_location(location) {
                martingale_difference(
                    pts,
                    &mut template_variables,
                    location,
                    &mut domains,
                    &mut restrictions,
            &mut seed
                );
                non_terminating_non_negativity(
                    pts,
                    &mut template_variables,
                    location,
                    &mut domains,
                    &mut restrictions,
            &mut seed
                );
            }
        }
        terminating_negativity(
            pts,
            &mut template_variables,
            &mut domains,
            &mut restrictions,
            &mut seed
        );

        let goal = Goal::Minimize({
            let mut temp = Polynomial::default();
            temp.add_term(
                1.0,
                template_var!(&mut template_variables, TemplateVariableData::Eps),
            );
            temp
        });

        Ok(Problem {
            domains,
            variables: template_variables,
            restrictions,
            goal,
        })
    }

    fn build_ranking_function<S: Solution<Self::VAR>>(
        pts: PTS,
        solution: S,
    ) -> Result<super::RankedPTS, super::GeneratorError> {
        let mut rf = RankingFunction::new(&pts);
        let mut found_solution = false;
        for (variable, value) in solution {
            match variable.ptr.borrow() {
                TemplateVariableData::Eps => {
                    found_solution = value.is_nonnegative() && !value.is_zero();
                }
                TemplateVariableData::RankingConstant(location) => {
                    rf.get_mut(location.clone()).unwrap().add_term(value, None)
                }
                TemplateVariableData::RankingCoefficient(location, var) => rf
                    .get_mut(location.clone())
                    .unwrap()
                    .add_term(value, Some(var.clone())),
                _ => (),
            }
        }
        if !found_solution {
            Err(GeneratorError::InvalidSolution)
        } else {
            Ok(RankedPTS { pts, function: rf })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::hash_map::{DefaultHasher, RandomState},
        hash::BuildHasherDefault,
    };

    use crate::{
        guards, invariant, program_var, program_variables,
        pts::{linear_polynomial::coefficient::Constant, variable::set::VariableSet, PTS},
        ranking_function_generators::{
            farkas_based::{non_terminating_non_negativity, TemplateVariableData},
            linear_solvers::{minilp::Minilp, Solution},
            Generator, RankedPTS, RankingFunction,
        },
        state_assignment, transition,
    };

    use super::{FarkasBasedGenerator, TemplateVariable};

    // cant be exported - uses template_var
    macro_rules! template_variables{
    [ $( $x:expr ),+ $(,)?] => {
        {
            $crate::variables![$($crate::ranking_function_generators::farkas_based::TemplateVariable{ptr: std::rc::Rc::from($x)},)+]
        }
    };
    [] => {
        {
            $crate::pts::variable::set::TemplateVariables::default()
        }
    }
}

    // cant be exported - uses template_var
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
            let temp: $crate::ranking_function_generators::farkas_based::Predicate = $crate::system!(
                $(
                    $(
                        $crate::relation!(
                            $sign,
                            $constant,
                            $varset
                            $(
                                ,
                                $coeff,
                                $crate::ranking_function_generators::farkas_based::TemplateVariable{ptr: std::rc::Rc::from($var)}
                            )*



                        ),
                    )*
                )?

            );
            temp
        }
    };
}

    impl Solution<TemplateVariable> for Vec<(TemplateVariable, Constant)> {}

    #[test]
    fn invalid_invariant() {
        let mut pts = PTS::default();
        let start_location = pts.locations.new_location();
        let intermediate_location = pts.locations.new_location();
        pts.locations.set_invariant(
            start_location,
            invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], []),
        );
        pts.locations
            .set_outgoing(
                start_location,
                guards!(P:
                        0.25,
                        transition!(intermediate_location, state_assignment!(&mut pts.variables, "a", 3.0, 7.0, "a")),
                        0.75,
                        transition!(None)
                ),
            )
            .unwrap();
        assert!(FarkasBasedGenerator::generate_problem::<Minilp>(&pts).is_err());
    }

    #[test]
    fn build_ranking_function() {
        let mut template_variables: VariableSet<TemplateVariable, RandomState> = Default::default();
        let mut pts = PTS::default();
        let start_location = pts.locations.new_location();
        let intermediate_location = pts.locations.new_location();
        pts.locations.set_invariant(
            start_location,
            invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], [">=", 0.0]),
        );
        pts.locations
            .set_outgoing(
                start_location,
                guards!(P:
                        0.25,
                        transition!(intermediate_location, state_assignment!(&mut pts.variables, "a", 3.0, 7.0, "a")),
                        0.75,
                        transition!(None)
                ),
            )
            .unwrap();
        let var = program_var!(&mut pts.variables, "a");
        let solution = vec![
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingConstant(Some(0)),
                ),
                Constant(1.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingConstant(None),
                ),
                Constant(2.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
                ),
                Constant(3.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingCoefficient(None, var.clone()),
                ),
                Constant(4.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingCoefficient(Some(0), var.clone()),
                ),
                Constant(5.0),
            ),
            (
                template_var!(&mut template_variables, TemplateVariableData::Eps),
                Constant(6.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingConstant(Some(1)),
                ),
                Constant(7.0),
            ),
            (
                template_var!(
                    &mut template_variables,
                    TemplateVariableData::RankingCoefficient(Some(1), var.clone())
                ),
                Constant(8.0),
            ),
        ];

        let mut function = RankingFunction::new(&pts);

        function.get_mut(Some(0)).unwrap().add_term(1.0, None);
        function.get_mut(None).unwrap().add_term(2.0, None);
        function
            .get_mut(None)
            .unwrap()
            .add_term(4.0, Some(var.clone()));
        function
            .get_mut(Some(0))
            .unwrap()
            .add_term(5.0, var.clone());
        function.get_mut(Some(1)).unwrap().add_term(7.0, None);
        function
            .get_mut(Some(1))
            .unwrap()
            .add_term(8.0, var.clone());
        assert_eq!(
            FarkasBasedGenerator::build_ranking_function(pts.clone(), solution).unwrap(),
            RankedPTS { pts, function }
        );
    }

    #[test]
    fn unrestricted_invariant() {
        let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
        pts.variables = program_variables!("a");
        pts.locations.set_invariant(
            pts.locations.get_terminating_location(),
            invariant!(&mut pts.variables, [">=", 0.0]),
        );

        let mut template_variables = Default::default();
        let mut domains = Default::default();
        let mut restrictions = Default::default();
        super::terminating_negativity(
            &pts,
            &mut template_variables,
            &mut domains,
            &mut restrictions,
            &mut 0
        );

        assert_eq!(
            template_variables,
            template_variables!(
                TemplateVariableData::RankingCoefficient(
                    None,
                    program_var!(&mut pts.variables, "a")
                ),
                TemplateVariableData::FarkasVariable(0),
                TemplateVariableData::FarkasVariable(1),
                TemplateVariableData::RankingConstant(None),
                TemplateVariableData::UpperBound,
                TemplateVariableData::LowerBound,
            )
        );

        assert_eq!(
            domains,
            template_domains!(
                &mut template_variables,
                TemplateVariableData::FarkasVariable(0),
                0.0,
                f64::INFINITY,
                TemplateVariableData::FarkasVariable(1),
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
                -1.0,
                TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a"));

                "<=",
                0.0,
                1.0,
                TemplateVariableData::RankingConstant(None),
                -1.0,
                TemplateVariableData::UpperBound;

                "==",
                0.0,
                1.0,
                TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a"));

                "<=",
                0.0,
                -1.0,
                TemplateVariableData::RankingConstant(None),
                1.0,
                TemplateVariableData::LowerBound;
            )
        );
    }

    #[test]
    fn terminating_negativity() {
        let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
        pts.locations
            .set_invariant(None, invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"]));
        let mut template_variables = Default::default();
        let mut domains = Default::default();
        let mut restrictions = Default::default();
        super::terminating_negativity(
            &pts,
            &mut template_variables,
            &mut domains,
            &mut restrictions,
            &mut 0
        );
        assert_eq!(
            template_variables,
            template_variables!(
                TemplateVariableData::RankingCoefficient(
                    None,
                    program_var!(&mut pts.variables, "a")
                ),
                TemplateVariableData::FarkasVariable(0),
                TemplateVariableData::FarkasVariable(1),
                TemplateVariableData::RankingConstant(None),
                TemplateVariableData::UpperBound,
                TemplateVariableData::LowerBound,
            )
        );

        assert_eq!(
            domains,
            template_domains!(
                &mut template_variables,
                TemplateVariableData::FarkasVariable(0),
                0.0,
                f64::INFINITY,
                TemplateVariableData::FarkasVariable(1),
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
                TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                -2.0,
                TemplateVariableData::FarkasVariable(0);

                "<=",
                0.0,
                -5.0,
                TemplateVariableData::FarkasVariable(0),
                1.0,
                TemplateVariableData::RankingConstant(None),
                -1.0,
                TemplateVariableData::UpperBound;

                "==",
                0.0,
                1.0,
                TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                -2.0,
                TemplateVariableData::FarkasVariable(1);

                "<=",
                0.0,
                -5.0,
                TemplateVariableData::FarkasVariable(1),
                -1.0,
                TemplateVariableData::RankingConstant(None),
                1.0,
                TemplateVariableData::LowerBound;
            )
        );
    }

    #[test]
    fn nonterminating_nonnegativity() {
        let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
        let location = pts.locations.new_location();
        pts.locations.set_invariant(
            location,
            invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"]),
        );
        let mut template_variables = Default::default();
        let mut domains = Default::default();
        let mut restrictions = Default::default();
        non_terminating_non_negativity(
            &pts,
            &mut template_variables,
            location,
            &mut domains,
            &mut restrictions,
            &mut 0
        );
        assert_eq!(
            template_variables,
            template_variables!(
                TemplateVariableData::RankingCoefficient(
                    Some(0),
                    program_var!(&mut pts.variables, "a")
                ),
                TemplateVariableData::FarkasVariable(0),
                TemplateVariableData::RankingConstant(Some(0)),
            )
        );

        assert_eq!(
            domains,
            template_domains!(
                &mut template_variables,
                TemplateVariableData::FarkasVariable(0),
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
                TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                -2.0,
                TemplateVariableData::FarkasVariable(0);

                "<=",
                0.0,
                -5.0,
                TemplateVariableData::FarkasVariable(0),
                -1.0,
                TemplateVariableData::RankingConstant(Some(0));

            )
        );
    }

    mod martingale_difference {
        use crate::{
            guards, invariant, program_var,
            pts::PTS,
            ranking_function_generators::farkas_based::{
                martingale_difference, TemplateVariableData,
            },
            state_assignment, transition,
        };
        use pretty_assertions::assert_eq;
        use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};

        #[test]
        fn probabilistic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations.set_invariant(
                start_location,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"]),
            );
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(P:
                            0.25,
                            transition!(intermediate_location,
                                        state_assignment!(&mut pts.variables, "a", 3.0, 7.0, "a") ),
                            0.75,
                            transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                template_variables,
                template_variables!(
                    TemplateVariableData::RankingCoefficient(
                        Some(0),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        Some(1),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        None,
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::FarkasVariable(0),
                    TemplateVariableData::RankingConstant(Some(0)),
                    TemplateVariableData::RankingConstant(Some(1)),
                    TemplateVariableData::RankingConstant(None),
                    TemplateVariableData::Eps,
                )
            );

            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
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
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -1.75,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -0.75,
                    TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(0);

                    "<=",
                    0.0,
                    0.75,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -5.0,
                    TemplateVariableData::FarkasVariable(0),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    0.25,
                    TemplateVariableData::RankingConstant(Some(1)),
                    0.75,
                    TemplateVariableData::RankingConstant(None),
                    1.0,
                    TemplateVariableData::Eps;
                )
            );
        }

        #[test]
        fn logic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations.set_invariant(
                start_location,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"], [">=", 0.0]),
            );
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(L:
                            invariant!(&mut pts.variables,[ "<=", 11.0, -1.0, "a"]),
                            transition!(intermediate_location, state_assignment!( &mut pts.variables, "a", 3.0, 7.0, "a")),
                            invariant!(&mut pts.variables,[ ">", 11.0, -1.0, "a"]),
                            transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                template_variables,
                template_variables!(
                    TemplateVariableData::RankingCoefficient(
                        Some(0),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        Some(1),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        None,
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::FarkasVariable(0),
                    TemplateVariableData::FarkasVariable(1),
                    TemplateVariableData::FarkasVariable(2),
                    TemplateVariableData::FarkasVariable(3),
                    TemplateVariableData::FarkasVariable(4),
                    TemplateVariableData::FarkasVariable(5),
                    TemplateVariableData::FarkasVariable(6),
                    TemplateVariableData::FarkasVariable(7),
                    TemplateVariableData::RankingConstant(Some(0)),
                    TemplateVariableData::RankingConstant(Some(1)),
                    TemplateVariableData::RankingConstant(None),
                    TemplateVariableData::Eps,
                )
            );

            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(1),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(2),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(3),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(4),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(5),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(6),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(7),
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
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -7.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(0),
                    -1.0,
                    TemplateVariableData::FarkasVariable(1);

                    "<=",
                    0.0,
                    3.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -5.0,
                    TemplateVariableData::FarkasVariable(0),
                    -11.0,
                    TemplateVariableData::FarkasVariable(1),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(Some(1)),
                    1.0,
                    TemplateVariableData::Eps;

                    "==",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -7.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    0.0,
                    TemplateVariableData::FarkasVariable(2),
                    -1.0,
                    TemplateVariableData::FarkasVariable(3);


                    "<=",
                    0.0,
                    3.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    0.0,
                    TemplateVariableData::FarkasVariable(2),
                    -11.0,                                
                    TemplateVariableData::FarkasVariable(3),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(Some(1)),
                    1.0,
                    TemplateVariableData::Eps;

                    ////////////////////////////////////////////////////////////
                    "==",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -1.0,
                    TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(4),
                    1.0,
                    TemplateVariableData::FarkasVariable(5);

                    "<=",
                    0.0,
                    -5.0,
                    TemplateVariableData::FarkasVariable(4),
                    11.0,                                 
                    TemplateVariableData::FarkasVariable(5),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(None),
                    1.0,
                    TemplateVariableData::Eps;


                    "==",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -1.0,
                    TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                    0.0,
                    TemplateVariableData::FarkasVariable(6),
                    1.0,
                    TemplateVariableData::FarkasVariable(7);

                    "<=",
                    0.0,
                    0.0,
                    TemplateVariableData::FarkasVariable(6),
                    11.0,                                 
                    TemplateVariableData::FarkasVariable(7),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(None),
                    1.0,
                    TemplateVariableData::Eps;
                )
            );
        }

        #[test]
        fn nondeterministic() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations.set_invariant(
                start_location,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"]),
            );
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(
                        transition!(
                            intermediate_location,
                            state_assignment!(&mut pts.variables, "a", 3.0, 7.0, "a")
                        ),
                        transition!(None)
                    ),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                template_variables,
                template_variables!(
                    TemplateVariableData::RankingCoefficient(
                        Some(0),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        Some(1),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        None,
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::FarkasVariable(0),
                    TemplateVariableData::FarkasVariable(1),
                    TemplateVariableData::RankingConstant(Some(0)),
                    TemplateVariableData::RankingConstant(Some(1)),
                    TemplateVariableData::RankingConstant(None),
                    TemplateVariableData::Eps,
                )
            );

            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(1),
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
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -7.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(0);

                    "<=",
                    0.0,
                    3.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -5.0,
                    TemplateVariableData::FarkasVariable(0),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(Some(1)),
                    1.0,
                    TemplateVariableData::Eps;

                    "==",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -1.0,
                    TemplateVariableData::RankingCoefficient(None, program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(1);

                    "<=",
                    0.0,
                    -5.0,
                    TemplateVariableData::FarkasVariable(1),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(None),
                    1.0,
                    TemplateVariableData::Eps;
                )
            );
        }

        #[test]
        fn single() {
            let mut pts = PTS::<BuildHasherDefault<DefaultHasher>>::default();
            let start_location = pts.locations.new_location();
            let intermediate_location = pts.locations.new_location();
            pts.locations.set_invariant(
                start_location,
                invariant!(&mut pts.variables, ["<=", 5.0, -2.0, "a"]),
            );
            pts.locations
                .set_outgoing(
                    start_location,
                    guards!(transition!(
                        intermediate_location,
                        state_assignment!(&mut pts.variables, "a", 3.0, 7.0, "a")
                    ),),
                )
                .unwrap();

            let mut template_variables = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            martingale_difference(
                &pts,
                &mut template_variables,
                start_location,
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                template_variables,
                template_variables!(
                    TemplateVariableData::RankingCoefficient(
                        Some(0),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::RankingCoefficient(
                        Some(1),
                        program_var!(&mut pts.variables, "a")
                    ),
                    TemplateVariableData::FarkasVariable(0),
                    TemplateVariableData::RankingConstant(Some(0)),
                    TemplateVariableData::RankingConstant(Some(1)),
                    TemplateVariableData::Eps,
                )
            );

            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
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
                    TemplateVariableData::RankingCoefficient(Some(0), program_var!(&mut pts.variables, "a")),
                    -7.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -2.0,
                    TemplateVariableData::FarkasVariable(0);

                    "<=",
                    0.0,
                    3.0,
                    TemplateVariableData::RankingCoefficient(Some(1), program_var!(&mut pts.variables, "a")),
                    -5.0,
                    TemplateVariableData::FarkasVariable(0),
                    -1.0,
                    TemplateVariableData::RankingConstant(Some(0)),
                    1.0,
                    TemplateVariableData::RankingConstant(Some(1)),
                    1.0,
                    TemplateVariableData::Eps;
                )
            );
        }
    }

    mod farkas_assertion {
        use std::{collections::hash_map::DefaultHasher, hash::BuildHasherDefault};

        use crate::{
            program_variables,
            pts::{
                relation::{Relation, RelationSign},
                variable::{
                    program_variable::{ProgramVariable, ProgramVariables},
                    set::VariableSet,
                },
            },
            ranking_function_generators::farkas_based::{
                farkas_assertion, generate_constant_template, generate_template,
                TemplateVariableData,
            },
            state_system,
        };

        #[test]
        fn empty() {
            {
                // no restrictions
                let program_variables: ProgramVariables = program_variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let lhs = Default::default();
                let rhs = Default::default();
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    &state_system!(&mut program_variables; ">=", 0.0),
                    Relation::new(lhs, RelationSign::LE, rhs),
                    &mut domains,
                    &mut restrictions,
                    &mut 8
                );
                assert_eq!(
                    domains,
                    template_domains!(
                        &mut template_variables,
                        TemplateVariableData::FarkasVariable(8),
                        0.0,
                        f64::INFINITY,
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

            {
                // All restrictions true
                let program_variables: ProgramVariables = program_variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let lhs = Default::default();
                let rhs = Default::default();
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    &state_system!(
                        &mut program_variables;
                        "<=", 0.0;
                        ">=", 0.0;
                        "<=", 0.0),
                    Relation::new(lhs, RelationSign::LE, rhs),
                    &mut domains,
                    &mut restrictions,
                    &mut 0
                );
                assert_eq!(domains.len(), 3);
                assert_eq!(
                    domains,
                    template_domains!(
                        &mut template_variables,
                        TemplateVariableData::FarkasVariable(0),
                        0.0,
                        f64::INFINITY,
                        TemplateVariableData::FarkasVariable(1),
                        0.0,
                        f64::INFINITY,
                        TemplateVariableData::FarkasVariable(2),
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
                let program_variables: ProgramVariables = program_variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let eps_pol =
                    generate_constant_template(&mut template_variables, TemplateVariableData::Eps);
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    &state_system!(&mut program_variables; ">=", 0.0),
                    Relation::new(eps_pol.clone(), RelationSign::LE, Default::default()),
                    &mut domains,
                    &mut restrictions,
                    &mut 0
                );

                assert_eq!(
                    domains,
                    template_domains!(
                        &mut template_variables,
                        TemplateVariableData::FarkasVariable(0),
                        0.0,
                        f64::INFINITY,
                    )
                );

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0, 1.0, TemplateVariableData::Eps
                    )
                );
            }
            {
                // Eps on RHS
                let program_variables: ProgramVariables = program_variables!("a", "b", "c");
                let mut template_variables = Default::default();
                let eps_pol =
                    generate_constant_template(&mut template_variables, TemplateVariableData::Eps);
                let mut domains = Default::default();
                let mut restrictions = Default::default();
                farkas_assertion(
                    &program_variables,
                    &mut template_variables,
                    &state_system!(&mut program_variables; ">=", 0.0),
                    Relation::new(Default::default(), RelationSign::LE, eps_pol),
                    &mut domains,
                    &mut restrictions,
                    &mut 0
                );
                assert_eq!(
                    domains,
                    template_domains!(
                        &mut template_variables,
                        TemplateVariableData::FarkasVariable(0),
                        0.0,
                        f64::INFINITY,
                    )
                );

                assert_eq!(
                    restrictions,
                    predicate!(
                        &mut template_variables;
                        "==", 0.0;
                        "==", 0.0;
                        "==", 0.0;
                        "<=", 0.0, -1.0, TemplateVariableData::Eps
                    )
                );
            }
        }

        #[test]
        fn template() {
            let program_variables: VariableSet<ProgramVariable, BuildHasherDefault<DefaultHasher>> =
                program_variables!("a", "b", "c");
            let mut template_variables = Default::default();
            let lhs = generate_template(&mut template_variables, &program_variables, Some(5));
            let rhs = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            farkas_assertion(
                &program_variables,
                &mut template_variables,
                &state_system!(&mut program_variables; ">=", 0.0),
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
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
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("c").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("b").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("a").unwrap().clone(),
                    );

                    "<=",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingConstant(
                        Some(5),
                    );
                )
            );
        }

        #[test]
        fn two_templates() {
            let program_variables: VariableSet<ProgramVariable, BuildHasherDefault<DefaultHasher>> =
                program_variables!("a", "b", "c");
            let mut template_variables = Default::default();
            let lhs = generate_template(&mut template_variables, &program_variables, Some(5));
            let rhs = generate_template(&mut template_variables, &program_variables, Some(6));
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            farkas_assertion(
                &program_variables,
                &mut template_variables,
                &state_system!(&mut program_variables; ">=", 0.0),
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
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
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("c").unwrap().clone(),
                    ),
                    1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(6),
                        program_variables.get("c").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("b").unwrap().clone(),
                    ),
                    1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(6),
                        program_variables.get("b").unwrap().clone(),
                    );

                    "==",
                    0.0,
                    -1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("a").unwrap().clone(),
                    ),
                    1.0,
                    TemplateVariableData::RankingCoefficient(
                        Some(6),
                        program_variables.get("a").unwrap().clone(),
                    );

                    "<=",
                    0.0,
                    1.0,
                    TemplateVariableData::RankingConstant(
                        Some(5),
                    ),
                    -1.0,
                    TemplateVariableData::RankingConstant(
                        Some(6),
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
                > = program_variables!("a", "b", "c");
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
            farkas_assertion(
                &program_variables,
                &mut template_variables,
                &polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(domains.len(), 3);
            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(1),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(2),
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
                TemplateVariableData::FarkasVariable(0);
                "==",
                0.0,
                1.0,
                TemplateVariableData::FarkasVariable(0),
                0.5,
                TemplateVariableData::FarkasVariable(2);
                "==",
                0.0,
                5.0,
                TemplateVariableData::FarkasVariable(1);
                "<=",
                0.0,
                1.0,
                TemplateVariableData::FarkasVariable(1),
                -10.0,
                TemplateVariableData::FarkasVariable(2);
                )
            );
        }

        #[test]
        fn template_restricted() {
            let (program_variables, polyhedron) = {
                let program_variables_original: VariableSet<
                    ProgramVariable,
                    BuildHasherDefault<DefaultHasher>,
                > = program_variables!("a", "b", "c");
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
                let mut tmp =
                    generate_template(&mut template_variables, &program_variables, Some(5));
                tmp.mul_by_constant(0.1.into());
                tmp
            };
            let rhs = Default::default();
            let mut domains = Default::default();
            let mut restrictions = Default::default();
            farkas_assertion(
                &program_variables,
                &mut template_variables,
                &polyhedron,
                Relation::new(lhs, RelationSign::LE, rhs),
                &mut domains,
                &mut restrictions,
                &mut 0
            );
            assert_eq!(domains.len(), 3);
            assert_eq!(
                domains,
                template_domains!(
                    &mut template_variables,
                    TemplateVariableData::FarkasVariable(0),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(1),
                    0.0,
                    f64::INFINITY,
                    TemplateVariableData::FarkasVariable(2),
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
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("b").unwrap().clone(),
                    ),
                    2.0,
                    TemplateVariableData::FarkasVariable(
                        0,
                    );

                    "==",
                    0.0,
                    -0.1,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("a").unwrap().clone(),
                    ),
                    1.0,
                    TemplateVariableData::FarkasVariable(
                        0,
                    ),
                    0.5,
                    TemplateVariableData::FarkasVariable(
                        2,
                    );

                    "==",
                    0.0,
                    -0.1,
                    TemplateVariableData::RankingCoefficient(
                        Some(5),
                        program_variables.get("c").unwrap().clone(),
                    ),
                    5.0,
                    TemplateVariableData::FarkasVariable(
                        1,
                    );

                    "<=",
                    0.0,
                    1.0,
                    TemplateVariableData::FarkasVariable(
                        1,
                    ),
                    -10.0,
                    TemplateVariableData::FarkasVariable(
                        2,
                    ),
                    0.1,
                    TemplateVariableData::RankingConstant(Some(5))
                )
            );
        }
    }
}
