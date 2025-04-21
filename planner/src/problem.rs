use crate::{
    action::Action,
    calculus::{
        first_order::{Pdnf, QuantifiedFormula},
        predicate::{GroundPredicate, PredicateDefinition, PredicateError, ScopedPredicate},
    },
    entity::{EntityStorage, ObjectStorage, TypeStorage},
    sealed::Sealed,
    state::State,
    util::{deep_clone::DeepClone, named::NamedStorage},
    InternerSymbol, INTERNER,
};
use alloc::{boxed::Box, collections::BTreeSet, string::String, vec::Vec};
use core::marker::PhantomData;
use itertools::Itertools;

pub use pddl::Requirement;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Domain {
    pub name: InternerSymbol,
    pub requirements: BTreeSet<Requirement>,
    pub entities: EntityStorage,
    pub predicate_definitions: NamedStorage<PredicateDefinition>,
    pub actions: NamedStorage<Action>,
}

impl Domain {
    pub fn new_problem<O, I, G>(&self, name: &str) -> ProblemBuilder<O, I, G, New>
    where
        O: FnMut(
            &BTreeSet<Requirement>,
            &dyn TypeStorage,
            &mut dyn ObjectStorage,
        ) -> Result<(), BuildError>,
        I: FnMut(
            &BTreeSet<Requirement>,
            &dyn ObjectStorage,
            &NamedStorage<PredicateDefinition>,
            &mut Vec<GroundPredicate>,
        ) -> Result<(), BuildError>,
        G: Fn(
            &BTreeSet<Requirement>,
            &dyn TypeStorage,
            &dyn ObjectStorage,
            &NamedStorage<PredicateDefinition>,
        ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
    {
        ProblemBuilder {
            problem_name: INTERNER.lock().get_or_intern(name),
            domain: self.deep_clone(),
            add_objects: None,
            add_init: None,
            add_goal: None,
            state: PhantomData,
        }
    }
}

impl DeepClone for Domain {
    fn deep_clone(&self) -> Self {
        Self {
            name: self.name,
            requirements: self.requirements.clone(),
            entities: self.entities.deep_clone(),
            predicate_definitions: self.predicate_definitions.clone(),
            actions: self.actions.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Problem {
    pub name: InternerSymbol,
    pub domain_name: InternerSymbol,
    pub entities: EntityStorage,
    pub actions: NamedStorage<Action>,
    pub init: State,
    pub goal: Pdnf<ScopedPredicate>,
}

pub const SUPPORTED_REQUIREMENTS: [Requirement; 9] = [
    Requirement::Strips,
    Requirement::Typing,
    Requirement::NegativePreconditions,
    Requirement::DisjunctivePreconditions,
    Requirement::ExistentialPreconditions,
    Requirement::UniversalPreconditions,
    Requirement::QuantifiedPreconditions,
    Requirement::ConditionalEffects,
    Requirement::Adl,
];

pub const REQUIRED_REQUIREMENTS: [Requirement; 2] = [Requirement::Strips, Requirement::Typing];

#[derive(Debug)]
pub enum BuildError {
    /// These requirements are unsupported by this parser.
    UnsupportedRequirements(Vec<Requirement>),
    /// These requirements are necessary for the parser.
    MissingRequirements(Vec<Requirement>),
    /// This [Requirement] is necessary
    /// to parse the [Domain] or [Problem].
    Requires(Requirement),
    /// This operation is unsupported by this planner.
    UnsupportedFeature(UnsupportedFeature),
    /// This operation is forbidden by the enabled requirements.
    ForbiddenOperation,
    /// Bad [Domain] or [Problem] definition
    /// like having an undeclared predicate in action's precondition
    /// or a reference to a variable that doesn't exist.
    BadDefinition(BadDefinition),
    /// Error when constructing a [Predicate](crate::calculus::predicate::Predicate).
    PredicateError(PredicateError),
}

#[derive(Debug)]
pub enum UnsupportedFeature {
    ExtendsDomain,
    EitherOfTypes,
    Function,
    Preference,
    NumericFluent,
    ObjectFluent,
    DurativeAction,
    Derive,
    TimedInitLiteral,
    Constraints,
    MetricSpec,
    LengthSpec,
    ProblemRequirements,
    NegationInInit,
}

#[derive(Debug)]
pub enum BadDefinition {
    UnknownType(String),
    UnknownObject(String),
    UnknownPredicate(String),
    UnknownParameter(String),
    WrongDomain(String),
    /// [Literal](pddl::TermLiteral) contains [AtomicFormula](pddl::AtomicFormula) inside.
    /// Docs say that this is requires Negative Precondition,
    /// but tests show that this is not needed.
    LiteralInGoalDefinition,
}

#[allow(private_bounds)]
pub trait DomainBuilderState: Sealed {}

pub struct New;
pub struct HasName;
pub struct HasRequirements;
pub struct HasTypes;
pub struct HasConsts;
pub struct HasPredicateDefinitions;
pub struct HasActions;

impl DomainBuilderState for New {}
impl DomainBuilderState for HasName {}
impl DomainBuilderState for HasRequirements {}
impl DomainBuilderState for HasTypes {}
impl DomainBuilderState for HasConsts {}
impl DomainBuilderState for HasPredicateDefinitions {}
impl DomainBuilderState for HasActions {}

impl Sealed for New {}
impl Sealed for HasName {}
impl Sealed for HasRequirements {}
impl Sealed for HasTypes {}
impl Sealed for HasConsts {}
impl Sealed for HasPredicateDefinitions {}
impl Sealed for HasActions {}

pub struct DomainBuilder<T, C, P, A, S>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    name: InternerSymbol,
    requirements: BTreeSet<Requirement>,
    add_types: Option<Box<T>>,
    add_consts: Option<Box<C>>,
    add_predicate_defnitions: Option<Box<P>>,
    add_actions: Option<Box<A>>,
    state: PhantomData<S>,
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, New>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn new_domain(name: &str) -> DomainBuilder<T, C, P, A, HasName> {
        DomainBuilder {
            name: INTERNER.lock().get_or_intern(name),
            requirements: BTreeSet::new(),
            add_types: None,
            add_consts: None,
            add_predicate_defnitions: None,
            add_actions: None,
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasName>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn requirements(
        self,
        requirements: BTreeSet<Requirement>,
    ) -> DomainBuilder<T, C, P, A, HasRequirements> {
        DomainBuilder {
            name: self.name,
            requirements,
            add_types: self.add_types,
            add_consts: self.add_consts,
            add_predicate_defnitions: self.add_predicate_defnitions,
            add_actions: self.add_actions,
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasRequirements>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn types(self, add_types: T) -> DomainBuilder<T, C, P, A, HasTypes> {
        DomainBuilder {
            name: self.name,
            requirements: self.requirements,
            add_types: Some(Box::new(add_types)),
            add_consts: self.add_consts,
            add_predicate_defnitions: self.add_predicate_defnitions,
            add_actions: self.add_actions,
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasTypes>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn consts(self, add_consts: C) -> DomainBuilder<T, C, P, A, HasConsts> {
        DomainBuilder {
            name: self.name,
            requirements: self.requirements,
            add_types: self.add_types,
            add_consts: Some(Box::new(add_consts)),
            add_predicate_defnitions: self.add_predicate_defnitions,
            add_actions: self.add_actions,
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasConsts>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn predicate_definitions(
        self,
        add_predicate_definitions: P,
    ) -> DomainBuilder<T, C, P, A, HasPredicateDefinitions> {
        DomainBuilder {
            name: self.name,
            requirements: self.requirements,
            add_types: self.add_types,
            add_consts: self.add_consts,
            add_predicate_defnitions: Some(Box::new(add_predicate_definitions)),
            add_actions: self.add_actions,
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasPredicateDefinitions>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn actions(self, add_actions: A) -> DomainBuilder<T, C, P, A, HasActions> {
        DomainBuilder {
            name: self.name,
            requirements: self.requirements,
            add_types: self.add_types,
            add_consts: self.add_consts,
            add_predicate_defnitions: self.add_predicate_defnitions,
            add_actions: Some(Box::new(add_actions)),
            state: PhantomData,
        }
    }
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, HasActions>
where
    T: FnMut(&BTreeSet<Requirement>, &mut dyn TypeStorage) -> Result<(), BuildError>,
    C: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    P: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut NamedStorage<PredicateDefinition>,
    ) -> Result<(), BuildError>,
    A: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ) -> Result<(), BuildError>,
{
    pub fn build(self) -> Result<Domain, BuildError> {
        let (requirements, unsupported_requirements): (Vec<_>, Vec<_>) = self
            .requirements
            .iter()
            .partition(|r| SUPPORTED_REQUIREMENTS.contains(r));

        let mut requirements = requirements.into_iter().collect::<BTreeSet<_>>();
        if requirements.contains(&Requirement::Adl) {
            requirements.insert(Requirement::Strips);
            requirements.insert(Requirement::Typing);
            requirements.insert(Requirement::NegativePreconditions);
            requirements.insert(Requirement::DisjunctivePreconditions);
            requirements.insert(Requirement::Equality);
            requirements.insert(Requirement::QuantifiedPreconditions);
            requirements.insert(Requirement::ConditionalEffects);
        }
        if requirements.contains(&Requirement::QuantifiedPreconditions) {
            requirements.insert(Requirement::UniversalPreconditions);
            requirements.insert(Requirement::ExistentialPreconditions);
        }

        if !unsupported_requirements.is_empty() {
            return Err(BuildError::UnsupportedRequirements(
                unsupported_requirements,
            ));
        }

        let missing_requirements = REQUIRED_REQUIREMENTS
            .iter()
            .filter(|&r| !requirements.contains(r))
            .cloned()
            .collect_vec();
        if !missing_requirements.is_empty() {
            return Err(BuildError::MissingRequirements(missing_requirements));
        }

        let mut entities = EntityStorage::default();
        let mut predicate_definitions = NamedStorage::default();
        let mut actions = NamedStorage::default();
        self.add_types.unwrap()(&requirements, &mut entities).and_then(|_| {
            self.add_consts.unwrap()(&requirements, &entities.clone(), &mut entities).and_then(
                |_| {
                    self.add_predicate_defnitions.unwrap()(
                        &requirements,
                        &entities,
                        &mut predicate_definitions,
                    )
                    .and_then(|_| {
                        self.add_actions.unwrap()(
                            &requirements,
                            &entities,
                            &entities,
                            &predicate_definitions,
                            &mut actions,
                        )
                    })
                    .map(|_| Domain {
                        name: self.name,
                        requirements,
                        entities,
                        predicate_definitions,
                        actions,
                    })
                },
            )
        })
    }
}

#[allow(private_bounds)]
pub trait ProblemBuilderState: Sealed {}

pub struct HasObjects;
pub struct HasInit;
pub struct HasGoal;

impl ProblemBuilderState for New {}
impl ProblemBuilderState for HasObjects {}
impl ProblemBuilderState for HasInit {}
impl ProblemBuilderState for HasGoal {}

impl Sealed for HasObjects {}
impl Sealed for HasInit {}
impl Sealed for HasGoal {}

pub struct ProblemBuilder<O, I, G, S>
where
    O: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    I: FnMut(
        &BTreeSet<Requirement>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut Vec<GroundPredicate>,
    ) -> Result<(), BuildError>,
    G: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
    S: ProblemBuilderState,
{
    problem_name: InternerSymbol,
    domain: Domain,
    add_objects: Option<Box<O>>,
    add_init: Option<Box<I>>,
    add_goal: Option<Box<G>>,
    state: PhantomData<S>,
}

impl<O, I, G> ProblemBuilder<O, I, G, New>
where
    O: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    I: FnMut(
        &BTreeSet<Requirement>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut Vec<GroundPredicate>,
    ) -> Result<(), BuildError>,
    G: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
{
    pub fn objects(self, add_objects: O) -> ProblemBuilder<O, I, G, HasObjects> {
        ProblemBuilder {
            problem_name: self.problem_name,
            domain: self.domain,
            add_objects: Some(Box::new(add_objects)),
            add_init: self.add_init,
            add_goal: self.add_goal,
            state: PhantomData,
        }
    }
}

impl<O, I, G> ProblemBuilder<O, I, G, HasObjects>
where
    O: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    I: FnMut(
        &BTreeSet<Requirement>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut Vec<GroundPredicate>,
    ) -> Result<(), BuildError>,
    G: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
{
    pub fn init(self, add_init: I) -> ProblemBuilder<O, I, G, HasInit> {
        ProblemBuilder {
            problem_name: self.problem_name,
            domain: self.domain,
            add_objects: self.add_objects,
            add_init: Some(Box::new(add_init)),
            add_goal: self.add_goal,
            state: PhantomData,
        }
    }
}

impl<O, I, G> ProblemBuilder<O, I, G, HasInit>
where
    O: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    I: FnMut(
        &BTreeSet<Requirement>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut Vec<GroundPredicate>,
    ) -> Result<(), BuildError>,
    G: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
{
    pub fn goal(self, add_goal: G) -> ProblemBuilder<O, I, G, HasGoal> {
        ProblemBuilder {
            problem_name: self.problem_name,
            domain: self.domain,
            add_objects: self.add_objects,
            add_init: self.add_init,
            add_goal: Some(Box::new(add_goal)),
            state: PhantomData,
        }
    }
}

impl<O, I, G> ProblemBuilder<O, I, G, HasGoal>
where
    O: FnMut(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &mut dyn ObjectStorage,
    ) -> Result<(), BuildError>,
    I: FnMut(
        &BTreeSet<Requirement>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut Vec<GroundPredicate>,
    ) -> Result<(), BuildError>,
    G: Fn(
        &BTreeSet<Requirement>,
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<QuantifiedFormula<ScopedPredicate>, BuildError>,
{
    pub fn build(self) -> Result<Problem, BuildError> {
        let mut entities = self.domain.entities;
        let mut init = Vec::new();
        let predicate_definitions = self.domain.predicate_definitions;
        let requirements = self.domain.requirements;

        self.add_objects.unwrap()(&requirements, &entities.clone(), &mut entities).and_then(|_| {
            self.add_init.unwrap()(&requirements, &entities, &predicate_definitions, &mut init)
                .and_then(|_| {
                    let init = State::default().with_predicates(init);
                    self.add_goal.unwrap()(
                        &requirements,
                        &entities,
                        &entities,
                        &predicate_definitions,
                    )
                    .map(|goal| Problem {
                        name: self.problem_name,
                        domain_name: self.domain.name,
                        entities,
                        actions: self.domain.actions,
                        init,
                        goal: goal.into(),
                    })
                })
        })
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use gazebo::dupe::Dupe;

    use super::*;
    use crate::{
        action::{ActionBuilder, ActionEffect as E},
        calculus::{
            first_order::QuantifiedFormula as F,
            predicate::{PredicateBuilder, ScopedValue, Value},
        },
    };

    #[test]
    fn test_objects_different_between_problems_for_same_domain() {
        let domain = DomainBuilder::new_domain("Sample Domain")
            .requirements(BTreeSet::from([Requirement::Strips, Requirement::Typing]))
            .types(|_, types| {
                let t1 = types.get_or_create_type("t1");
                let t2 = types.get_or_create_type("t2");
                let _ = types.create_inheritance(&t2, &t1);

                Ok(())
            })
            .consts(|_, types, objects| {
                let _ = objects.get_or_create_object("A", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("B", &types.get_type("t2").unwrap());

                Ok(())
            })
            .predicate_definitions(|_, types, predicates| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                predicates.insert(PredicateBuilder::new("foo").arguments(vec![t1.dupe()]));
                predicates
                    .insert(PredicateBuilder::new("bar").arguments(vec![t1.dupe(), t2.dupe()]));

                Ok(())
            })
            .actions(|_, types, objects, predicates, actions| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                let p1 = predicates.get("foo").unwrap();
                let p2 = predicates.get("bar").unwrap();
                let c1 = objects.get_object("A").unwrap();
                ActionBuilder::new("baz")
                    .parameters(vec![t1.dupe(), t2.dupe()])
                    .precondition(|params| {
                        Ok(F::and(vec![
                            F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                            F::not(F::pred(
                                p2.values(vec![Value::object(&c1), Value::param(&params[1])])
                                    .build()
                                    .unwrap(),
                            )),
                        ]))
                    })
                    .effect(|params| {
                        Ok(E::and(vec![
                            E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                            E::pred(
                                p2.values(vec![Value::object(&c1), Value::param(&params[1])])
                                    .build()
                                    .unwrap(),
                            ),
                        ]))
                    })
                    .build()
                    .map(|a| actions.insert(a))
            })
            .build()
            .unwrap();

        let problem1 = domain
            .new_problem("Sample Problem 1")
            .objects(|_, types, objects| {
                let _ = objects.get_or_create_object("a", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("b", &types.get_type("t2").unwrap());

                Ok(())
            })
            .init(|_, objects, predicates, init| {
                let o1 = objects.get_object("a").unwrap();
                init.push(
                    predicates
                        .get("foo")
                        .unwrap()
                        .values(vec![o1])
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .goal(|_, _types, objects, predicates| {
                let c1 = objects.get_object("A").unwrap();
                let o2 = objects.get_object("b").unwrap();
                let p2 = predicates.get("bar").unwrap();
                Ok(F::pred(
                    p2.values(vec![ScopedValue::object(&c1), ScopedValue::object(&o2)])
                        .build()
                        .unwrap(),
                ))
            })
            .build()
            .unwrap();

        let problem2 = domain
            .new_problem("Sample Problem 2")
            .objects(|_, types, objects| {
                let _ = objects.get_or_create_object("c", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("d", &types.get_type("t2").unwrap());

                Ok(())
            })
            .init(|_, objects, predicates, init| {
                let o3 = objects.get_object("c").unwrap();
                init.push(
                    predicates
                        .get("foo")
                        .unwrap()
                        .values(vec![o3.dupe()])
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .goal(|_, _types, objects, predicates| {
                let c1 = objects.get_object("A").unwrap();
                let o4 = objects.get_object("d").unwrap();
                let p2 = predicates.get("bar").unwrap();
                Ok(F::pred(
                    p2.values(vec![ScopedValue::object(&c1), ScopedValue::object(&o4)])
                        .build()
                        .unwrap(),
                ))
            })
            .build()
            .unwrap();

        assert_ne!(problem1.name, problem2.name);
        assert_eq!(problem1.domain_name, problem2.domain_name);
        assert_ne!(problem1.entities, problem2.entities);
        assert_eq!(problem1.actions, problem2.actions);
        assert_ne!(problem1.init, problem2.init);
        assert_ne!(problem1.goal, problem2.goal);
    }
}
