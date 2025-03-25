//! # Usage:
//!```
//! use planner::{
//!     problem::DomainBuilder,
//!     calculus::{
//!         predicate::{PredicateBuilder, Value},
//!         propositional::{Formula, FormulaMembers as FM, Primitives as Pr, And},
//!     },
//!     action::ActionBuilder,
//! };
//!
//! let domain = DomainBuilder::new_domain("Sample Domain")
//!     .types(|types| {
//!         let t1 = types.get_or_create_type("t1");
//!         let t2 = types.get_or_create_type("t2");
//!         let _ = types.create_inheritance(&t2, &t1);
//!     })
//!     .consts(|types, objects| {
//!         let _ = objects.get_or_create_object("A", &types.get_type("t1").unwrap());
//!         let _ = objects.get_or_create_object("B", &types.get_type("t2").unwrap());
//!     })
//!     .predicate_definitions(|types, predicates| {
//!         let t1 = types.get_type("t1").unwrap();
//!         let t2 = types.get_type("t2").unwrap();
//!         predicates.insert(PredicateBuilder::new("foo").arguments(&[&t1]));
//!         predicates.insert(PredicateBuilder::new("bar").arguments(&[&t1, &t2]));
//!     })
//!     .actions(|types, objects, predicates, actions| {
//!         let t1 = types.get_type("t1").unwrap();
//!         let t2 = types.get_type("t2").unwrap();
//!         let p1 = predicates.get("foo").unwrap();
//!         let p2 = predicates.get("bar").unwrap();
//!         let c1 = objects.get_object("A", &t1).unwrap();
//!         actions.insert(
//!             ActionBuilder::new("baz")
//!                 .parameters([&t1, &t2])
//!                 .precondition(|params| {
//!                     Formula::new(FM::and(&[
//!                         FM::pred(p1.values(&[&Value::param(&params[0])]).build().unwrap()),
//!                         FM::not(&FM::pred(
//!                             p2.values(&[&Value::object(&c1), &Value::param(&params[1])])
//!                                 .build()
//!                                 .unwrap(),
//!                         )),
//!                     ]))
//!                 })
//!                 .effect(|params| {
//!                     And::new(&[
//!                         Pr::not(p1.values(&[&Value::param(&params[0])]).build().unwrap()),
//!                         Pr::pred(
//!                             p2.values(&[&Value::object(&c1), &Value::param(&params[1])])
//!                                 .build()
//!                                 .unwrap(),
//!                         ),
//!                     ])
//!                 })
//!                 .build(),
//!         );
//!     })
//!     .build();
//!
//! let problem = domain
//!     .new_problem("Sample Problem")
//!     .objects(|types, objects| {
//!         let _ = objects.get_or_create_object("a", &types.get_type("t1").unwrap());
//!         let _ = objects.get_or_create_object("b", &types.get_type("t2").unwrap());
//!     })
//!     .init(|types, objects, predicates, init| {
//!         let t1 = types.get_type("t1").unwrap();
//!         let o1 = objects.get_object("a", &t1).unwrap();
//!         init.insert(
//!             predicates
//!                 .get("foo")
//!                 .unwrap()
//!                 .resolved_values(&[&o1])
//!                 .build()
//!                 .unwrap(),
//!         );
//!     })
//!     .goal(|types, objects, predicates| {
//!         let t1 = types.get_type("t1").unwrap();
//!         let t2 = types.get_type("t2").unwrap();
//!         let c1 = objects.get_object("A", &t1).unwrap();
//!         let o2 = objects.get_object("b", &t2).unwrap();
//!         let p2 = predicates.get("bar").unwrap();
//!         Formula::new(FM::pred(p2.resolved_values(&[&c1, &o2]).build().unwrap()))
//!     })
//!     .build();
//!```

use alloc::boxed::Box;
use core::marker::PhantomData;

use crate::{
    action::Action,
    calculus::{
        predicate::{PredicateDefinition, ResolvedPredicate},
        propositional::Formula,
    },
    entity::{EntityStorage, ObjectStorage, TypeStorage},
    sealed::Sealed,
    state::State,
    util::{deep_clone::DeepClone, named::NamedStorage},
    InternerSymbol, INTERNER,
};

#[allow(private_bounds)]
pub trait DomainBuilderState: Sealed {}

pub struct New;
pub struct HasName;
pub struct HasTypes;
pub struct HasConsts;
pub struct HasPredicateDefinitions;
pub struct HasActions;

impl DomainBuilderState for New {}
impl DomainBuilderState for HasName {}
impl DomainBuilderState for HasTypes {}
impl DomainBuilderState for HasConsts {}
impl DomainBuilderState for HasPredicateDefinitions {}
impl DomainBuilderState for HasActions {}

impl Sealed for New {}
impl Sealed for HasName {}
impl Sealed for HasTypes {}
impl Sealed for HasConsts {}
impl Sealed for HasPredicateDefinitions {}
impl Sealed for HasActions {}

pub struct DomainBuilder<T, C, P, A, S>
where
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
    S: DomainBuilderState,
{
    name: InternerSymbol,
    add_types: Option<Box<T>>,
    add_consts: Option<Box<C>>,
    add_predicate_defnitions: Option<Box<P>>,
    add_actions: Option<Box<A>>,
    state: PhantomData<S>,
}

impl<T, C, P, A> DomainBuilder<T, C, P, A, New>
where
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn new_domain(name: &str) -> DomainBuilder<T, C, P, A, HasName> {
        DomainBuilder {
            name: INTERNER.lock().get_or_intern(name),
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
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn types(self, add_types: T) -> DomainBuilder<T, C, P, A, HasTypes> {
        DomainBuilder {
            name: self.name,
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
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn consts(self, add_consts: C) -> DomainBuilder<T, C, P, A, HasConsts> {
        DomainBuilder {
            name: self.name,
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
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn predicate_definitions(
        self,
        add_predicate_definitions: P,
    ) -> DomainBuilder<T, C, P, A, HasPredicateDefinitions> {
        DomainBuilder {
            name: self.name,
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
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn actions(self, add_actions: A) -> DomainBuilder<T, C, P, A, HasActions> {
        DomainBuilder {
            name: self.name,
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
    T: FnMut(&mut dyn TypeStorage),
    C: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    P: Fn(&dyn TypeStorage, &mut NamedStorage<PredicateDefinition>),
    A: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<Action>,
    ),
{
    pub fn build(self) -> Domain {
        let mut entities = EntityStorage::default();
        self.add_types.unwrap()(&mut entities);
        self.add_consts.unwrap()(&entities.clone(), &mut entities);

        let mut predicate_definitions = NamedStorage::default();
        self.add_predicate_defnitions.unwrap()(&entities, &mut predicate_definitions);

        let mut actions = NamedStorage::default();
        self.add_actions.unwrap()(&entities, &entities, &predicate_definitions, &mut actions);

        Domain {
            name: self.name,
            entities,
            predicate_definitions,
            actions,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Domain {
    pub name: InternerSymbol,
    pub entities: EntityStorage,
    pub predicate_definitions: NamedStorage<PredicateDefinition>,
    pub actions: NamedStorage<Action>,
}

impl Domain {
    pub fn new_problem<O, I, G>(&self, name: &str) -> ProblemBuilder<O, I, G, New>
    where
        O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
        I: FnMut(
            &dyn TypeStorage,
            &dyn ObjectStorage,
            &NamedStorage<PredicateDefinition>,
            &mut NamedStorage<ResolvedPredicate>,
        ),
        G: Fn(
            &dyn TypeStorage,
            &dyn ObjectStorage,
            &NamedStorage<PredicateDefinition>,
        ) -> Formula<ResolvedPredicate>,
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
            entities: self.entities.deep_clone(),
            predicate_definitions: self.predicate_definitions.clone(),
            actions: self.actions.clone(),
        }
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
    O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    I: FnMut(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<ResolvedPredicate>,
    ),
    G: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Formula<ResolvedPredicate>,
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
    O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    I: FnMut(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<ResolvedPredicate>,
    ),
    G: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Formula<ResolvedPredicate>,
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
    O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    I: FnMut(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<ResolvedPredicate>,
    ),
    G: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Formula<ResolvedPredicate>,
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
    O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    I: FnMut(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<ResolvedPredicate>,
    ),
    G: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Formula<ResolvedPredicate>,
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
    O: FnMut(&dyn TypeStorage, &mut dyn ObjectStorage),
    I: FnMut(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
        &mut NamedStorage<ResolvedPredicate>,
    ),
    G: Fn(
        &dyn TypeStorage,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Formula<ResolvedPredicate>,
{
    pub fn build(self) -> Problem {
        let mut entities = self.domain.entities;
        self.add_objects.unwrap()(&entities.clone(), &mut entities);

        let mut init = NamedStorage::default();
        let predicate_definitions = self.domain.predicate_definitions;
        self.add_init.unwrap()(&entities, &entities, &predicate_definitions, &mut init);
        let init = State::default().with_predicates(&init.as_vec());
        let goal = self.add_goal.unwrap()(&entities, &entities, &predicate_definitions);

        Problem {
            name: self.problem_name,
            domain_name: self.domain.name,
            entities,
            actions: self.domain.actions,
            init,
            goal,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Problem {
    pub name: InternerSymbol,
    pub domain_name: InternerSymbol,
    pub entities: EntityStorage,
    pub actions: NamedStorage<Action>,
    pub init: State,
    pub goal: Formula<ResolvedPredicate>,
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{
        action::ActionBuilder,
        calculus::{
            predicate::{PredicateBuilder, Value},
            propositional::{And, Formula, FormulaMembers as FM, Primitives as Pr},
        },
    };

    #[test]
    fn test_objects_different_between_problems_for_same_domain() {
        let domain = DomainBuilder::new_domain("Sample Domain")
            .types(|types| {
                let t1 = types.get_or_create_type("t1");
                let t2 = types.get_or_create_type("t2");
                let _ = types.create_inheritance(&t2, &t1);
            })
            .consts(|types, objects| {
                let _ = objects.get_or_create_object("A", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("B", &types.get_type("t2").unwrap());
            })
            .predicate_definitions(|types, predicates| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                predicates.insert(PredicateBuilder::new("foo").arguments(&[&t1]));
                predicates.insert(PredicateBuilder::new("bar").arguments(&[&t1, &t2]));
            })
            .actions(|types, objects, predicates, actions| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                let p1 = predicates.get("foo").unwrap();
                let p2 = predicates.get("bar").unwrap();
                let c1 = objects.get_object("A", &t1).unwrap();
                actions.insert(
                    ActionBuilder::new("baz")
                        .parameters([&t1, &t2])
                        .precondition(|params| {
                            Formula::new(FM::and(&[
                                FM::pred(p1.values(&[&Value::param(&params[0])]).build().unwrap()),
                                FM::not(&FM::pred(
                                    p2.values(&[&Value::object(&c1), &Value::param(&params[1])])
                                        .build()
                                        .unwrap(),
                                )),
                            ]))
                        })
                        .effect(|params| {
                            And::new(&[
                                Pr::not(p1.values(&[&Value::param(&params[0])]).build().unwrap()),
                                Pr::pred(
                                    p2.values(&[&Value::object(&c1), &Value::param(&params[1])])
                                        .build()
                                        .unwrap(),
                                ),
                            ])
                        })
                        .build(),
                );
            })
            .build();

        let problem1 = domain
            .new_problem("Sample Problem 1")
            .objects(|types, objects| {
                let _ = objects.get_or_create_object("a", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("b", &types.get_type("t2").unwrap());
            })
            .init(|types, objects, predicates, init| {
                let t1 = types.get_type("t1").unwrap();
                let o1 = objects.get_object("a", &t1).unwrap();
                init.insert(
                    predicates
                        .get("foo")
                        .unwrap()
                        .resolved_values(&[&o1])
                        .build()
                        .unwrap(),
                );
            })
            .goal(|types, objects, predicates| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                let c1 = objects.get_object("A", &t1).unwrap();
                let o2 = objects.get_object("b", &t2).unwrap();
                let p2 = predicates.get("bar").unwrap();
                Formula::new(FM::pred(p2.resolved_values(&[&c1, &o2]).build().unwrap()))
            })
            .build();

        let problem2 = domain
            .new_problem("Sample Problem 2")
            .objects(|types, objects| {
                let _ = objects.get_or_create_object("c", &types.get_type("t1").unwrap());
                let _ = objects.get_or_create_object("d", &types.get_type("t2").unwrap());
            })
            .init(|types, objects, predicates, init| {
                let t1 = types.get_type("t1").unwrap();
                let o3 = objects.get_object("c", &t1).unwrap();
                init.insert(
                    predicates
                        .get("foo")
                        .unwrap()
                        .resolved_values(&[&o3])
                        .build()
                        .unwrap(),
                );
            })
            .goal(|types, objects, predicates| {
                let t1 = types.get_type("t1").unwrap();
                let t2 = types.get_type("t2").unwrap();
                let c1 = objects.get_object("A", &t1).unwrap();
                let o4 = objects.get_object("d", &t2).unwrap();
                let p2 = predicates.get("bar").unwrap();
                Formula::new(FM::pred(p2.resolved_values(&[&c1, &o4]).build().unwrap()))
            })
            .build();

        assert_ne!(problem1.name, problem2.name);
        assert_eq!(problem1.domain_name, problem2.domain_name);
        assert_ne!(problem1.entities, problem2.entities);
        assert_eq!(problem1.actions, problem2.actions);
        assert_ne!(problem1.init, problem2.init);
        assert_ne!(problem1.goal, problem2.goal);
    }
}
