use core::ops::Deref;

use crate::{
    action::Action,
    expression::Not,
    object::{ObjectCollection, ObjectHandle},
    predicate::{ActionParameterRef, Predicate, ResolvedPredicate},
    r#type::TypeCollection,
};
use alloc::{
    collections::{BTreeMap, BTreeSet},
    vec::Vec,
};

pub trait Evaluable: Clone {
    fn eval(&self, context: &impl EvaluationContext) -> bool;
    fn predicates(&self) -> Vec<Predicate>;
}

impl<T, D> Evaluable for D
where
    T: Evaluable,
    D: Deref<Target = T> + Clone,
{
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        (**self).eval(context)
    }

    fn predicates(&self) -> Vec<Predicate> {
        (**self).predicates()
    }
}

pub trait EvaluationContext {
    fn eval(&self, predicate: Predicate) -> bool;
}

pub type PredicateResolution = BTreeSet<ResolvedPredicate>;

pub type ActionResolution = BTreeSet<BTreeMap<ActionParameterRef, BTreeSet<ObjectHandle>>>;

pub trait ResolutionContext {
    fn is_resolution_of(
        &self,
        predicate: &Predicate,
        resolved_predicate: &ResolvedPredicate,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> bool;

    fn resolve_predicate(
        &self,
        predicate: &Predicate,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> PredicateResolution;

    fn resolve_negated_predicate(
        &self,
        predicate: &Not<Predicate>,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> PredicateResolution;

    fn resolve_action(
        &self,
        action: Action,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> ActionResolution;
}
