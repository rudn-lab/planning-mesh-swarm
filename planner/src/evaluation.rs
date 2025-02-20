use core::ops::Deref;

use crate::{
    action::Action,
    expression::Not,
    object::{ObjectCollection, ObjectHandle},
    predicate::{ActionParameterRef, Predicate, ResolvedPredicate},
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

#[allow(
    clippy::mutable_key_type,
    reason = "See SmartHandle Hash and Ord impls."
)]
pub trait ResolutionContext {
    fn is_resolution_of(
        &self,
        predicate: &Predicate,
        resolved_predicate: &ResolvedPredicate,
        objects: &ObjectCollection,
    ) -> bool;

    fn resolve_predicate(
        &self,
        predicate: &Predicate,
        objects: &ObjectCollection,
    ) -> PredicateResolution;

    fn resolve_negated_predicate(
        &self,
        predicate: &Not<Predicate>,
        objects: &ObjectCollection,
    ) -> PredicateResolution;

    fn resolve_action(&self, action: Action, objects: &ObjectCollection) -> ActionResolution;
}
