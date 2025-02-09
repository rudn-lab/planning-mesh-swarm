use core::ops::Deref;

use crate::predicate::{Predicate, ResolvedPredicate};
use alloc::{boxed::Box, vec::Vec};

pub trait Evaluable: Clone {
    fn eval(&self, context: &impl EvaluationContext) -> bool;
    fn predicates(&self) -> Vec<Box<dyn Predicate>>;
}

impl<T, D> Evaluable for D
where
    T: Evaluable,
    D: Deref<Target = T> + Clone,
{
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        (**self).eval(context)
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        (**self).predicates()
    }
}

pub trait EvaluationContext {
    fn eval(&self, predicate: Box<dyn Predicate>) -> bool;
}

pub trait ResolutionContext {
    fn resolve(&self, predicate: Box<dyn Predicate>) -> Option<Box<dyn ResolvedPredicate>>;
}
