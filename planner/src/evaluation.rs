use core::ops::Deref;

use crate::predicate::Predicate;
use alloc::vec::Vec;

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
