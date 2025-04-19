pub mod first_order;
pub mod predicate;
pub mod propositional;
pub mod truth_table;

use alloc::collections::BTreeSet;
use core::fmt::Debug;

use crate::{calculus::predicate::IsPredicate, state::PredicateKey};

pub trait Evaluable<P: IsPredicate>: Clone + Debug {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool;
}

pub trait EvaluationContext<P: IsPredicate> {
    fn matching_predicates(&self, key: &PredicateKey) -> Option<&BTreeSet<P>>;
}
