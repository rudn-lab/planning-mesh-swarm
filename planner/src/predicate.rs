use crate::{
    evaluation::{Evaluable, EvaluationContext},
    r#type::Type,
    sealed::Sealed,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{boxed::Box, vec, vec::Vec};
use core::fmt::Debug;
use dyn_clone::DynClone;
use rand::Rng;

pub trait Predicate: DynClone {
    fn name(&self) -> InternerSymbol;
    fn params(&self) -> &[Type];
    fn unique_marker(&self) -> u32;
}

impl Sealed for dyn Predicate {}

dyn_clone::clone_trait_object!(Predicate);

impl Evaluable for Box<dyn Predicate> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(self.clone())
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        vec![self.clone()]
    }
}

impl PartialEq for dyn Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.params() == other.params()
            && self.unique_marker() == other.unique_marker()
    }
}

impl Eq for dyn Predicate {}

impl PartialOrd for dyn Predicate {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for dyn Predicate {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.name().cmp(&other.name())
    }
}

impl Debug for dyn Predicate {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("dyn Predicate")
            .field("name", &self.name())
            .field("params", &self.params())
            .field("unique_marker", &self.unique_marker())
            .finish()
    }
}

impl<const N: usize> Predicate for Pred<N> {
    fn name(&self) -> InternerSymbol {
        self.name
    }

    fn params(&self) -> &[Type] {
        &self.params
    }

    fn unique_marker(&self) -> u32 {
        self.unique_marker
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pred<const N: usize> {
    name: InternerSymbol,
    params: [Type; N],
    /// Used to distinguish predicates with the same name and parameters
    /// when doing normalization. For example:
    /// `p XOR q` can be transformed into the following CNF: `(NOT p OR NOT q) AND (p OR q)`
    /// In this example even if p and q are the same predicate,
    /// they are still independent variables in the expression.
    /// On the other hand, in the CNF above the two `p`s and `q`s are the same predicate
    /// __and__ the same variables, and the expression should not be treated as having 4 inputs,
    /// but only 2.
    unique_marker: u32,
}

impl<const N: usize> Pred<N> {
    pub fn new(name: &str, params: &[Type; N]) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
            params: *params,
            unique_marker: RANDOM.lock().gen(),
        }
    }
}

impl<const N: usize> Evaluable for Pred<N> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(Box::new(*self))
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        vec![Box::new(*self)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::assert;

    #[test]
    fn test_equality() {
        // Different predicates, even if the name and the params
        // are the same, but marker is not
        let p = Pred::new("foo", &[]);
        let p1 = Pred::new("foo", &[]);

        assert!(p != p1);

        // Different because of marker and name
        let p = Pred::new("foo", &[]);
        let p1 = Pred::new("bar", &[]);

        assert!(p != p1);

        // Same, because of the marker and all other params
        let t = Type::new("t");
        let p = Pred::new("foo", &[t]);
        let mut p1 = Pred::new("foo", &[t]);
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t = Type::new("t");
        let t1 = Type::new("t2");
        let p = Pred::new("foo", &[t]);
        let mut p1 = Pred::new("foo", &[t1]);
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);
    }
}
