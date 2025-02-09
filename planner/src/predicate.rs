use crate::{
    evaluation::{Evaluable, EvaluationContext},
    r#type::TypeHandle,
    sealed::Sealed,
    util::const_table::ConstTable,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{boxed::Box, vec, vec::Vec};
use core::fmt::Debug;
use dyn_clone::DynClone;
use rand::Rng;

pub trait Predicate: DynClone {
    fn name(&self) -> InternerSymbol;
    fn params(&self) -> &[TypeHandle];
    fn resolutions(&self) -> Vec<(usize, InternerSymbol)>;
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

#[coverage(off)]
impl Debug for dyn Predicate {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("dyn Predicate")
            .field("name", &self.name())
            .field("params", &self.params())
            .field("resolutions", &self.resolutions())
            .field("unique_marker", &self.unique_marker())
            .finish()
    }
}

pub trait ResolvedPredicate: Predicate {}

dyn_clone::clone_trait_object!(ResolvedPredicate);

impl PartialEq for dyn ResolvedPredicate {
    fn eq(&self, other: &Self) -> bool {
        let a: &dyn Predicate = self;
        let b: &dyn Predicate = other;
        a.eq(b)
    }
}

impl Eq for dyn ResolvedPredicate {}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for dyn ResolvedPredicate {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        let a: &dyn Predicate = self;
        let b: &dyn Predicate = other;
        a.partial_cmp(b)
    }
}

impl Ord for dyn ResolvedPredicate {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        let a: &dyn Predicate = self;
        let b: &dyn Predicate = other;
        a.cmp(b)
    }
}

impl Debug for dyn ResolvedPredicate {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let a: &dyn Predicate = self;
        write!(f, "{:?}", a)
    }
}

macro_rules! partial_eq_predicate_traits {
    ($p1:ident, $p2:ident) => {
        impl PartialEq<dyn $p1> for dyn $p2 {
            fn eq(&self, other: &dyn $p1) -> bool {
                self.name() == other.name() && self.params() == other.params()
            }
        }
    };
}

partial_eq_predicate_traits!(Predicate, ResolvedPredicate);
partial_eq_predicate_traits!(ResolvedPredicate, Predicate);

impl<const N: usize> ResolvedPredicate for Pred<N, N> {}

impl<const N: usize, const M: usize> Predicate for Pred<N, M> {
    fn name(&self) -> InternerSymbol {
        self.name
    }

    fn params(&self) -> &[TypeHandle] {
        &self.params
    }

    fn unique_marker(&self) -> u32 {
        self.unique_marker
    }

    fn resolutions(&self) -> Vec<(usize, InternerSymbol)> {
        self.resolution_table.key_values()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pred<const N: usize, const M: usize> {
    name: InternerSymbol,
    params: [TypeHandle; N],
    resolution_table: ConstTable<usize, InternerSymbol, M>,
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

impl<const N: usize> Pred<N, 0> {
    pub fn new(name: &str, params: &[TypeHandle; N]) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
            params: *params,
            resolution_table: ConstTable::new([]),
            unique_marker: RANDOM.lock().gen(),
        }
    }
}

impl<const N: usize, const M: usize> Pred<N, M> {
    pub fn with_resolution(
        name: &str,
        params: &[TypeHandle; N],
        resolution: [(usize, &str); M],
    ) -> Self {
        assert!(
            M <= N,
            "Resolution map size should not exceed the number of parameters in a predicate."
        );
        let mut interner = INTERNER.lock();
        Self {
            name: interner.get_or_intern(name),
            params: *params,
            resolution_table: ConstTable::new(
                resolution.map(|(k, v)| (k, interner.get_or_intern(v))),
            ),
            unique_marker: RANDOM.lock().gen(),
        }
    }

    pub fn as_resolved<const L: usize>(self, resolution: [(usize, &str); L]) -> Pred<N, N> {
        // Asserting this because if L repeats some keys in M,
        // we should still get N at the end
        assert!(L + M >= N, "Provided resolution array is too small.");
        let mut interner = INTERNER.lock();
        let resolution_table = self
            .resolution_table
            .append(resolution.map(|(k, v)| (k, interner.get_or_intern(v))));

        Pred {
            name: self.name,
            params: self.params,
            resolution_table,
            unique_marker: self.unique_marker,
        }
    }
}

impl<const N: usize, const M: usize> Evaluable for Pred<N, M> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(Box::new(*self))
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        vec![Box::new(*self)]
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::r#type::TypeCollection;

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
        let mut types = TypeCollection::default();
        let t = types.create("t");
        let p = Pred::new("foo", &[t]);
        let mut p1 = Pred::new("foo", &[t]);
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t1 = types.create("t1");
        let p = Pred::new("foo", &[t]);
        let mut p1 = Pred::new("foo", &[t1]);
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);
    }
}
