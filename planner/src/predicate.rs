use crate::{
    evaluation::{Evaluable, EvaluationContext},
    r#type::Type,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{collections::BTreeMap, rc::Rc, string::String, vec, vec::Vec};
use rand::Rng;

#[derive(Clone, PartialOrd, Ord, Eq)]
pub struct Predicate {
    name: InternerSymbol,
    params: BTreeMap<InternerSymbol, Type>,
    // Considering that I carry predicates with `Rc` everywhere,
    // it should probably be possible to enforce this uniqueness using them
    // and not a special struct member.
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

impl Predicate {
    pub fn new(name: &str, params: &[(&str, Type)]) -> Self {
        let params_map = params
            .iter()
            .map(|(n, t)| (INTERNER.lock().get_or_intern(n), *t))
            .collect();
        Self {
            name: INTERNER.lock().get_or_intern(name),
            params: params_map,
            unique_marker: RANDOM.lock().gen(),
        }
    }

    pub fn name(&self) -> String {
        String::from(INTERNER.lock().resolve(self.name).unwrap())
    }

    pub fn params(&self) -> &BTreeMap<InternerSymbol, Type> {
        &self.params
    }

    pub fn unique_marker(&self) -> u32 {
        self.unique_marker
    }
}

impl core::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Predicate {}",
            INTERNER.lock().resolve(self.name).unwrap()
        )
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.params == other.params
            && self.unique_marker == other.unique_marker
    }
}

impl Evaluable for Predicate {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(self)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        vec![Rc::new(self.clone())]
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
        let p = Predicate::new("foo", &[]);
        let p1 = Predicate::new("foo", &[]);

        assert!(p != p1);

        // Different because of marker and name
        let p = Predicate::new("foo", &[]);
        let p1 = Predicate::new("bar", &[]);

        assert!(p != p1);

        // Same, because of the marker and all other params
        let t = Type::new("t");
        let p = Predicate::new("foo", &[("x", t)]);
        let mut p1 = Predicate::new("foo", &[("x", t)]);
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of parameter name
        let t = Type::new("t");
        let p = Predicate::new("foo", &[("x", t)]);
        let mut p1 = Predicate::new("foo", &[("y", t)]);
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);

        // Different because of type
        let t = Type::new("t");
        let t1 = Type::new("t2");
        let p = Predicate::new("foo", &[("x", t)]);
        let mut p1 = Predicate::new("foo", &[("x", t1)]);
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);
    }
}
