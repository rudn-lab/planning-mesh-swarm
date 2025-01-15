use crate::{r#type::Type, InternerSymbol, INTERNER, RANDOM};
use alloc::{boxed::Box, collections::BTreeMap, rc::Rc, string::String, vec, vec::Vec};
use rand::Rng;

pub trait EvaluationContext {
    fn eval(&self, predicate: &Predicate) -> bool;
}

pub trait Evaluable: Clone {
    fn eval(&self, context: &impl EvaluationContext) -> bool;
    fn predicates(&self) -> Vec<Rc<Predicate>>;
}

impl<T: Evaluable> Evaluable for Box<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        (**self).eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        (**self).predicates()
    }
}

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
        // TODO: We have Predicate.unique_marker now.
        // Figure out if this can be replaced by marker comparision,
        // once you do resolved predicates in state.
        self.name == other.name && self.params == other.params
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

impl Evaluable for Rc<Predicate> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(self)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        (**self).predicates()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::assert;

    #[test]
    fn test_equality() {
        let p = Predicate::new("foo", &[]);
        let p2 = Predicate::new("foo", &[]);

        assert!(p == p2);

        let p = Predicate::new("foo", &[]);
        let p2 = Predicate::new("bar", &[]);

        assert!(p != p2);

        let t = Type::new("t");
        let p = Predicate::new("foo", &[("x", t)]);
        let p2 = Predicate::new("foo", &[("x", t)]);

        assert!(p == p2);

        let t = Type::new("t");
        let p = Predicate::new("foo", &[("x", t)]);
        let p2 = Predicate::new("foo", &[("y", t)]);

        assert!(p != p2);

        let t = Type::new("t");
        let t2 = Type::new("t2");
        let p = Predicate::new("foo", &[("x", t)]);
        let p2 = Predicate::new("foo", &[("x", t2)]);

        assert!(p != p2);
    }
}
