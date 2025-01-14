use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use crate::r#type::Type;
use crate::InternerSymbol;
use crate::INTERNER;

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
        }
    }

    pub fn name(&self) -> String {
        String::from(INTERNER.lock().resolve(self.name).unwrap())
    }

    pub fn params(&self) -> &BTreeMap<InternerSymbol, Type> {
        &self.params
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
