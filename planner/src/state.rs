use crate::predicate::Predicate;
use alloc::collections::BTreeSet;
use alloc::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct State {
    predicates: BTreeSet<Rc<Predicate>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            predicates: BTreeSet::new(),
        }
    }

    pub fn with_predicates(predicates: &[Rc<Predicate>]) -> Self {
        Self {
            predicates: predicates.iter().map(|p| Rc::clone(p)).collect(),
        }
    }

    pub fn predicates(&self) -> &BTreeSet<Rc<Predicate>> {
        &self.predicates
    }

    pub fn eval(&self, predicate: &Predicate) -> bool {
        self.predicates.contains(predicate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{predicate::*, r#type::*};
    use core::assert;

    #[test]
    fn test_predicate_eval() {
        let t = Type::new("foo");
        let p = Rc::new(Predicate::new("bar", &[("x", t), ("y", t)]));
        let state = State {
            predicates: BTreeSet::from([Rc::clone(&p)]),
        };

        assert!(p.eval(&state));

        let p = Predicate::new("baz", &[]);

        assert!(!p.eval(&state));
    }
}
