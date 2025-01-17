use crate::{evaluation::EvaluationContext, predicate::Predicate};
use alloc::collections::BTreeSet;
use alloc::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: BTreeSet<Rc<Predicate>>,
}

impl State {
    pub fn with_predicates(predicates: &[Rc<Predicate>]) -> Self {
        Self {
            predicates: predicates.iter().map(Rc::clone).collect(),
        }
    }

    pub fn predicates(&self) -> &BTreeSet<Rc<Predicate>> {
        &self.predicates
    }
}

impl EvaluationContext for State {
    fn eval(&self, predicate: &Predicate) -> bool {
        self.predicates.contains(predicate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{evaluation::Evaluable, predicate::*, r#type::*};

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
