use crate::{evaluation::EvaluationContext, predicate::Predicate};
use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: Vec<Box<dyn Predicate>>,
}

impl State {
    pub fn with_predicates(mut self, predicates: &[Box<dyn Predicate>]) -> Self {
        self.predicates
            .append(&mut predicates.iter().map(Box::clone).collect());
        self
    }

    pub fn predicates(&self) -> &Vec<Box<dyn Predicate>> {
        &self.predicates
    }
}

impl EvaluationContext for State {
    fn eval(&self, predicate: Box<dyn Predicate>) -> bool {
        self.predicates.contains(&predicate)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{evaluation::Evaluable, predicate::*, r#type::*};

    #[test]
    fn test_predicate_eval() {
        let t = Type::new("foo");
        let p = Pred::new("bar", &[t, t]);
        let state = State::default().with_predicates(&[Box::new(p)]);

        assert!(p.eval(&state));

        let p = Pred::new("baz", &[]);

        assert!(!p.eval(&state));
    }
}
