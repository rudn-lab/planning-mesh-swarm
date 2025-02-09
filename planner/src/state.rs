use crate::{
    evaluation::{EvaluationContext, ResolutionContext},
    predicate::{Predicate, ResolvedPredicate},
};
use alloc::boxed::Box;
use alloc::vec::Vec;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: Vec<Box<dyn ResolvedPredicate>>,
}

impl State {
    pub fn with_predicates(mut self, predicates: &[Box<dyn ResolvedPredicate>]) -> Self {
        self.predicates
            .append(&mut predicates.iter().map(Box::clone).collect());
        self
    }

    pub fn predicates(&self) -> &Vec<Box<dyn ResolvedPredicate>> {
        &self.predicates
    }
}

impl EvaluationContext for State {
    fn eval(&self, predicate: Box<dyn Predicate>) -> bool {
        self.predicates.iter().any(|rp| **rp == *predicate)
    }
}

impl ResolutionContext for State {
    fn resolve(&self, predicate: Box<dyn Predicate>) -> Option<Box<dyn ResolvedPredicate>> {
        self.predicates
            .iter()
            .find(|rp| ***rp == *predicate)
            .cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{evaluation::Evaluable, predicate::*, r#type::*};

    #[test]
    fn test_predicate_eval() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let rp = Pred::with_resolution("bar", &[t, t], [(0, "V"), (1, "B")]);
        let p = Pred::new("bar", &[t, t]);
        let state = State::default().with_predicates(&[Box::new(rp)]);

        assert!(p.eval(&state));

        let p = Pred::new("baz", &[]);

        assert!(!p.eval(&state));
    }

    #[test]
    fn test_preciate_resolution() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let rp: Box<dyn ResolvedPredicate> =
            Box::new(Pred::with_resolution("bar", &[t, t], [(0, "V"), (1, "B")]));
        let p = Pred::new("bar", &[t, t]);
        let state = State::default().with_predicates(&[rp.clone()]);

        assert_eq!(state.resolve(Box::new(p)), Some(rp));

        let p = Pred::new("baz", &[]);

        assert_eq!(state.resolve(Box::new(p)), None);
    }
}
