use crate::predicate::Predicate;
use alloc::{boxed::Box, rc::Rc, vec::Vec};

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

impl<T: Evaluable> Evaluable for Rc<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        (**self).eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        (**self).predicates()
    }
}

impl<T: Evaluable> Evaluable for &T {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        (**self).eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        (**self).predicates()
    }
}

pub trait EvaluationContext {
    fn eval(&self, predicate: &Predicate) -> bool;
}
