use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::ops::Deref;

use crate::calculus::predicate::Predicate;

pub trait Evaluable: Clone {
    fn eval(&self, context: &impl EvaluationContext) -> bool;
    fn predicates(&self) -> Vec<&Predicate>;
}

/// Skill issue implementing it like this
/// ```ignore
/// impl<T, D> Evaluable for D
/// where
///     T: Evaluable,
///     D: Deref<Target = T> + Clone,
/// {
///     fn eval(&self, context: &impl EvaluationContext) -> bool {
///         self.deref().eval(context)
///     }
///
///     fn predicates(&self) -> Vec<&Predicate> {
///         self.deref().predicates()
///     }
/// }
/// ```
macro_rules! impl_evaluable_for_ref {
    ($type:ty) => {
        impl<T> Evaluable for $type
        where
            T: Evaluable,
        {
            fn eval(&self, context: &impl EvaluationContext) -> bool {
                self.deref().eval(context)
            }

            fn predicates(&self) -> Vec<&Predicate> {
                self.deref().predicates()
            }
        }
    };
}

impl_evaluable_for_ref!(Rc<T>);
impl_evaluable_for_ref!(Box<T>);
impl_evaluable_for_ref!(&T);

pub trait EvaluationContext {
    fn eval(&self, predicate: &Predicate) -> bool;
}
