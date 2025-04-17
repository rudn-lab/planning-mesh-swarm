use alloc::{boxed::Box, rc::Rc};
use core::{fmt::Debug, ops::Deref};

use crate::calculus::predicate::IsPredicate;

pub trait Evaluable<P: IsPredicate<P>, RP: IsPredicate<RP>>: Clone + Debug {
    fn eval(&self, context: &impl EvaluationContext<RP>) -> bool;
    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a>;
}

/// Skill issue implementing it like this
/// ```ignore
/// impl<T, P, D> Evaluable<P> for D
/// where
///     T: Evaluable,
///     P: IsPredicate<P>,
///     D: Deref<Target = T> + Clone,
/// {
///     fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
///         self.deref().eval(context)
///     }
///
///     fn predicates(&self) -> Box<dyn Iterator<Item = P>> {
///         self.deref().predicates()
///     }
/// }
/// ```
macro_rules! impl_evaluable_for_ref {
    ($type:ty) => {
        impl<T, P> Evaluable<P, P> for $type
        where
            T: Evaluable<P, P>,
            P: IsPredicate<P>,
        {
            fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
                self.deref().eval(context)
            }

            fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
                self.deref().predicates()
            }
        }
    };
}

impl_evaluable_for_ref!(Rc<T>);
impl_evaluable_for_ref!(Box<T>);
impl_evaluable_for_ref!(&T);

pub trait EvaluationContext<P: IsPredicate<P>> {
    fn eval(&self, predicate: &P) -> bool;
}
