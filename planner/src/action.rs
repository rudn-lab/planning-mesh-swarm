use core::marker::PhantomData;

use crate::{
    expression::{And, Dnf, Primitives},
    predicate::{ActionParameterRef, ParameterHandle},
    r#type::TypeHandle,
    sealed::Sealed,
    InternerSymbol, INTERNER,
};
use alloc::vec::Vec;
use core::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Action {
    pub name: InternerSymbol,
    pub parameters: Vec<ActionParameterRef>,
    pub precondition: Dnf,
    pub effect: And<Primitives>,
}

pub trait ActionBuilderState: Sealed {}
impl<D: ActionBuilderState> Sealed for D {}

pub struct New;
pub struct HasName;
pub struct HasParameters;
pub struct HasPrecondition;
pub struct HasEffect;

impl ActionBuilderState for New {}
impl ActionBuilderState for HasName {}
impl ActionBuilderState for HasParameters {}
impl ActionBuilderState for HasPrecondition {}
impl ActionBuilderState for HasEffect {}

pub struct ActionBuilder<S: ActionBuilderState, D: Into<Dnf>> {
    pub name: InternerSymbol,
    pub parameters: Vec<ActionParameterRef>,
    pub precondition: Option<D>,
    pub effect: Option<And<Primitives>>,
    pub state: PhantomData<S>,
}

impl<D: Into<Dnf>> ActionBuilder<New, D> {
    pub fn new(name: &str) -> ActionBuilder<HasName, D> {
        ActionBuilder {
            name: INTERNER.lock().get_or_intern(name),
            parameters: Vec::new(),
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<D: Into<Dnf>> ActionBuilder<HasName, D> {
    pub fn parameters(self, parameters: &[TypeHandle]) -> ActionBuilder<HasParameters, D> {
        let parameters = parameters
            .iter()
            .enumerate()
            .map(|(i, t)| ActionParameterRef {
                parameter_handle: ParameterHandle { idx: i },
                r#type: *t,
            })
            .collect::<Vec<_>>();
        ActionBuilder {
            name: self.name,
            parameters,
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<D: Into<Dnf>> ActionBuilder<HasParameters, D> {
    pub fn precondition<F>(self, precondition: F) -> ActionBuilder<HasPrecondition, D>
    where
        F: Fn(&[ActionParameterRef]) -> D,
    {
        ActionBuilder {
            name: self.name,
            precondition: Some(precondition(&self.parameters)),
            parameters: self.parameters,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<D: Into<Dnf>> ActionBuilder<HasPrecondition, D> {
    pub fn effect<F>(self, effect: F) -> ActionBuilder<HasEffect, D>
    where
        F: Fn(&[ActionParameterRef]) -> And<Primitives>,
    {
        ActionBuilder {
            name: self.name,
            effect: Some(effect(&self.parameters)),
            parameters: self.parameters,
            precondition: self.precondition,
            state: PhantomData,
        }
    }
}

impl<D: Into<Dnf>> ActionBuilder<HasEffect, D> {
    pub fn build(self) -> Action {
        Action {
            name: self.name,
            parameters: self.parameters,
            precondition: self.precondition.unwrap().into(),
            effect: self.effect.unwrap(),
        }
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{
        expression::{Formula, FormulaMembers as FM, Primitives as Pr},
        predicate::{PredicateDeclaration, Value},
        r#type::TypeCollection,
    };

    use super::*;

    #[test]
    pub fn test_build_action() {
        let mut types = TypeCollection::default();
        let t = types.get_or_create("foo");
        let t1 = types.get_or_create("bar");

        let p = PredicateDeclaration::new("foo", &[t]);
        let p1 = PredicateDeclaration::new("bar", &[t1]);

        let _action = ActionBuilder::new("flip")
            .parameters(&[t, t1])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p.as_specific(&[Value::ActionParam(params[0])])),
                    FM::not(&FM::pred(p1.as_specific(&[Value::ActionParam(params[1])]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();
    }
}
