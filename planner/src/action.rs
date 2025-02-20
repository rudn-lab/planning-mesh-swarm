use crate::{
    expression::{And, Dnf, Primitives},
    predicate::{ActionParameterRef, ParameterHandle},
    r#type::TypeHandle,
    sealed::Sealed,
    InternerSymbol, INTERNER,
};
use alloc::vec::Vec;
use core::{fmt::Debug, marker::PhantomData};
use getset::Getters;

#[derive(Debug, Clone, Getters)]
pub struct Action {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    parameters: Vec<ActionParameterRef>,
    #[getset(get = "pub")]
    precondition: Dnf,
    #[getset(get = "pub")]
    effect: And<Primitives>,
}

#[allow(private_bounds)]
pub trait ActionBuilderState: Sealed {}

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

impl Sealed for New {}
impl Sealed for HasName {}
impl Sealed for HasParameters {}
impl Sealed for HasPrecondition {}
impl Sealed for HasEffect {}

pub struct ActionBuilder<S: ActionBuilderState, const N: usize, D: Into<Dnf>> {
    name: InternerSymbol,
    parameters: Option<[ActionParameterRef; N]>,
    precondition: Option<D>,
    effect: Option<And<Primitives>>,
    state: PhantomData<S>,
}

impl<const N: usize, D: Into<Dnf>> ActionBuilder<New, N, D> {
    pub fn new(name: &str) -> ActionBuilder<HasName, N, D> {
        ActionBuilder {
            name: INTERNER.lock().get_or_intern(name),
            parameters: None,
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<const N: usize, D: Into<Dnf>> ActionBuilder<HasName, N, D> {
    pub fn parameters(self, parameters: &[TypeHandle; N]) -> ActionBuilder<HasParameters, N, D> {
        let parameters = parameters
            .iter()
            .enumerate()
            .map(|(i, t)| ActionParameterRef {
                parameter_handle: ParameterHandle { idx: i },
                r#type: t.clone(),
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        ActionBuilder {
            name: self.name,
            parameters: Some(parameters),
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<const N: usize, D: Into<Dnf>> ActionBuilder<HasParameters, N, D> {
    pub fn precondition<F>(self, precondition: F) -> ActionBuilder<HasPrecondition, N, D>
    where
        F: Fn(&[ActionParameterRef; N]) -> D,
    {
        ActionBuilder {
            name: self.name,
            precondition: Some(precondition(self.parameters.as_ref().unwrap())),
            parameters: self.parameters,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<const N: usize, D: Into<Dnf>> ActionBuilder<HasPrecondition, N, D> {
    pub fn effect<F>(self, effect: F) -> ActionBuilder<HasEffect, N, D>
    where
        F: Fn(&[ActionParameterRef; N]) -> And<Primitives>,
    {
        ActionBuilder {
            name: self.name,
            effect: Some(effect(self.parameters.as_ref().unwrap())),
            parameters: self.parameters,
            precondition: self.precondition,
            state: PhantomData,
        }
    }
}

impl<const N: usize, D: Into<Dnf>> ActionBuilder<HasEffect, N, D> {
    pub fn build(self) -> Action {
        Action {
            name: self.name,
            parameters: self.parameters.unwrap().to_vec(),
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

        let p = PredicateDeclaration::new("foo", &[&t]);
        let p1 = PredicateDeclaration::new("bar", &[&t1]);

        let _action = ActionBuilder::new("flip")
            .parameters(&[t, t1])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p.as_specific(&[Value::ActionParam(params[0].clone())])),
                    FM::not(&FM::pred(
                        p1.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();
    }
}
