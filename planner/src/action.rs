use crate::{
    calculus::{
        evaluation::Evaluable,
        propositional::{And, Dnf, Expression, Primitives},
    },
    entity::TypeHandle,
    sealed::Sealed,
    state::{ModifyState, ParameterResolution},
    InternerSymbol, INTERNER,
};
use alloc::{collections::BTreeSet, vec::Vec};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::Getters;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub struct ActionParameter {
    pub(crate) parameter_handle: usize,
    pub(crate) r#type: TypeHandle,
}

#[derive(Debug, Clone, Getters)]
pub struct Action {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    parameters: Vec<ActionParameter>,
    #[getset(get = "pub")]
    precondition: Dnf,
    #[getset(get = "pub")]
    effect: And<Primitives>,
}

impl Action {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolved_effects<'a>(
        &'a self,
        parameter_resolutions: &'a BTreeSet<ParameterResolution<'a>>,
    ) -> BTreeSet<BTreeSet<ModifyState>> {
        // For each sub expression in the original DNF
        parameter_resolutions
            .iter()
            .flat_map(|r| {
                r.as_simple().map(|s| {
                    self.effect
                        .members()
                        .iter()
                        .map(|v| match v {
                            // TODO: handle the errors properly
                            Primitives::Not(not) => {
                                ModifyState::Del(not.predicates()[0].into_resolved(&s).unwrap())
                            }
                            Primitives::Pred(predicate) => {
                                ModifyState::Add(predicate.into_resolved(&s).unwrap())
                            }
                        })
                        .collect::<BTreeSet<ModifyState>>()
                })
            })
            .collect::<BTreeSet<_>>()
    }
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
    parameters: Option<[ActionParameter; N]>,
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
    pub fn parameters(self, parameters: [&TypeHandle; N]) -> ActionBuilder<HasParameters, N, D> {
        let parameters = parameters
            .iter()
            .enumerate()
            .map(|(i, &t)| ActionParameter {
                parameter_handle: i,
                r#type: t.dupe(),
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
        F: Fn(&[ActionParameter; N]) -> D,
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
        F: Fn(&[ActionParameter; N]) -> And<Primitives>,
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
        calculus::{
            predicate::{PredicateBuilder, Value},
            propositional::{Formula, FormulaMembers as FM, Primitives as Pr},
        },
        entity::EntityStorage,
    };

    use super::*;

    #[test]
    fn test_build_action() {
        let mut types = EntityStorage::default();
        let t = types.get_or_create_type("foo");
        let t1 = types.get_or_create_type("bar");

        let p = PredicateBuilder::new("foo").arguments([&t]);
        let p1 = PredicateBuilder::new("bar").arguments([&t1]);

        let _action = ActionBuilder::new("flip")
            .parameters([&t, &t1])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p.values([Value::param(&params[0])]).build().unwrap()),
                    FM::not(&FM::pred(
                        p1.values([Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p.values([Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p1.values([Value::param(&params[1])]).build().unwrap()),
                ])
            })
            .build();
    }
}
