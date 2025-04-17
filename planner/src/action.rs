use crate::{
    calculus::{
        first_order::{FOExpression, ForAll, Pdnf},
        predicate::{Predicate, PredicateError},
        propositional::Primitives,
    },
    entity::{ObjectHandle, TypeHandle},
    problem::BuildError,
    sealed::Sealed,
    state::{ModifyState, ParameterResolution},
    util::named::Named,
    InternerSymbol, INTERNER,
};
use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
    vec::Vec,
};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::Getters;
use itertools::Itertools;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub struct ActionParameter {
    pub(crate) parameter_idx: usize,
    pub(crate) r#type: TypeHandle,
}

#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Action {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    parameters: Vec<ActionParameter>,
    #[getset(get = "pub")]
    precondition: Pdnf<Predicate>,
    #[getset(get = "pub")]
    effect: ActionEffect,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionEffect {
    And(Vec<ActionEffect>),
    ForAll(ForAll<ActionEffect, Predicate>),
    Primitives(Primitives<Predicate>),
}

impl ActionEffect {
    pub fn and(operands: Vec<ActionEffect>) -> Self {
        Self::And(operands)
    }

    pub fn forall(operand: ForAll<ActionEffect, Predicate>) -> Self {
        Self::ForAll(operand)
    }

    pub fn pred(operand: Predicate) -> Self {
        Self::Primitives(Primitives::Pred(operand))
    }

    pub fn npred(operand: Predicate) -> Self {
        Self::Primitives(Primitives::not(operand))
    }
}

impl FOExpression for ActionEffect {}

impl Action {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolved_effects<'a>(
        &'a self,
        parameter_resolutions: &'a BTreeSet<ParameterResolution<'a>>,
    ) -> Result<BTreeSet<BTreeSet<ModifyState>>, PredicateError> {
        fn ground_effect_flat(
            effect: &ActionEffect,
            resolution: &BTreeMap<&ActionParameter, ObjectHandle>,
        ) -> Result<BTreeSet<ModifyState>, PredicateError> {
            match effect {
                ActionEffect::And(effects) => effects
                    .iter()
                    .map(|e| ground_effect_flat(e, resolution))
                    .try_fold(BTreeSet::new(), |mut acc, el| {
                        el.map(|e| {
                            acc.extend(e);
                            acc
                        })
                    }),
                ActionEffect::ForAll(forall) => ground_effect_flat(forall.expression(), resolution),
                ActionEffect::Primitives(p) => match p {
                    Primitives::Not(not) => not
                        .inner()
                        .into_resolved(resolution)
                        .map(|r| r.into_iter().map(ModifyState::Del).collect::<BTreeSet<_>>()),
                    Primitives::Pred(pred) => pred
                        .into_resolved(resolution)
                        .map(|r| r.into_iter().map(ModifyState::Add).collect::<BTreeSet<_>>()),
                },
            }
        }

        parameter_resolutions
            .iter()
            .flat_map(|resolution| {
                resolution
                    .as_simple()
                    .map(|resolution| ground_effect_flat(&self.effect, &resolution))
            })
            .collect::<Result<BTreeSet<_>, _>>()
    }
}

impl Named for Action {
    fn name(&self) -> InternerSymbol {
        self.name
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

pub struct ActionBuilder<P, E, D, S>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
    S: ActionBuilderState,
{
    name: InternerSymbol,
    parameters: Option<Vec<ActionParameter>>,
    precondition: Option<Box<P>>,
    effect: Option<Box<E>>,
    state: PhantomData<S>,
}

impl<P, E, D> ActionBuilder<P, E, D, New>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
{
    pub fn new(name: &str) -> ActionBuilder<P, E, D, HasName> {
        ActionBuilder {
            name: INTERNER.lock().get_or_intern(name),
            parameters: None,
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<P, E, D> ActionBuilder<P, E, D, HasName>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
{
    pub fn parameters(self, parameters: Vec<TypeHandle>) -> ActionBuilder<P, E, D, HasParameters> {
        let parameters = parameters
            .into_iter()
            .enumerate()
            .map(|(i, t)| ActionParameter {
                parameter_idx: i,
                r#type: t,
            })
            .collect_vec();

        ActionBuilder {
            name: self.name,
            parameters: Some(parameters),
            precondition: None,
            effect: None,
            state: PhantomData,
        }
    }
}

impl<P, E, D> ActionBuilder<P, E, D, HasParameters>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
{
    pub fn precondition(self, precondition: P) -> ActionBuilder<P, E, D, HasPrecondition> {
        ActionBuilder {
            name: self.name,
            parameters: self.parameters,
            precondition: Some(Box::new(precondition)),
            effect: None,
            state: PhantomData,
        }
    }
}

impl<P, E, D> ActionBuilder<P, E, D, HasPrecondition>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
{
    pub fn effect(self, effect: E) -> ActionBuilder<P, E, D, HasEffect> {
        ActionBuilder {
            name: self.name,
            parameters: self.parameters,
            precondition: self.precondition,
            effect: Some(Box::new(effect)),
            state: PhantomData,
        }
    }
}

impl<P, E, D> ActionBuilder<P, E, D, HasEffect>
where
    P: Fn(&[ActionParameter]) -> Result<D, BuildError>,
    E: Fn(&[ActionParameter]) -> Result<ActionEffect, BuildError>,
    D: Into<Pdnf<Predicate>>,
{
    pub fn build(self) -> Result<Action, BuildError> {
        let params = self.parameters.unwrap();
        self.precondition.unwrap()(&params).and_then(|precondition| {
            self.effect.unwrap()(&params).map(|effect| Action {
                name: self.name,
                parameters: params,
                precondition: precondition.into(),
                effect,
            })
        })
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{
        calculus::{
            predicate::{PredicateBuilder, Value},
            propositional::{Formula as F, Primitives as Pr},
        },
        entity::{EntityStorage, TypeStorage},
    };

    use super::*;

    #[test]
    fn test_build_action() {
        let mut types = EntityStorage::default();
        let t = types.get_or_create_type("foo");
        let t1 = types.get_or_create_type("bar");

        let p = PredicateBuilder::new("foo").arguments(vec![t.clone()]);
        let p1 = PredicateBuilder::new("bar").arguments(vec![t1.clone()]);

        ActionBuilder::new("flip")
            .parameters(vec![t, t1])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::not(F::pred(
                        p1.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(ActionEffect::And(vec![
                    ActionEffect::Primitives(Pr::not(
                        p.values(vec![Value::param(&params[0])]).build().unwrap(),
                    )),
                    ActionEffect::Primitives(Pr::pred(
                        p1.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .build()
            .unwrap();
    }
}
