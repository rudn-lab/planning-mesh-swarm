use crate::{
    calculus::{
        predicate::Predicate,
        propositional::{And, Dnf, Expression, Primitives},
    },
    entity::TypeHandle,
    problem::BuildError,
    sealed::Sealed,
    state::{ModifyState, ParameterResolution},
    util::named::Named,
    InternerSymbol, INTERNER,
};
use alloc::{boxed::Box, collections::BTreeSet, vec::Vec};
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
    precondition: Dnf<Predicate>,
    #[getset(get = "pub")]
    effect: And<Primitives<Predicate>, Predicate>,
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
                                ModifyState::Del(not.inner().into_resolved(&s).unwrap())
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
{
    pub fn parameters(self, parameters: &[TypeHandle]) -> ActionBuilder<P, E, D, HasParameters> {
        let parameters = parameters
            .iter()
            .enumerate()
            .map(|(i, t)| ActionParameter {
                parameter_idx: i,
                r#type: t.dupe(),
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
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
    E: Fn(&[ActionParameter]) -> Result<And<Primitives<Predicate>, Predicate>, BuildError>,
    D: Into<Dnf<Predicate>>,
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
            propositional::{Formula, FormulaMembers as FM, Primitives as Pr},
        },
        entity::{EntityStorage, TypeStorage},
    };

    use super::*;

    #[test]
    fn test_build_action() {
        let mut types = EntityStorage::default();
        let t = types.get_or_create_type("foo");
        let t1 = types.get_or_create_type("bar");

        let p = PredicateBuilder::new("foo").arguments(&[t.clone()]);
        let p1 = PredicateBuilder::new("bar").arguments(&[t1.clone()]);

        ActionBuilder::new("flip")
            .parameters(&[t, t1])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::pred(p.values(&[Value::param(&params[0])]).build().unwrap()),
                    FM::not(FM::pred(
                        p1.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(p.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p1.values(&[Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();
    }
}
