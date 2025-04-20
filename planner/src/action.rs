use crate::{
    calculus::{
        first_order::{FOExpression, ForAll, Pdnf},
        predicate::{GroundPredicate, LiftedPredicate, PredicateError},
        propositional::Primitives,
        Evaluable, EvaluationContext,
    },
    entity::{ObjectHandle, TypeHandle},
    problem::BuildError,
    sealed::Sealed,
    state::{ModifyState, ParameterGrounding},
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

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    precondition: Pdnf<LiftedPredicate>,
    #[getset(get = "pub")]
    effect: ActionEffect,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionEffect {
    And(Vec<ActionEffect>),
    ForAll(ForAll<ActionEffect, LiftedPredicate>),
    When {
        condition: Pdnf<LiftedPredicate>,
        effect: Box<ActionEffect>,
    },
    Primitives(Primitives<LiftedPredicate>),
}

impl ActionEffect {
    pub fn and(operands: Vec<ActionEffect>) -> Self {
        Self::And(operands)
    }

    pub fn forall(operand: ForAll<ActionEffect, LiftedPredicate>) -> Self {
        Self::ForAll(operand)
    }

    pub fn when<C: Into<Pdnf<LiftedPredicate>>>(condition: C, effect: ActionEffect) -> Self {
        Self::When {
            condition: condition.into(),
            effect: Box::new(effect),
        }
    }

    pub fn pred(operand: LiftedPredicate) -> Self {
        Self::Primitives(Primitives::Pred(operand))
    }

    pub fn npred(operand: LiftedPredicate) -> Self {
        Self::Primitives(Primitives::Not(operand))
    }

    /// Returns all action parameters that are used in this effect.
    pub fn action_parameters(&self) -> BTreeSet<&ActionParameter> {
        match self {
            ActionEffect::And(and) => and.iter().flat_map(|e| e.action_parameters()).collect(),
            ActionEffect::ForAll(forall) => forall.expression().action_parameters(),
            ActionEffect::When {
                condition: _,
                effect,
            } => effect.action_parameters(),
            ActionEffect::Primitives(p) => match p {
                Primitives::Pred(p) | Primitives::Not(p) => p.action_parameters(),
            },
        }
    }
}

impl FOExpression for ActionEffect {}

impl Action {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn ground_effect<'a>(
        &'a self,
        parameter_groundings: &'a BTreeSet<ParameterGrounding<'a>>,
        state: &impl EvaluationContext<GroundPredicate>,
    ) -> Result<BTreeSet<BTreeSet<ModifyState>>, PredicateError> {
        fn ground_effect_flat(
            effect: &ActionEffect,
            grounding: &BTreeMap<&ActionParameter, ObjectHandle>,
            state: &impl EvaluationContext<GroundPredicate>,
        ) -> Result<BTreeSet<ModifyState>, PredicateError> {
            match effect {
                ActionEffect::And(effects) => effects
                    .iter()
                    .map(|e| ground_effect_flat(e, grounding, state))
                    .try_fold(BTreeSet::new(), |mut acc, el| {
                        el.map(|e| {
                            acc.extend(e);
                            acc
                        })
                    }),
                ActionEffect::ForAll(forall) => {
                    ground_effect_flat(forall.expression(), grounding, state)
                }
                ActionEffect::When { condition, effect } => {
                    condition.into_scoped(grounding).and_then(|condition| {
                        if condition.eval(state) {
                            ground_effect_flat(effect, grounding, state)
                        } else {
                            Ok(BTreeSet::new())
                        }
                    })
                }
                ActionEffect::Primitives(p) => match p {
                    Primitives::Pred(pred) => pred
                        .as_ground(grounding)
                        .map(|r| r.into_iter().map(ModifyState::Add).collect::<BTreeSet<_>>()),
                    Primitives::Not(not) => not
                        .as_ground(grounding)
                        .map(|r| r.into_iter().map(ModifyState::Del).collect::<BTreeSet<_>>()),
                },
            }
        }

        parameter_groundings
            .iter()
            .flat_map(|grounding| {
                grounding
                    .as_simple()
                    .map(|grounding| ground_effect_flat(&self.effect, &grounding, state))
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
    D: Into<Pdnf<LiftedPredicate>>,
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
    D: Into<Pdnf<LiftedPredicate>>,
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
    D: Into<Pdnf<LiftedPredicate>>,
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
    D: Into<Pdnf<LiftedPredicate>>,
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
    D: Into<Pdnf<LiftedPredicate>>,
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
    D: Into<Pdnf<LiftedPredicate>>,
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
            first_order::QuantifierBuilder,
            predicate::{PredicateBuilder, Value},
            propositional::{Formula as F, Primitives as Pr},
        },
        entity::{EntityStorage, ObjectStorage, TypeStorage},
        state::State,
    };

    use super::{ActionEffect as E, *};

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
                    ActionEffect::Primitives(Pr::Not(
                        p.values(vec![Value::param(&params[0])]).build().unwrap(),
                    )),
                    ActionEffect::Primitives(Pr::Pred(
                        p1.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .build()
            .unwrap();
    }

    #[test]
    fn test_when_effect() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe()]);
        let p3 = PredicateBuilder::new("p3").arguments(vec![t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::pred(p3.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::when(
                        F::pred(p2.values(vec![Value::param(&params[1])]).build().unwrap()),
                        E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    ),
                    E::npred(p3.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x.dupe()]).build().unwrap();
        let gp2 = p2.values(vec![y.dupe()]).build().unwrap();
        let gp3 = p3.values(vec![y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1.clone(), gp3.clone()]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.ground_effect(&res, &state).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([ModifyState::Del(
            p3.values(vec![y.dupe()]).build().unwrap(),
        )])]);

        assert_eq!(effects, correct_effects);

        let state = State::default().with_predicates(vec![gp1, gp2, gp3]);
        let res = state.ground_action(&action);
        let effects = action.ground_effect(&res, &state).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![x.dupe()]).build().unwrap()),
            ModifyState::Del(p3.values(vec![y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);
    }

    #[test]
    fn test_forall_effect() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);
        let y3 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe()])
            .precondition(|params| {
                Ok(F::pred(
                    p1.values(vec![Value::param(&params[0])]).build().unwrap(),
                ))
            })
            .effect(|params| {
                Ok(E::forall(
                    QuantifierBuilder::forall(vec![t2.dupe()])
                        .expression(|fa_params| {
                            Ok(E::pred(
                                p2.values(vec![
                                    Value::param(&params[0]),
                                    Value::bound(&fa_params[0]),
                                ])
                                .build()
                                .unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                ))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1.clone()]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([(
            &action_params[0],
            BTreeSet::from([x.clone()]),
        )]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.ground_effect(&res, &state).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Add(p2.values(vec![x.dupe(), y1.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![x.dupe(), y2.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![x.dupe(), y3.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);
    }
}
