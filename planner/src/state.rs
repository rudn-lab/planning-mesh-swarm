use crate::{
    action::{Action, ActionParameter},
    calculus::{
        evaluation::EvaluationContext,
        predicate::{Predicate, ResolvedPredicate},
        propositional::{DnfMembers, Expression, NormalForm, Not, Primitives},
    },
    entity::ObjectHandle,
};
use alloc::collections::{BTreeMap, BTreeSet};
use gazebo::dupe::Dupe;
use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: BTreeSet<ResolvedPredicate>,
}

impl State {
    pub fn with_predicates(mut self, predicates: &[ResolvedPredicate]) -> Self {
        self.predicates.extend(predicates.iter().cloned());
        self
    }
}

/// All possible valid resolutions for action parameters.
///
/// All permutations of the values are valid resolutions in a state.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParameterResolution<'a>(
    pub(crate) BTreeMap<&'a ActionParameter, BTreeSet<ObjectHandle>>,
);

impl<'a> ParameterResolution<'a> {
    /// Transforms the inner map of [ActionParameter] to multiple [ObjectHandle]s
    /// into several simple maps of [ActionParameter] to one [ObjectHandle].
    pub(crate) fn as_simple(
        &'a self,
    ) -> impl Iterator<Item = BTreeMap<&'a ActionParameter, ObjectHandle>> + use<'a> {
        self.0.values().multi_cartesian_product().map(|s| {
            self.0
                .keys()
                .copied()
                .zip(s.into_iter().map(Dupe::dupe))
                .collect::<BTreeMap<_, _>>()
        })
    }
}

impl State {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// This returns a "raw" map, because a predicate may not
    /// have all of the parameters in it, so it's only a partial resolution.
    fn resolve_predicate<'a>(
        &'a self,
        predicate: &'a Predicate,
    ) -> BTreeMap<&'a ActionParameter, BTreeSet<ObjectHandle>> {
        let keys = predicate.action_parameters();

        self.predicates
            .iter()
            .filter(|rp| rp.is_resolution_of(predicate))
            // Get only those values that correspond to an action parameter
            .flat_map(|rp| {
                keys.iter()
                    .zip(keys.iter().map(|(i, _)| rp.values()[*i].dupe()))
                    .map(|((_, ap), v)| (ap, v))
                    .collect_vec()
            })
            // This just transforms a list of ResolvedPredicates to the output type
            .fold(
                // Populating accumulator with all of the keys,
                // because even if there's no resolution for a predicate
                // we still need somehting that holds the value's type
                // (like ap in this case) to use later.
                // Because this method is being called for positive predicates
                // __and__ for negated ones too.
                keys.iter()
                    .map(|&(_, k)| (k, BTreeSet::new()))
                    .collect::<BTreeMap<_, _>>(),
                |mut acc, (ap, v)| {
                    acc.entry(ap)
                        .and_modify(|s: &mut BTreeSet<_>| {
                            let _ = s.insert(v.dupe());
                        })
                        .or_insert_with(|| BTreeSet::from([v]));
                    acc
                },
            )
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// This returns a "raw" map, because a predicate may not
    /// have all of the parameters in it, so it's only a partial resolution.
    fn resolve_negated_predicate<'a>(
        &'a self,
        predicate: &'a Not<Predicate, Predicate>,
    ) -> BTreeMap<&'a ActionParameter, BTreeSet<ObjectHandle>> {
        let predicate = predicate.inner();
        self.resolve_predicate(predicate)
            .into_iter()
            // Because it's a negated predicate,
            // possible resolutions should be all objects
            // except for those found for a positive predicate
            .map(|(r, obj_for_param)| {
                let objects = r
                    .r#type
                    .get_objects()
                    .into_iter()
                    .filter(|o| !obj_for_param.contains(o))
                    .collect::<BTreeSet<_>>();

                (r, objects)
            })
            .collect::<BTreeMap<_, _>>()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    fn handle_primitives<'a>(
        &'a self,
        p: &'a Primitives<Predicate>,
    ) -> BTreeMap<&'a ActionParameter, BTreeSet<ObjectHandle>> {
        match p {
            Primitives::Not(np) => self.resolve_negated_predicate(np),
            Primitives::Pred(p) => self.resolve_predicate(p),
        }
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolve_action<'a>(&'a self, action: &'a Action) -> BTreeSet<ParameterResolution<'a>> {
        action
            .precondition()
            .expression()
            .members()
            .iter()
            .filter_map(|m| match m {
                // Because DNF used "or" only as a top most operation
                // it means that we only need one operand to evaluate to true
                // in order to evaluate the whole DNF to true.
                // This allows us to treat all operands in isolation
                DnfMembers::And(and) => {
                    let o = and.members().iter().fold(
                        BTreeMap::default(),
                        |mut acc: BTreeMap<_, _>, p| {
                            self.handle_primitives(p).iter().for_each(|(k, v)| {
                                acc.entry(*k)
                                    .and_modify(|s: &mut BTreeSet<_>| {
                                        // Because we're doing intersection here
                                        // the resulting resolution set should apply
                                        // to all predicates with this parameter
                                        *s = s.intersection(v).cloned().collect()
                                    })
                                    .or_insert_with(|| v.clone());
                            });
                            acc
                        },
                    );

                    if o.is_empty() {
                        None
                    } else {
                        Some(o)
                    }
                }
                DnfMembers::Prim(p) => Some(self.handle_primitives(p)),
            })
            // Filter out those resolutions that don't cover all parameters
            .filter(|r| r.values().filter(|v| !v.is_empty()).count() == action.parameters().len())
            .map(ParameterResolution)
            .collect::<BTreeSet<_>>()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn apply_modification(&mut self, modifications: BTreeSet<ModifyState>) {
        modifications.into_iter().for_each(|m| {
            match m {
                ModifyState::Add(p) => self.predicates.insert(p),
                ModifyState::Del(p) => self.predicates.remove(&p),
            };
        });
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn unapply_modification(&mut self, modifications: BTreeSet<ModifyState>) {
        let _ = modifications.into_iter().map(|m| match m {
            ModifyState::Add(predicate) => self.predicates.remove(&predicate),
            ModifyState::Del(predicate) => self.predicates.insert(predicate),
        });
    }
}

impl EvaluationContext<ResolvedPredicate> for State {
    fn eval(&self, predicate: &ResolvedPredicate) -> bool {
        self.predicates.iter().any(|v| v == predicate)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModifyState {
    Add(ResolvedPredicate),
    Del(ResolvedPredicate),
}

#[allow(
    clippy::mutable_key_type,
    reason = "See SmartHandle Hash and Ord impls."
)]
#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{
        action::ActionBuilder,
        calculus::predicate::*,
        calculus::propositional::{
            And, Dnf, DnfMembers, Formula, FormulaMembers as FM, Primitives as Pr,
        },
        entity::*,
    };

    #[test]
    fn test_preciate_resolution() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);
        let b = entities.get_or_create_object("b", &t);
        let c = entities.get_or_create_object("c", &t);

        let p = PredicateBuilder::new("foo").arguments(&[t.dupe(), t.dupe()]);
        let ps = p
            .values(&[Value::object(&x), Value::object(&y)])
            .build()
            .unwrap();
        let rp = ps.into_resolved(&BTreeMap::new()).unwrap();
        // Just for the test creating a predicate with the same name
        // to simulate that there migth be several different
        // resolved versions of the same predicate in a state.
        // Normally names are unique, and this predicate should have
        // arguments like [ArgumentValue::ActionParameter],
        // which then get resolved and added to the state over the
        // course of the program.
        let p1 = PredicateBuilder::new("foo")
            .arguments(&[t.dupe(), t.dupe()])
            .values(&[Value::object(&x), Value::object(&b)])
            .build()
            .unwrap();
        let rp1 = p1.into_resolved(&BTreeMap::new()).unwrap();

        let p2 = PredicateBuilder::new("bar").arguments(&[t.dupe()]);
        let ps2 = p2.values(&[Value::object(&c)]).build().unwrap();
        let rp2 = ps2.into_resolved(&BTreeMap::new()).unwrap();

        let state = State::default().with_predicates(&[rp.clone(), rp1.clone(), rp2.clone()]);

        // No values, because no ActionParameters
        assert_eq!(state.resolve_predicate(&ps), BTreeMap::default());

        let ap0 = ActionParameter {
            parameter_idx: 0,
            r#type: t.dupe(),
        };
        let mut ap1 = ap0.dupe();
        ap1.parameter_idx = 1;
        let pp = p
            .values(&[Value::param(&ap0), Value::param(&ap1)])
            .build()
            .unwrap();
        // Resolutions from `rp` and `rp1`, because they are both `foo` and have the same type of arguments
        assert_eq!(
            state.resolve_predicate(&pp),
            BTreeMap::from([
                (&ap0, BTreeSet::from([x.dupe()])),
                (&ap1, BTreeSet::from([y.dupe(), b.dupe()]))
            ])
        );

        let pp2 = p2.values(&[Value::param(&ap0)]).build().unwrap();
        // Resolutions from negated `bar`
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(pp2)),
            BTreeMap::from([(&ap0, BTreeSet::from([x.dupe(), y.dupe(), b.dupe()]))])
        );

        // Predicate with no parameters doesn't get resolved
        let p3 = PredicateBuilder::new("baz")
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        assert_eq!(state.resolve_predicate(&p3), BTreeMap::default());

        // Negated predicate that isn't in a state should get all permutations of values
        let p4 = PredicateBuilder::new("qux")
            .arguments(&[t.dupe()])
            .values(&[Value::param(&ap0)])
            .build()
            .unwrap();
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(p4)),
            BTreeMap::from([(&ap0, BTreeSet::from([x, y, b, c]))])
        );
    }

    #[test]
    pub fn test_action_resolution_with_explicit_predicates() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe()]);
        let p3 = PredicateBuilder::new("p3").arguments(&[t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    FM::not(FM::pred(
                        p2.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                    FM::pred(p3.values(&[Value::param(&params[1])]).build().unwrap()),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p2.values(&[Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let rp1 = p1.resolved_values(&[x.dupe()]).build().unwrap();
        let rp3 = p3.resolved_values(&[y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp1, rp3]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.resolved_values(&[x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.resolved_values(&[y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_resolution_with_not_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    FM::not(FM::pred(
                        p2.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p2.values(&[Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let rp1 = p1.resolved_values(&[x.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp1]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.resolved_values(&[x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.resolved_values(&[y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_resolution_with_only_negated() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::not(FM::pred(
                        p1.values(&[Value::param(&params[0])]).build().unwrap(),
                    )),
                    FM::not(FM::pred(
                        p2.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p2.values(&[Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let rp1 = p1.resolved_values(&[x1.dupe()]).build().unwrap();
        let rp2 = p2.resolved_values(&[y1.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x2.dupe()])),
            (&action_params[1], BTreeSet::from([y2.dupe()])),
        ]))]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Add(p1.resolved_values(&[x2.dupe()]).build().unwrap()),
            ModifyState::Add(p2.resolved_values(&[y2.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_action_resolution() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::not(FM::pred(
                        p1.values(&[Value::param(&params[0])]).build().unwrap(),
                    )),
                    FM::not(FM::pred(
                        p2.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p2.values(&[Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let rp11 = p1.resolved_values(&[x1.dupe()]).build().unwrap();
        let rp12 = p1.resolved_values(&[x2.dupe()]).build().unwrap();
        let rp21 = p2.resolved_values(&[y1.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp11, rp12, rp21]);

        let res = state.resolve_action(&action);
        // This one is empty because not all params are resolved
        // the output is like this before partially resolved variants are removed:
        // ```
        // let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
        //     (&action_params[0], BTreeSet::from([])),
        //     (&action_params[1], BTreeSet::from([y2])),
        // ]))]);
        // ```
        assert!(res.is_empty());

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::new();

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_resolution_with_partially_resolved_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);
        let y3 = entities.get_or_create_object("y3", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe(), t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::pred(
                        p1.values(&[Value::param(&params[0]), Value::object(&x2)])
                            .build()
                            .unwrap(),
                    ),
                    FM::not(FM::pred(
                        p2.values(&[Value::object(&y1), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    )),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(
                        p1.values(&[Value::object(&x2), Value::param(&params[0])])
                            .build()
                            .unwrap(),
                    ),
                    Pr::pred(
                        p2.values(&[Value::param(&params[1]), Value::object(&y1)])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        let rp1 = p1.resolved_values(&[x1.dupe(), x2.dupe()]).build().unwrap();
        let rp2 = p2.resolved_values(&[y1.dupe(), y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x1.dupe()])),
            (&action_params[1], BTreeSet::from([y1.dupe(), y3.dupe()])),
        ]))]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y3.dupe(), y1.dupe()]).build().unwrap()),
            ]),
        ]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_with_mixed_dnf() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);
        let y3 = entities.get_or_create_object("y3", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe(), t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Dnf::new(vec![
                    DnfMembers::and(vec![
                        Pr::pred(
                            p1.values(&[Value::param(&params[0]), Value::object(&x2)])
                                .build()
                                .unwrap(),
                        ),
                        Pr::not(
                            p2.values(&[Value::object(&y1), Value::param(&params[1])])
                                .build()
                                .unwrap(),
                        ),
                    ]),
                    DnfMembers::and(vec![
                        Pr::not(
                            p1.values(&[Value::object(&x2), Value::param(&params[0])])
                                .build()
                                .unwrap(),
                        ),
                        Pr::pred(
                            p2.values(&[Value::param(&params[1]), Value::object(&y2)])
                                .build()
                                .unwrap(),
                        ),
                    ]),
                    DnfMembers::and(vec![]),
                ]))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(
                        p1.values(&[Value::object(&x2), Value::param(&params[0])])
                            .build()
                            .unwrap(),
                    ),
                    Pr::pred(
                        p2.values(&[Value::param(&params[1]), Value::object(&y1)])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        let rp1 = p1.resolved_values(&[x1.dupe(), x2.dupe()]).build().unwrap();
        let rp2 = p2.resolved_values(&[y1.dupe(), y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            ParameterResolution(BTreeMap::from([
                (&action_params[0], BTreeSet::from([x1.dupe()])),
                (&action_params[1], BTreeSet::from([y1.dupe(), y3.dupe()])),
            ])),
            ParameterResolution(BTreeMap::from([
                // Both `x1` and `x2` here, because the initial pred already doesn't
                // resolve because of the Object as the first parameter
                (&action_params[0], BTreeSet::from([x1.dupe(), x2.dupe()])),
                (&action_params[1], BTreeSet::from([y1.dupe()])),
            ])),
        ]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y3.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            // The same as the first one, will be re&moved by the set
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe(), x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
        ]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_resolution_with_subtypes() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let tt1 = entities.get_or_create_type("tt1");
        entities.create_inheritance(&tt1, &t1).unwrap();
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let xx1 = entities.get_or_create_object("xx1", &tt1);
        let xx2 = entities.get_or_create_object("xx2", &tt1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let pp1 = PredicateBuilder::new("pp1").arguments(&[tt1.clone()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), tt1.clone(), t2.dupe()])
            .precondition(|params| {
                Ok(Dnf::new(vec![
                    DnfMembers::and(vec![
                        Pr::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                        Pr::pred(pp1.values(&[Value::param(&params[1])]).build().unwrap()),
                        Pr::pred(p2.values(&[Value::param(&params[2])]).build().unwrap()),
                    ]),
                    DnfMembers::and(vec![
                        Pr::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                        Pr::pred(p1.values(&[Value::param(&params[1])]).build().unwrap()),
                        Pr::pred(p2.values(&[Value::param(&params[2])]).build().unwrap()),
                    ]),
                    // These won't get resolved as they are missing the other params
                    DnfMembers::prim(Pr::pred(
                        pp1.values(&[Value::param(&params[1])]).build().unwrap(),
                    )),
                    DnfMembers::prim(Pr::pred(
                        p2.values(&[Value::param(&params[2])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(p2.values(&[Value::param(&params[2])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let rp1_t1_1 = p1.resolved_values(&[x1.dupe()]).build().unwrap();
        let rp1_t1_2 = p1.resolved_values(&[x2.dupe()]).build().unwrap();
        let rp1_tt1_1 = p1.resolved_values(&[xx1.dupe()]).build().unwrap();
        let rp1_tt1_2 = p1.resolved_values(&[xx2.dupe()]).build().unwrap();

        let rpp1_tt1_1 = pp1.resolved_values(&[xx1.dupe()]).build().unwrap();
        let rpp1_tt1_2 = pp1.resolved_values(&[xx2.dupe()]).build().unwrap();

        let rp2_t2_1 = p2.resolved_values(&[y1.dupe()]).build().unwrap();
        let rp2_t2_2 = p2.resolved_values(&[y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[
            rp1_t1_1, rp1_t1_2, rp1_tt1_1, rp1_tt1_2, rpp1_tt1_1, rpp1_tt1_2, rp2_t2_1, rp2_t2_2,
        ]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            ParameterResolution(BTreeMap::from([
                // Getting both `x`s and `xx`s, because `xx`s of type `tt1`,
                // and the predicate is declared with `t1` which is a supertype of `tt1`
                (
                    &action_params[0],
                    BTreeSet::from([x1.clone(), x2.clone(), xx1.clone(), xx2.clone()]),
                ),
                // This predicate is defined with type `tt1`, which has no subentities,
                // so only values of this type will be resolved
                (
                    &action_params[1],
                    BTreeSet::from([xx1.clone(), xx2.clone()]),
                ),
                // Same as above, no subentities
                (&action_params[2], BTreeSet::from([y1.clone(), y2.clone()])),
            ])),
            ParameterResolution(BTreeMap::from([
                (
                    &action_params[0],
                    BTreeSet::from([x1.clone(), x2.clone(), xx1.clone(), xx2.clone()]),
                ),
                // This predicate is defined with `t1`, but it was turned into a specific
                // with `tt1`, so it should only resolve with `tt1`
                (
                    &action_params[1],
                    BTreeSet::from([xx1.clone(), xx2.clone()]),
                ),
                (&action_params[2], BTreeSet::from([y1.clone(), y2.clone()])),
            ])),
        ]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[xx1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[xx1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[xx2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.resolved_values(&[xx2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.resolved_values(&[y2.dupe()]).build().unwrap()),
            ]),
        ]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }

    #[test]
    fn test_multiple_predicates_for_same_parameter() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let a = entities.get_or_create_object("a", &t1);

        let p1 = PredicateBuilder::new("p1").arguments(&[t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(&[t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(Formula::new(FM::and(vec![
                    FM::pred(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    FM::pred(
                        p2.values(&[Value::param(&params[0]), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    ),
                ])))
            })
            .effect(|params| {
                Ok(And::new(vec![
                    Pr::not(p1.values(&[Value::param(&params[0])]).build().unwrap()),
                    Pr::pred(
                        p2.values(&[Value::param(&params[0]), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        let rp11 = p1.resolved_values(&[x.dupe()]).build().unwrap();
        let rp12 = p1.resolved_values(&[a.dupe()]).build().unwrap();
        let rp21 = p2.resolved_values(&[a.dupe(), y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(&[rp11, rp12, rp21]);

        let action_params = action.parameters();
        let res = state.resolve_action(&action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([ParameterResolution(BTreeMap::from([
            (&action_params[0], BTreeSet::from([a.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_resolution);

        let effects = action.resolved_effects(&res);
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.resolved_values(&[a.dupe()]).build().unwrap()),
            ModifyState::Add(p2.resolved_values(&[a.dupe(), y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.contains(rp),
                    ModifyState::Del(rp) => !st.predicates.contains(rp),
                })
            }));
    }
}
