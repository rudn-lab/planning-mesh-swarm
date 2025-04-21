use crate::{
    action::{Action, ActionParameter},
    calculus::{
        first_order::{BoundVariable, QuantifierSymbol},
        predicate::{GroundPredicate, LiftedPredicate, LiftedValue},
        propositional::Primitives,
        EvaluationContext,
    },
    entity::ObjectHandle,
    InternerSymbol,
};
use alloc::{
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    vec::Vec,
};
use gazebo::dupe::Dupe;
use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PredicateKey(InternerSymbol, usize);

impl From<&LiftedPredicate> for PredicateKey {
    fn from(value: &LiftedPredicate) -> Self {
        Self(*value.name(), value.arguments().len())
    }
}

impl From<&GroundPredicate> for PredicateKey {
    fn from(value: &GroundPredicate) -> Self {
        Self(*value.name(), value.arguments().len())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default, PartialOrd, Ord)]
pub struct State {
    predicates: BTreeMap<PredicateKey, BTreeSet<GroundPredicate>>,
}

impl State {
    pub fn with_predicates(mut self, predicates: Vec<GroundPredicate>) -> Self {
        for p in predicates {
            match self.predicates.entry((&p).into()) {
                Entry::Vacant(e) => {
                    let _ = e.insert(BTreeSet::from([p]));
                }
                Entry::Occupied(mut e) => {
                    let _ = e.get_mut().insert(p);
                }
            }
        }
        self
    }

    pub fn predicates(&self) -> impl Iterator<Item = &GroundPredicate> {
        self.predicates.values().flat_map(|v| v.iter())
    }
}

/// All possible valid groundings for action parameters.
///
/// All permutations of the values are valid groundings in a state.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParameterGrounding<'a>(pub(crate) BTreeMap<&'a ActionParameter, ObjectHandle>);

pub fn eval_dnf_clause_with_quantifier_prefix(
    prefix: &[QuantifierSymbol],
    clause_map: &BTreeMap<LiftedValue<'_>, BTreeSet<ObjectHandle>>,
) -> bool {
    fn eval(
        prefix: &[QuantifierSymbol],
        clause_map: &BTreeMap<LiftedValue<'_>, BTreeSet<ObjectHandle>>,
        var_assignment: &mut BTreeMap<BoundVariable, ObjectHandle>,
    ) -> bool {
        let Some((head, tail)) = prefix.split_first() else {
            // All of the prefix is applied.
            // Now we need to check whether the current application in `var_assignment`
            // can evaluate the clause to true.
            return clause_map.iter().all(|(k, v)| match k {
                LiftedValue::ActionParameter(_, _) => true,
                LiftedValue::BoundVariable(_, bv) => v.contains(
                    var_assignment
                        .get(bv)
                        .expect("BoundVariable is being used without a quantifier."),
                ),
            });
        };

        fn with_var_assignment<F>(
            var_assignment: &mut BTreeMap<BoundVariable, ObjectHandle>,
            bindings: Vec<(BoundVariable, ObjectHandle)>,
            mut f: F,
        ) -> bool
        where
            F: FnMut(&mut BTreeMap<BoundVariable, ObjectHandle>) -> bool,
        {
            let past_keys = var_assignment.keys().cloned().collect_vec();
            // Update the variables that we check in this branch
            var_assignment.extend(bindings);

            let res = f(var_assignment);

            var_assignment.retain(|k, _| past_keys.contains(k));
            res
        }

        match head {
            QuantifierSymbol::ForAll(variables) => variables
                .iter()
                .map(|v| v.r#type.get_objects().into_iter().map(|e| (v.dupe(), e)))
                .multi_cartesian_product()
                // Ensure that for all permutations of this quantifier's
                // bound variables the rest of the expression satisfies.
                .all(|v| with_var_assignment(var_assignment, v, |x| eval(tail, clause_map, x))),
            QuantifierSymbol::Exists(variables) => variables
                .iter()
                .map(|v| v.r#type.get_objects().into_iter().map(|e| (v.dupe(), e)))
                .multi_cartesian_product()
                // The rest of the expression should evaluate to true
                // for at least one of the bound variables
                .any(|v| with_var_assignment(var_assignment, v, |x| eval(tail, clause_map, x))),
        }
    }

    eval(prefix, clause_map, &mut BTreeMap::new())
}

impl State {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// This returns a "raw" map, because a predicate may not
    /// have all of the parameters in it, so it's only a partial grounding.
    fn ground_predicate<'a>(
        &'a self,
        predicate: &'a LiftedPredicate,
    ) -> BTreeMap<LiftedValue<'a>, BTreeSet<ObjectHandle>> {
        let keys = predicate.lifted_values();

        // Populating accumulator with all of the keys,
        // because even if there's no grounding for a predicate
        // we still need somehting that holds the value's type
        // (like ap in this case) to use later.
        // Because this method is being called for positive predicates
        // __and__ for negated ones too.
        let acc = || {
            keys.iter()
                .map(|&k| (k, BTreeSet::new()))
                .collect::<BTreeMap<_, _>>()
        };

        self.predicates
            .get(&predicate.into())
            .map(|preds| {
                preds
                    .iter()
                    .filter(|gp| gp.is_ground_of(predicate))
                    // Get only those values that correspond to an action parameter
                    .flat_map(|gp| {
                        keys.iter()
                            .zip(keys.iter().map(|k| gp.values()[k.idx()].dupe()))
                            .collect_vec()
                    })
                    .fold(acc(), |mut acc, (lv, v)| {
                        acc.entry(*lv)
                            .and_modify(|s: &mut BTreeSet<_>| {
                                let _ = s.insert(v.dupe());
                            })
                            .or_insert_with(|| BTreeSet::from([v]));
                        acc
                    })
            })
            .unwrap_or_else(acc)
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// This returns a "raw" map, because a predicate may not
    /// have all of the parameters in it, so it's only a partial grounding.
    fn ground_negated_predicate<'a>(
        &'a self,
        predicate: &'a LiftedPredicate,
    ) -> BTreeMap<LiftedValue<'a>, BTreeSet<ObjectHandle>> {
        self.ground_predicate(predicate)
            .into_iter()
            // Because it's a negated predicate,
            // possible groundings should be all objects
            // except for those found for a positive predicate
            .map(|(r, obj_for_param)| {
                let objects = match r {
                    LiftedValue::ActionParameter(_, ap) => &ap.r#type,
                    LiftedValue::BoundVariable(_, bv) => &bv.r#type,
                }
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
        p: &'a Primitives<LiftedPredicate>,
    ) -> BTreeMap<LiftedValue<'a>, BTreeSet<ObjectHandle>> {
        match p {
            Primitives::Pred(p) => self.ground_predicate(p),
            Primitives::Not(np) => self.ground_negated_predicate(np),
        }
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    fn handle_clause<'a>(
        &'a self,
        and: &'a BTreeSet<Primitives<LiftedPredicate>>,
    ) -> Option<BTreeMap<LiftedValue<'a>, BTreeSet<ObjectHandle>>> {
        let o = and
            .iter()
            .fold(BTreeMap::default(), |mut acc: BTreeMap<_, _>, p| {
                self.handle_primitives(p).into_iter().for_each(|(k, v)| {
                    acc.entry(k)
                        .and_modify(|s: &mut BTreeSet<_>| {
                            // Because we're doing intersection here
                            // the resulting grounding set should apply
                            // to all predicates with this parameter
                            *s = s.intersection(&v).cloned().collect()
                        })
                        .or_insert_with(|| v.clone());
                });
                acc
            });

        // Filter out empty action parameter groundings
        if o.iter().all(|(k, v)| match k {
            LiftedValue::ActionParameter(_, _) => v.is_empty(),
            LiftedValue::BoundVariable(_, _) => true,
        }) {
            None
        } else {
            Some(o)
        }
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn ground_action<'a>(&'a self, action: &'a Action) -> BTreeSet<ParameterGrounding<'a>> {
        let prefix = action.precondition().prefix();
        let params_in_effect = action.effect().action_parameters();
        action
            .precondition()
            .matrix()
            .clauses()
            .iter()
            // We don't need to filter clauses by action parameters first
            // because our DNF is a full DNF, which means all predicates
            // and therefore all action parameters appear in each clause.
            .filter_map(|clause|
                // Because DNF used "or" only as a top most operation
                // it means that we only need one clause to evaluate to true
                // in order to evaluate the whole DNF to true.
                // This allows us to treat all clauses in isolation
                self.handle_clause(clause))
            // Filter out those groundings that didn't ground
            // all of the action parameters
            // that appear in the effect.
            // This means that not all action parametes
            // need to be present in the grounding.
            .filter(|g| {
                g.iter()
                    .filter_map(|(&k, v)| {
                        if v.is_empty() {
                            None
                        } else {
                            match k {
                                LiftedValue::ActionParameter(_, ap) => Some(ap),
                                LiftedValue::BoundVariable(_, _) => None,
                            }
                        }
                    })
                    .collect::<BTreeSet<_>>()
                    .is_superset(&params_in_effect)
            })
            // For each clause check if it is satisfactory under the prefix.
            // We can do this for each clause because our [DNF] is full,
            // that is, each of its clauses contains each predicate of the original expression
            // either as is or in its negated form.
            .filter(|clause_map| eval_dnf_clause_with_quantifier_prefix(prefix, clause_map))
            .map(|map| {
                map.into_iter()
                    .filter_map(|(k, v)| match k {
                        LiftedValue::ActionParameter(_, ap) => Some((ap, v)),
                        LiftedValue::BoundVariable(_, _) => None,
                    })
                    .collect::<BTreeMap<_, _>>()
            })
            .flat_map(|g| {
                let (keys, values): (Vec<_>, Vec<_>) = g.into_iter().unzip();
                values
                    .into_iter()
                    .map(|s| s.into_iter().collect_vec())
                    .multi_cartesian_product()
                    .map(move |s| keys.iter().copied().zip(s).collect::<BTreeMap<_, _>>())
            })
            .map(ParameterGrounding)
            .collect::<BTreeSet<_>>()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn modify(&self, modifications: BTreeSet<ModifyState>) -> Self {
        let mut predicates = self.predicates.clone();
        modifications.into_iter().for_each(|m| {
            match m {
                ModifyState::Add(p) => match predicates.entry((&p).into()) {
                    Entry::Vacant(e) => {
                        let _ = e.insert(BTreeSet::from([p]));
                    }
                    Entry::Occupied(mut e) => {
                        let _ = e.get_mut().insert(p);
                    }
                },
                ModifyState::Del(p) => match predicates.entry((&p).into()) {
                    Entry::Vacant(_) => {
                        // No-op, TODO: add logging
                    }
                    Entry::Occupied(mut e) => {
                        let _ = e.get_mut().remove(&p);
                    }
                },
            };
        });
        State { predicates }
    }
}

impl EvaluationContext<GroundPredicate> for State {
    fn matching_predicates(&self, key: &PredicateKey) -> Option<&BTreeSet<GroundPredicate>> {
        self.predicates.get(key)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModifyState {
    Add(GroundPredicate),
    Del(GroundPredicate),
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
        action::{ActionBuilder, ActionEffect as E},
        calculus::{
            first_order::{QuantifiedFormula as F, QuantifierBuilder},
            predicate::*,
        },
        entity::*,
    };

    #[test]
    fn test_preciate_grounding() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);
        let b = entities.get_or_create_object("b", &t);
        let c = entities.get_or_create_object("c", &t);

        let p = PredicateBuilder::new("foo").arguments(vec![t.dupe(), t.dupe()]);
        let ps = p
            .values(vec![Value::object(&x), Value::object(&y)])
            .build()
            .unwrap();
        let rp = ps.as_ground(&BTreeMap::new()).unwrap();
        // Just for the test creating a predicate with the same name
        // to simulate that there migth be several different
        // ground versions of the same predicate in a state.
        // Normally names are unique, and this predicate should have
        // arguments like [ArgumentValue::ActionParameter],
        // which then get ground and added to the state over the
        // course of the program.
        let p1 = PredicateBuilder::new("foo")
            .arguments(vec![t.dupe(), t.dupe()])
            .values(vec![Value::object(&x), Value::object(&b)])
            .build()
            .unwrap();
        let gp1 = p1.as_ground(&BTreeMap::new()).unwrap();

        let p2 = PredicateBuilder::new("bar").arguments(vec![t.dupe()]);
        let ps2 = p2.values(vec![Value::object(&c)]).build().unwrap();
        let gp2 = ps2.as_ground(&BTreeMap::new()).unwrap();

        let state = State::default().with_predicates(
            rp.iter()
                .chain(gp1.iter())
                .chain(gp2.iter())
                .cloned()
                .collect_vec(),
        );

        // No values, because no ActionParameters
        assert_eq!(state.ground_predicate(&ps), BTreeMap::default());

        let ap0 = ActionParameter {
            parameter_idx: 0,
            r#type: t.dupe(),
        };
        let mut ap1 = ap0.dupe();
        ap1.parameter_idx = 1;
        let pp = p
            .values(vec![Value::param(&ap0), Value::param(&ap1)])
            .build()
            .unwrap();
        // Groundings from `rp` and `gp1`, because they are both `foo` and have the same type of arguments
        assert_eq!(
            state.ground_predicate(&pp),
            BTreeMap::from([
                (
                    LiftedValue::ActionParameter(0, &ap0),
                    BTreeSet::from([x.dupe()])
                ),
                (
                    LiftedValue::ActionParameter(1, &ap1),
                    BTreeSet::from([y.dupe(), b.dupe()])
                )
            ])
        );

        let pp2 = p2.values(vec![Value::param(&ap0)]).build().unwrap();
        // Groundings from negated `bar`
        assert_eq!(
            state.ground_negated_predicate(&pp2),
            BTreeMap::from([(
                LiftedValue::ActionParameter(0, &ap0),
                BTreeSet::from([x.dupe(), y.dupe(), b.dupe()])
            )])
        );

        // Predicate with no parameters doesn't get ground
        let p3 = PredicateBuilder::new("baz")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        assert_eq!(state.ground_predicate(&p3), BTreeMap::default());

        // Negated predicate that isn't in a state should get all permutations of values
        let p4 = PredicateBuilder::new("qux")
            .arguments(vec![t.dupe()])
            .values(vec![Value::param(&ap0)])
            .build()
            .unwrap();
        assert_eq!(
            state.ground_negated_predicate(&p4),
            BTreeMap::from([(
                LiftedValue::ActionParameter(0, &ap0),
                BTreeSet::from([x, y, b, c])
            )])
        );
    }

    #[test]
    pub fn test_action_grounding_with_explicit_predicates() {
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
                    F::not(F::pred(
                        p2.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                    F::pred(p3.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x.dupe()]).build().unwrap();
        let gp3 = p3.values(vec![y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1, gp3]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], x.dupe()),
            (&action_params[1], y.dupe()),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_grounding_with_not_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::not(F::pred(
                        p2.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], x.dupe()),
            (&action_params[1], y.dupe()),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_grounding_with_only_negated() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::not(F::pred(
                        p1.values(vec![Value::param(&params[0])]).build().unwrap(),
                    )),
                    F::not(F::pred(
                        p2.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x1.dupe()]).build().unwrap();
        let gp2 = p2.values(vec![y1.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1, gp2]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], x2.dupe()),
            (&action_params[1], y2.dupe()),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Add(p1.values(vec![x2.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_action_grounding() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");

        let x1 = entities.get_or_create_object("x1", &t1);
        let _ = entities.get_or_create_object("x2", &t1);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::not(F::pred(
                        p1.values(vec![Value::param(&params[0])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|_params| Ok(E::and(vec![])))
            .build()
            .unwrap();

        let gp11 = p1.values(vec![x1.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp11]);

        let res = state.ground_action(&action);
        // This one is empty because precondition contradicts itself
        println!("{:?}", res);
        assert!(res.is_empty());
    }

    #[test]
    pub fn test_partial_action_grounding() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::not(F::pred(
                        p1.values(vec![Value::param(&params[0])]).build().unwrap(),
                    )),
                    F::not(F::pred(
                        p2.values(vec![Value::param(&params[1])]).build().unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[1])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp11 = p1.values(vec![x1.dupe()]).build().unwrap();
        let gp12 = p1.values(vec![x2.dupe()]).build().unwrap();
        let gp21 = p2.values(vec![y1.dupe()]).build().unwrap();
        let gp22 = p2.values(vec![y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp11, gp12, gp21, gp22]);

        let res = state.ground_action(&action);
        // This one is empty because not all params are ground
        // the output is like this before partially ground variants are removed:
        // ```
        // let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
        //     (&action_params[0], BTreeSet::from([])),
        //     (&action_params[1], BTreeSet::from([y2])),
        // ]))]);
        // ```
        println!("{:?}", res);
        assert!(res.is_empty());

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::new();

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_grounding_with_partially_ground_predicates() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);
        let y3 = entities.get_or_create_object("y3", &t2);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe(), t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(
                        p1.values(vec![Value::param(&params[0]), Value::object(&x2)])
                            .build()
                            .unwrap(),
                    ),
                    F::not(F::pred(
                        p2.values(vec![Value::object(&y1), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    )),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(
                        p1.values(vec![Value::object(&x2), Value::param(&params[0])])
                            .build()
                            .unwrap(),
                    ),
                    E::pred(
                        p2.values(vec![Value::param(&params[1]), Value::object(&y1)])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        let gp1 = p1.values(vec![x1.dupe(), x2.dupe()]).build().unwrap();
        let gp2 = p2.values(vec![y1.dupe(), y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp1, gp2]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([
            ParameterGrounding(BTreeMap::from([
                (&action_params[0], x1.dupe()),
                (&action_params[1], y1.dupe()),
            ])),
            ParameterGrounding(BTreeMap::from([
                (&action_params[0], x1.dupe()),
                (&action_params[1], y3.dupe()),
            ])),
        ]);

        assert_eq!(res, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y3.dupe(), y1.dupe()]).build().unwrap()),
            ]),
        ]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_action_grounding_with_subtypes() {
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

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let pp1 = PredicateBuilder::new("pp1").arguments(vec![tt1.clone()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), tt1.clone(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::pred(pp1.values(vec![Value::param(&params[1])]).build().unwrap()),
                    F::pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let gp1_t1_1 = p1.values(vec![x1.dupe()]).build().unwrap();
        let gp1_t1_2 = p1.values(vec![x2.dupe()]).build().unwrap();
        let gp1_tt1_1 = p1.values(vec![xx1.dupe()]).build().unwrap();
        let gp1_tt1_2 = p1.values(vec![xx2.dupe()]).build().unwrap();

        let rpp1_tt1_1 = pp1.values(vec![xx1.dupe()]).build().unwrap();
        let rpp1_tt1_2 = pp1.values(vec![xx2.dupe()]).build().unwrap();

        let gp2_t2_1 = p2.values(vec![y1.dupe()]).build().unwrap();
        let gp2_t2_2 = p2.values(vec![y2.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![
            gp1_t1_1, gp1_t1_2, gp1_tt1_1, gp1_tt1_2, rpp1_tt1_1, rpp1_tt1_2, gp2_t2_1, gp2_t2_2,
        ]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        // Correctness is easier to express like this
        let correct_grounding = BTreeMap::from([
            // Getting both `x`s and `xx`s, because `xx`s of type `tt1`,
            // and the predicate is declared with `t1` which is a supertype of `tt1`
            (
                &action_params[0],
                BTreeSet::from([x1.dupe(), x2.dupe(), xx1.dupe(), xx2.dupe()]),
            ),
            // This predicate is defined with type `tt1`, which has no subentities,
            // so only values of this type will be ground
            (&action_params[1], BTreeSet::from([xx1.dupe(), xx2.dupe()])),
            // Same as above, no subentities
            (&action_params[2], BTreeSet::from([y1.dupe(), y2.dupe()])),
        ]);

        println!("{:?}", res);
        let mapped_grounding = res.iter().map(|g| g.0.clone()).fold(
            BTreeMap::<&ActionParameter, BTreeSet<_>>::new(),
            |mut acc, g| {
                for &k in g.keys() {
                    acc.entry(k)
                        .and_modify(|e| {
                            let _ = e.insert(g[k].dupe());
                        })
                        .or_insert_with(|| BTreeSet::from([g[k].dupe()]));
                }
                acc
            },
        );
        assert_eq!(mapped_grounding, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![xx1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![xx1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![xx2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![xx2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
            ]),
        ]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), tt1.clone(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::pred(p1.values(vec![Value::param(&params[1])]).build().unwrap()),
                    F::pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeMap::from([
            (
                &action_params[0],
                BTreeSet::from([x1.dupe(), x2.dupe(), xx1.dupe(), xx2.dupe()]),
            ),
            // This predicate is defined with `t1`, but it was turned into a specific
            // with `tt1`, so it should only ground with `tt1`
            (&action_params[1], BTreeSet::from([xx1.dupe(), xx2.dupe()])),
            (&action_params[2], BTreeSet::from([y1.dupe(), y2.dupe()])),
        ]);

        let mapped_grounding = res.iter().map(|g| g.0.clone()).fold(
            BTreeMap::<&ActionParameter, BTreeSet<_>>::new(),
            |mut acc, g| {
                for &k in g.keys() {
                    acc.entry(k)
                        .and_modify(|e| {
                            let _ = e.insert(g[k].dupe());
                        })
                        .or_insert_with(|| BTreeSet::from([g[k].dupe()]));
                }
                acc
            },
        );
        assert_eq!(mapped_grounding, correct_grounding);
    }

    #[test]
    fn test_multiple_predicates_for_same_parameter() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let a = entities.get_or_create_object("a", &t1);

        let p1 = PredicateBuilder::new("p1").arguments(vec![t1.dupe()]);
        let p2 = PredicateBuilder::new("p2").arguments(vec![t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action")
            .parameters(vec![t1.dupe(), t2.dupe()])
            .precondition(|params| {
                Ok(F::and(vec![
                    F::pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::pred(
                        p2.values(vec![Value::param(&params[0]), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .effect(|params| {
                Ok(E::and(vec![
                    E::npred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                    E::pred(
                        p2.values(vec![Value::param(&params[0]), Value::param(&params[1])])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        let gp11 = p1.values(vec![x.dupe()]).build().unwrap();
        let gp12 = p1.values(vec![a.dupe()]).build().unwrap();
        let gp21 = p2.values(vec![a.dupe(), y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gp11, gp12, gp21]);

        let action_params = action.parameters();
        let res = state.ground_action(&action);
        assert!(!res.is_empty());

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], a.dupe()),
            (&action_params[1], y.dupe()),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = res
            .iter()
            .map(|res| action.ground_effect(res, &state).unwrap())
            .collect::<BTreeSet<_>>();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![a.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![a.dupe(), y.dupe()]).build().unwrap()),
        ])]);

        assert_eq!(effects, correct_effects);

        assert!(effects
            .into_iter()
            .map(|modifications| {
                let st = state.modify(modifications.clone());
                (st, modifications)
            })
            .all(|(st, modifications)| {
                modifications.iter().all(|m| match m {
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    fn test_action_precondition_with_forall() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t1);
        let a = entities.get_or_create_object("a", &t2);
        let b = entities.get_or_create_object("b", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("action_forall")
            .parameters(vec![t1.dupe()])
            .precondition(|params| {
                Ok(F::forall(
                    QuantifierBuilder::forall(vec![t2.dupe()])
                        .expression(|q_params| {
                            Ok(F::pred(
                                p.values(vec![
                                    Value::param(&params[0]),
                                    Value::bound(&q_params[0]),
                                ])
                                .build()
                                .unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                ))
            })
            .effect(|_| Ok(E::And(Vec::new())))
            .build()
            .unwrap();

        let pred_a = p.values(vec![x.dupe(), a.dupe()]).build().unwrap();
        let pred_b = p.values(vec![y.dupe(), b.dupe()]).build().unwrap();

        let state = State::default().with_predicates(vec![pred_a.clone(), pred_b.clone()]);

        let res = state.ground_action(&action);
        let correct = BTreeSet::from([
            ParameterGrounding(BTreeMap::from([(&action.parameters()[0], x.dupe())])),
            ParameterGrounding(BTreeMap::from([(&action.parameters()[0], y.dupe())])),
        ]);

        assert_eq!(res, correct);
    }

    #[test]
    fn test_action_precondition_with_exists() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let r = PredicateBuilder::new("r").arguments(vec![t1.dupe()]);
        let s = PredicateBuilder::new("s").arguments(vec![t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("exists_or")
            .parameters(vec![t1.dupe()])
            .precondition(|params| {
                Ok(F::or(vec![
                    F::pred(r.values(vec![Value::param(&params[0])]).build().unwrap()),
                    F::exists(
                        QuantifierBuilder::exists(vec![t2.dupe()])
                            .expression(|q_params| {
                                Ok(F::pred(
                                    s.values(vec![
                                        Value::param(&params[0]),
                                        Value::bound(&q_params[0]),
                                    ])
                                    .build()
                                    .unwrap(),
                                ))
                            })
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .effect(|_| Ok(E::And(Vec::new())))
            .build()
            .unwrap();

        let gs = s.values(vec![x.dupe(), y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gs]);

        let res = state.ground_action(&action);
        let expected = BTreeSet::from([ParameterGrounding(BTreeMap::from([(
            &action.parameters()[0],
            x.dupe(),
        )]))]);

        assert_eq!(res, expected);
    }

    #[test]
    fn test_action_precondition_with_forall_and_exists() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t1.dupe(), t2.dupe()]);

        let action = ActionBuilder::new("forall_and_exists")
            .parameters(vec![t1.dupe()])
            .precondition(|params| {
                let forall = F::forall(
                    QuantifierBuilder::forall(vec![t2.dupe()])
                        .expression(|q_params| {
                            Ok(F::pred(
                                p.values(vec![
                                    Value::param(&params[0]),
                                    Value::bound(&q_params[0]),
                                ])
                                .build()
                                .unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                );

                let exists = F::exists(
                    QuantifierBuilder::exists(vec![t2.dupe()])
                        .expression(|q_params| {
                            Ok(F::pred(
                                q.values(vec![
                                    Value::param(&params[0]),
                                    Value::bound(&q_params[0]),
                                ])
                                .build()
                                .unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                );

                Ok(F::and(vec![forall, exists]))
            })
            .effect(|_| Ok(E::And(Vec::new())))
            .build()
            .unwrap();

        let gp1 = p.values(vec![x.dupe(), y1.dupe()]).build().unwrap();
        let gp2 = p.values(vec![x.dupe(), y2.dupe()]).build().unwrap();
        let gq = q.values(vec![x.dupe(), y2.dupe()]).build().unwrap();

        let state = State::default().with_predicates(vec![gp1, gp2, gq]);

        let res = state.ground_action(&action);
        let expected = BTreeSet::from([ParameterGrounding(BTreeMap::from([(
            &action.parameters()[0],
            x.dupe(),
        )]))]);

        assert_eq!(res, expected);
    }
}
