use crate::{
    action::{Action, ActionParameter},
    calculus::{
        first_order::{BoundVariable, QuantifierSymbol},
        predicate::{GroundedPredicate, LiftedPredicate, LiftedValue},
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

impl From<&GroundedPredicate> for PredicateKey {
    fn from(value: &GroundedPredicate) -> Self {
        Self(*value.name(), value.arguments().len())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: BTreeMap<PredicateKey, BTreeSet<GroundedPredicate>>,
}

impl State {
    pub fn with_predicates(mut self, predicates: Vec<GroundedPredicate>) -> Self {
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
}

/// All possible valid groundings for action parameters.
///
/// All permutations of the values are valid groundings in a state.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParameterGrounding<'a>(pub(crate) BTreeMap<&'a ActionParameter, BTreeSet<ObjectHandle>>);

impl<'a> ParameterGrounding<'a> {
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
                    .filter(|rp| rp.is_grounded_from(predicate))
                    // Get only those values that correspond to an action parameter
                    .flat_map(|rp| {
                        keys.iter()
                            .zip(keys.iter().map(|k| rp.values()[k.idx()].dupe()))
                            .collect_vec()
                    })
                    // This just transforms a list of GroundedPredicates to the output type
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

        if o.is_empty() {
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
        let num_params = action.parameters().len();
        let prefix = action.precondition().prefix();
        action
            .precondition()
            .matrix()
            .clauses
            .iter()
            // Filter out clauses that don't contain all of the action parameters
            .filter(|clause| {
                clause
                    .iter()
                    .map(|p| p.inner())
                    .flat_map(|p| p.action_parameters())
                    .sorted()
                    .dedup()
                    .count()
                    == num_params
            })
            .filter_map(|clause| 
                // Because DNF used "or" only as a top most operation
                // it means that we only need one clause to evaluate to true
                // in order to evaluate the whole DNF to true.
                // This allows us to treat all clauses in isolation
                self.handle_clause(clause),
            )
            // Filter out those groundings that didn't ground all of the action parameters
            .filter(|r| {
                r.iter()
                    .filter(|(k, v)| {
                        matches!(k, LiftedValue::ActionParameter(_, _)) && !v.is_empty()
                    })
                    .count()
                    == num_params
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
            .map(ParameterGrounding)
            .collect::<BTreeSet<_>>()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn apply_modification(&mut self, modifications: BTreeSet<ModifyState>) {
        modifications.into_iter().for_each(|m| {
            match m {
                ModifyState::Add(p) => match self.predicates.entry((&p).into()) {
                    Entry::Vacant(e) => {
                        let _ = e.insert(BTreeSet::from([p]));
                    }
                    Entry::Occupied(mut e) => {
                        let _ = e.get_mut().insert(p);
                    }
                },
                ModifyState::Del(p) => match self.predicates.entry((&p).into()) {
                    Entry::Vacant(_) => {
                        // No-op, TODO: add logging
                    }
                    Entry::Occupied(mut e) => {
                        let _ = e.get_mut().remove(&p);
                    }
                },
            };
        });
    }
}

impl EvaluationContext<GroundedPredicate> for State {
    fn matching_predicates(&self, key: &PredicateKey) -> Option<&BTreeSet<GroundedPredicate>> {
        self.predicates.get(key)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModifyState {
    Add(GroundedPredicate),
    Del(GroundedPredicate),
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
            propositional::{Dnf, Primitives as Pr},
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
        let rp = ps.into_grounded(&BTreeMap::new()).unwrap();
        // Just for the test creating a predicate with the same name
        // to simulate that there migth be several different
        // grounded versions of the same predicate in a state.
        // Normally names are unique, and this predicate should have
        // arguments like [ArgumentValue::ActionParameter],
        // which then get grounded and added to the state over the
        // course of the program.
        let p1 = PredicateBuilder::new("foo")
            .arguments(vec![t.dupe(), t.dupe()])
            .values(vec![Value::object(&x), Value::object(&b)])
            .build()
            .unwrap();
        let gp1 = p1.into_grounded(&BTreeMap::new()).unwrap();

        let p2 = PredicateBuilder::new("bar").arguments(vec![t.dupe()]);
        let ps2 = p2.values(vec![Value::object(&c)]).build().unwrap();
        let gp2 = ps2.into_grounded(&BTreeMap::new()).unwrap();

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

        // Predicate with no parameters doesn't get grounded
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
            (&action_params[0], BTreeSet::from([x.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y.dupe()]).build().unwrap()),
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
            (&action_params[0], BTreeSet::from([x.clone()])),
            (&action_params[1], BTreeSet::from([y.clone()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![x.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y.dupe()]).build().unwrap()),
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
            (&action_params[0], BTreeSet::from([x2.dupe()])),
            (&action_params[1], BTreeSet::from([y2.dupe()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Add(p1.values(vec![x2.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![y2.dupe()]).build().unwrap()),
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
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_action_grounding() {
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
        // This one is empty because not all params are grounded
        // the output is like this before partially grounded variants are removed:
        // ```
        // let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
        //     (&action_params[0], BTreeSet::from([])),
        //     (&action_params[1], BTreeSet::from([y2])),
        // ]))]);
        // ```
        println!("{:?}", res);
        assert!(res.is_empty());

        let effects = action.grounded_effect(&res).unwrap();
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
                    ModifyState::Add(rp) => st.predicates.values().flatten().contains(rp),
                    ModifyState::Del(rp) => !st.predicates.values().flatten().contains(rp),
                })
            }));
    }

    #[test]
    pub fn test_impossible_grounding_with_partially_grounded_predicates() {
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

        let correct_grounding = BTreeSet::from([ParameterGrounding(BTreeMap::from([
            (&action_params[0], BTreeSet::from([x1.dupe()])),
            (&action_params[1], BTreeSet::from([y1.dupe(), y3.dupe()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
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
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
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
    pub fn test_with_mixed_dnf() {
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
                Ok(Dnf {
                    clauses: BTreeSet::from([
                        BTreeSet::from([
                            Pr::Pred(
                                p1.values(vec![Value::param(&params[0]), Value::object(&x2)])
                                    .build()
                                    .unwrap(),
                            ),
                            Pr::Not(
                                p2.values(vec![Value::object(&y1), Value::param(&params[1])])
                                    .build()
                                    .unwrap(),
                            ),
                        ]),
                        BTreeSet::from([
                            Pr::Not(
                                p1.values(vec![Value::object(&x2), Value::param(&params[0])])
                                    .build()
                                    .unwrap(),
                            ),
                            Pr::Pred(
                                p2.values(vec![Value::param(&params[1]), Value::object(&y2)])
                                    .build()
                                    .unwrap(),
                            ),
                        ]),
                        BTreeSet::new(),
                    ]),
                })
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
                (&action_params[0], BTreeSet::from([x1.dupe()])),
                (&action_params[1], BTreeSet::from([y1.dupe(), y3.dupe()])),
            ])),
            ParameterGrounding(BTreeMap::from([
                // Both `x1` and `x2` here, because the initial pred already doesn't
                // ground because of the Object as the first parameter
                (&action_params[0], BTreeSet::from([x1.dupe(), x2.dupe()])),
                (&action_params[1], BTreeSet::from([y1.dupe()])),
            ])),
        ]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
        let correct_effects = BTreeSet::from([
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y3.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            // The same as the first one, will be re&moved by the set
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x1.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe(), y1.dupe()]).build().unwrap()),
            ]),
            BTreeSet::from([
                ModifyState::Del(p1.values(vec![x2.dupe(), x2.dupe()]).build().unwrap()),
                ModifyState::Add(p2.values(vec![y1.dupe(), y1.dupe()]).build().unwrap()),
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
                Ok(Dnf {
                    clauses: BTreeSet::from([
                        BTreeSet::from([
                            Pr::Pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                            Pr::Pred(pp1.values(vec![Value::param(&params[1])]).build().unwrap()),
                            Pr::Pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                        ]),
                        BTreeSet::from([
                            Pr::Pred(p1.values(vec![Value::param(&params[0])]).build().unwrap()),
                            Pr::Pred(p1.values(vec![Value::param(&params[1])]).build().unwrap()),
                            Pr::Pred(p2.values(vec![Value::param(&params[2])]).build().unwrap()),
                        ]),
                        // These won't get grounded as they are missing the other params.
                        // Strictly speaking this should never happen, as [DNF] is full dnf,
                        // but we're doing this just for the test's sake.
                        BTreeSet::from([Pr::Pred(
                            pp1.values(vec![Value::param(&params[1])]).build().unwrap(),
                        )]),
                        BTreeSet::from([Pr::Pred(
                            p2.values(vec![Value::param(&params[2])]).build().unwrap(),
                        )]),
                    ]),
                })
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

        let correct_grounding = BTreeSet::from([
            ParameterGrounding(BTreeMap::from([
                // Getting both `x`s and `xx`s, because `xx`s of type `tt1`,
                // and the predicate is declared with `t1` which is a supertype of `tt1`
                (
                    &action_params[0],
                    BTreeSet::from([x1.clone(), x2.clone(), xx1.clone(), xx2.clone()]),
                ),
                // This predicate is defined with type `tt1`, which has no subentities,
                // so only values of this type will be grounded
                (
                    &action_params[1],
                    BTreeSet::from([xx1.clone(), xx2.clone()]),
                ),
                // Same as above, no subentities
                (&action_params[2], BTreeSet::from([y1.clone(), y2.clone()])),
            ])),
            ParameterGrounding(BTreeMap::from([
                (
                    &action_params[0],
                    BTreeSet::from([x1.clone(), x2.clone(), xx1.clone(), xx2.clone()]),
                ),
                // This predicate is defined with `t1`, but it was turned into a specific
                // with `tt1`, so it should only grounded with `tt1`
                (
                    &action_params[1],
                    BTreeSet::from([xx1.clone(), xx2.clone()]),
                ),
                (&action_params[2], BTreeSet::from([y1.clone(), y2.clone()])),
            ])),
        ]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
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
                let mut st = state.clone();
                st.apply_modification(modifications.clone());
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
            (&action_params[0], BTreeSet::from([a.dupe()])),
            (&action_params[1], BTreeSet::from([y.dupe()])),
        ]))]);

        assert_eq!(res, correct_grounding);

        let effects = action.grounded_effect(&res).unwrap();
        let correct_effects = BTreeSet::from([BTreeSet::from([
            ModifyState::Del(p1.values(vec![a.dupe()]).build().unwrap()),
            ModifyState::Add(p2.values(vec![a.dupe(), y.dupe()]).build().unwrap()),
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
        let correct = BTreeSet::from([ParameterGrounding(BTreeMap::from([(
            &action.parameters()[0],
            BTreeSet::from([x.dupe(), y.dupe()]),
        )]))]);

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

        let rs = s.values(vec![x.dupe(), y.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![rs]);

        let res = state.ground_action(&action);
        let expected = BTreeSet::from([ParameterGrounding(BTreeMap::from([(
            &action.parameters()[0],
            BTreeSet::from([x.dupe()]),
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
            BTreeSet::from([x.dupe()]),
        )]))]);

        assert_eq!(res, expected);
    }
}
