use crate::{
    action::Action,
    entity::ObjectHandle,
    evaluation::Evaluable,
    expression::{DnfMembers, Expression, NormalForm, Not, Primitives},
    predicate::{ActionParameterRef, Predicate, PredicateDeclaration, ResolvedPredicate, Value},
};
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;
use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct State {
    predicates: Vec<ResolvedPredicate>,
}

impl State {
    pub fn with_predicates(mut self, predicates: &[ResolvedPredicate]) -> Self {
        self.predicates.extend(predicates.iter().cloned());
        self
    }
}

#[allow(
    clippy::mutable_key_type,
    reason = "See SmartHandle Hash and Ord impls."
)]
fn resolve_pred_as_parameter_map<F>(
    p: &Predicate,
    resolve: F,
) -> BTreeMap<ActionParameterRef, BTreeSet<ObjectHandle>>
where
    F: Fn() -> BTreeSet<ResolvedPredicate>,
{
    let keys = p
        .values()
        .into_iter()
        .enumerate()
        .filter_map(|(i, v)| match v {
            Value::Object(_) => None,
            Value::ActionParam(ap) => Some((i, ap)),
        })
        .collect::<Vec<_>>();

    let mut res: BTreeMap<ActionParameterRef, BTreeSet<_>> = keys
        .iter()
        .map(|(_, k)| (k.clone(), BTreeSet::default()))
        .collect();
    resolve().iter().for_each(|rp| {
        keys.iter().for_each(|&(i, ref k)| {
            res.entry(k.clone()).and_modify(|os| {
                os.insert(rp.values()[i].clone());
            });
        });
    });

    res
}

impl State {
    pub fn is_resolution_of(
        &self,
        predicate: &Predicate,
        resolved_predicate: &ResolvedPredicate,
    ) -> bool {
        if predicate.name() != resolved_predicate.name()
            || predicate.arguments().len() != resolved_predicate.arguments().len()
        {
            return false;
        }

        predicate
            .values()
            .iter()
            .zip(resolved_predicate.values())
            .all(|(av, v)| match av {
                Value::Object(o) => o == v,
                Value::ActionParam(ap) => v.r#type().inherits_or_eq(&ap.r#type),
            })
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolve_predicate(&self, predicate: &Predicate) -> BTreeSet<ResolvedPredicate> {
        // TODO: turn this into a closure to make equality predicate possible
        self.predicates
            .iter()
            .filter(|rp| self.is_resolution_of(predicate, rp))
            .cloned()
            .collect()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolve_negated_predicate(
        &self,
        predicate: &Not<Predicate>,
    ) -> BTreeSet<ResolvedPredicate> {
        let pos_predicate = predicate.predicates()[0].clone();

        let entities_for_param = resolve_pred_as_parameter_map(&pos_predicate, || {
            self.resolve_predicate(&pos_predicate)
        });

        entities_for_param
            .iter()
            // Get all entities for all arguments that don't appear in the current state
            .map(|(r, obj_for_param)| {
                let obj_for_type = r.r#type.get_objects().into_iter().collect::<BTreeSet<_>>();
                obj_for_type
                    .difference(obj_for_param)
                    .cloned()
                    .collect::<Vec<_>>()
            })
            // Build all possible predicates with them
            .multi_cartesian_product()
            .map(|vars| {
                let mut vars = vars.iter();
                let resolution = pos_predicate
                    .values()
                    .iter()
                    .map(|v| match v {
                        Value::Object(o) => o,
                        Value::ActionParam(_) => vars
                            .next()
                            .expect("Resolved parameter array length mismatch"),
                    })
                    .cloned()
                    .collect::<Vec<_>>();
                PredicateDeclaration {
                    name: pos_predicate.name(),
                    arguments: pos_predicate.arguments().to_vec(),
                }
                .as_resolved(&resolution)
            })
            .collect::<BTreeSet<_>>()
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    pub fn resolve_action(
        &self,
        action: Action,
    ) -> BTreeSet<BTreeMap<ActionParameterRef, BTreeSet<ObjectHandle>>> {
        let handle_primitives = |p: &Primitives| match p {
            Primitives::Not(np) => {
                resolve_pred_as_parameter_map(&np.o, || self.resolve_negated_predicate(np))
            }
            Primitives::Pred(p) => resolve_pred_as_parameter_map(p, || self.resolve_predicate(p)),
        };

        action
            .precondition()
            .expression()
            .members()
            .iter()
            .filter_map(|m| match m {
                DnfMembers::And(and) => {
                    let o = and.o.iter().fold(
                        BTreeMap::default(),
                        |mut acc: BTreeMap<ActionParameterRef, BTreeSet<_>>, p| {
                            handle_primitives(p).iter().for_each(|(k, v)| {
                                acc.entry(k.clone())
                                    .and_modify(|s| *s = s.intersection(v).cloned().collect())
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
                DnfMembers::Prim(p) => Some(handle_primitives(p)),
            })
            .collect::<BTreeSet<_>>()
    }
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
        entity::*,
        expression::{And, Dnf, DnfMembers, Formula, FormulaMembers as FM, Primitives as Pr},
        predicate::*,
    };

    #[test]
    fn test_preciate_resolution() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);
        let b = entities.get_or_create_object("b", &t);
        let c = entities.get_or_create_object("c", &t);

        let p = PredicateDeclaration::new("foo", &[&t, &t]);
        let ps = p.as_specific(&[Value::Object(x.clone()), Value::Object(y.clone())]);
        let rp = ps.as_resolved(&[]);
        // Just for the test creating a predicate with the same name
        // to simulate that there migth be several different
        // resolved versions of the same predicate in a state.
        // Normally names are unique, and this predicate should have
        // arguments like [ArgumentValue::ActionParameter],
        // which then get resolved and added to the state over the
        // course of the program.
        let p1 = PredicateDeclaration::new("foo", &[&t, &t])
            .as_specific(&[Value::Object(x.clone()), Value::Object(b.clone())]);
        let rp1 = p1.as_resolved(&[]);

        let p2 = PredicateDeclaration::new("bar", &[&t]);
        let ps2 = p2.as_specific(&[Value::Object(c.clone())]);
        let rp2 = ps2.as_resolved(&[]);

        let state = State::default().with_predicates(&[rp.clone(), rp1.clone(), rp2.clone()]);

        // Expect only one predicate - the one we are resolving,
        // because it is already resolved
        assert_eq!(state.resolve_predicate(&ps), BTreeSet::from([rp.clone()]));

        let ap0 = ActionParameterRef {
            parameter_handle: ParameterHandle { idx: 0 },
            r#type: t.clone(),
        };
        let mut ap1 = ap0.clone();
        ap1.parameter_handle.idx = 1;
        let pp = p.as_specific(&[Value::ActionParam(ap0.clone()), Value::ActionParam(ap1)]);
        // Two predicates, because `rp` and `rp1` are both `foo` and have the same type of arguments
        assert_eq!(
            state.resolve_predicate(&pp),
            BTreeSet::from([rp.clone(), rp1.clone()])
        );

        let pp2 = p2.as_specific(&[Value::ActionParam(ap0.clone())]);
        // Three predicates, because there is one resolved `bar`
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(&pp2)),
            BTreeSet::from([
                p2.as_resolved(&[x.clone()]),
                p2.as_resolved(&[y.clone()]),
                p2.as_resolved(&[b.clone()])
            ])
        );

        // Predicate with no parameters doesn't get resolved
        let p3 = PredicateDeclaration::new("baz", &[]).as_specific(&[]);
        assert_eq!(state.resolve_predicate(&p3), BTreeSet::default());

        // Negated predicate that isn't in a state should get all permutations of values
        let p4 = PredicateDeclaration::new("qux", &[&t]).as_specific(&[Value::ActionParam(ap0)]);
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(&p4)),
            BTreeSet::from([
                p4.as_resolved(&[x]),
                p4.as_resolved(&[y]),
                p4.as_resolved(&[b]),
                p4.as_resolved(&[c]),
            ])
        );
    }

    #[test]
    pub fn test_action_resolution_with_explicit_predicates() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateDeclaration::new("p1", &[&t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2]);
        let p3 = PredicateDeclaration::new("p3", &[&t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    FM::not(&FM::pred(
                        p2.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                    FM::pred(p3.as_specific(&[Value::ActionParam(params[1].clone())])),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x.clone()]);
        let rp3 = p3.as_resolved(&[y.clone()]);
        let state = State::default().with_predicates(&[rp1, rp3]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0].clone(), BTreeSet::from([x.clone()])),
            (action_params[1].clone(), BTreeSet::from([y.clone()])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_action_resolution_with_not_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x = entities.get_or_create_object("x", &t1);
        let y = entities.get_or_create_object("y", &t2);

        let p1 = PredicateDeclaration::new("p1", &[&t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    FM::not(&FM::pred(
                        p2.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x.clone()]);
        let state = State::default().with_predicates(&[rp1]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0].clone(), BTreeSet::from([x.clone()])),
            (action_params[1].clone(), BTreeSet::from([y.clone()])),
        ])]);

        assert_eq!(res, correct_resolution)
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

        let p1 = PredicateDeclaration::new("p1", &[&t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::not(&FM::pred(
                        p1.as_specific(&[Value::ActionParam(params[0].clone())]),
                    )),
                    FM::not(&FM::pred(
                        p2.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1]);
        let rp2 = p2.as_resolved(&[y1]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0].clone(), BTreeSet::from([x2])),
            (action_params[1].clone(), BTreeSet::from([y2])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_impossible_action_resolution() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);

        let p1 = PredicateDeclaration::new("p1", &[&t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::not(&FM::pred(
                        p1.as_specific(&[Value::ActionParam(params[0].clone())]),
                    )),
                    FM::not(&FM::pred(
                        p2.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();

        let rp11 = p1.as_resolved(&[x1]);
        let rp12 = p1.as_resolved(&[x2]);
        let rp21 = p2.as_resolved(&[y1]);
        let state = State::default().with_predicates(&[rp11, rp12, rp21]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0].clone(), BTreeSet::from([])),
            (action_params[1].clone(), BTreeSet::from([y2])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_impossible_resolution_with_partially_defined_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let x1 = entities.get_or_create_object("x1", &t1);
        let x2 = entities.get_or_create_object("x2", &t1);
        let y1 = entities.get_or_create_object("y1", &t2);
        let y2 = entities.get_or_create_object("y2", &t2);
        let y3 = entities.get_or_create_object("y3", &t2);

        let p1 = PredicateDeclaration::new("p1", &[&t1, &t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2, &t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[
                        Value::ActionParam(params[0].clone()),
                        Value::Object(x2.clone()),
                    ])),
                    FM::not(&FM::pred(p2.as_specific(&[
                        Value::Object(y1.clone()),
                        Value::ActionParam(params[1].clone()),
                    ]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[
                        Value::Object(x2.clone()),
                        Value::ActionParam(params[0].clone()),
                    ])),
                    Pr::pred(p2.as_specific(&[
                        Value::ActionParam(params[1].clone()),
                        Value::Object(y1.clone()),
                    ])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1.clone(), x2.clone()]);
        let rp2 = p2.as_resolved(&[y1.clone(), y2.clone()]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0].clone(), BTreeSet::from([x1])),
            (action_params[1].clone(), BTreeSet::from([y1, y3])),
        ])]);

        assert_eq!(res, correct_resolution)
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

        let p1 = PredicateDeclaration::new("p1", &[&t1, &t1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2, &t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Dnf::new(&[
                    DnfMembers::and(&[
                        Pr::pred(p1.as_specific(&[
                            Value::ActionParam(params[0].clone()),
                            Value::Object(x2.clone()),
                        ])),
                        Pr::not(p2.as_specific(&[
                            Value::Object(y1.clone()),
                            Value::ActionParam(params[1].clone()),
                        ])),
                    ]),
                    DnfMembers::prim(&Pr::not(p1.as_specific(&[
                        Value::Object(x2.clone()),
                        Value::ActionParam(params[0].clone()),
                    ]))),
                    DnfMembers::prim(&Pr::pred(p2.as_specific(&[
                        Value::ActionParam(params[1].clone()),
                        Value::Object(y2.clone()).clone(),
                    ]))),
                    DnfMembers::prim(&Pr::pred(p2.as_specific(&[
                        Value::ActionParam(params[1].clone()),
                        Value::Object(y2.clone()),
                    ]))),
                    DnfMembers::and(&[]),
                ])
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[
                        Value::Object(x2.clone()),
                        Value::ActionParam(params[0].clone()),
                    ])),
                    Pr::pred(p2.as_specific(&[
                        Value::ActionParam(params[1].clone()),
                        Value::Object(y1.clone()),
                    ])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1.clone(), x2.clone()]);
        let rp2 = p2.as_resolved(&[y1.clone(), y2.clone()]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            BTreeMap::from([
                (action_params[0].clone(), BTreeSet::from([x1.clone()])),
                (action_params[1].clone(), BTreeSet::from([y1.clone(), y3])),
            ]),
            // Both `x1` and `x2` here, because the initial pred already doesn't
            // resolve because of the Object as the first parameter
            BTreeMap::from([(action_params[0].clone(), BTreeSet::from([x1, x2]))]),
            // This one is here only once, because of the set
            BTreeMap::from([(action_params[1].clone(), BTreeSet::from([y1]))]),
            // We do not expect the 4th element, because it's empty
        ]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_action_resolution_with_subentities() {
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

        let p1 = PredicateDeclaration::new("p1", &[&t1]);
        let pp1 = PredicateDeclaration::new("pp1", &[&tt1]);
        let p2 = PredicateDeclaration::new("p2", &[&t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, tt1, t2])
            .precondition(|params| {
                Dnf::new(&[
                    DnfMembers::prim(&Pr::pred(
                        p1.as_specific(&[Value::ActionParam(params[0].clone())]),
                    )),
                    DnfMembers::prim(&Pr::pred(
                        p1.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                    DnfMembers::prim(&Pr::pred(
                        pp1.as_specific(&[Value::ActionParam(params[1].clone())]),
                    )),
                    DnfMembers::prim(&Pr::pred(
                        p2.as_specific(&[Value::ActionParam(params[2].clone())]),
                    )),
                ])
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0].clone())])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1].clone())])),
                ])
            })
            .build();

        let rp1_t1_1 = p1.as_resolved(&[x1.clone()]);
        let rp1_t1_2 = p1.as_resolved(&[x2.clone()]);
        let rp1_tt1_1 = p1.as_resolved(&[xx1.clone()]);
        let rp1_tt1_2 = p1.as_resolved(&[xx2.clone()]);

        let rpp1_tt1_1 = pp1.as_resolved(&[xx1.clone()]);
        let rpp1_tt1_2 = pp1.as_resolved(&[xx2.clone()]);

        let rp2_t2_1 = p2.as_resolved(&[y1.clone()]);
        let rp2_t2_2 = p2.as_resolved(&[y2.clone()]);
        let state = State::default().with_predicates(&[
            rp1_t1_1, rp1_t1_2, rp1_tt1_1, rp1_tt1_2, rpp1_tt1_1, rpp1_tt1_2, rp2_t2_1, rp2_t2_2,
        ]);

        let action_params = action.parameters().to_owned();
        let res = state.resolve_action(action);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            // Getting both `x`s and `xx`s, because `xx`s of type `tt1`,
            // and the predicate is declared with `t1` which is a supertype of `tt1`
            BTreeMap::from([(
                action_params[0].clone(),
                BTreeSet::from([x1, x2, xx1.clone(), xx2.clone()]),
            )]),
            // This predicate is defined with `t1`, but it was turned into a specific
            // with `tt1`, so it should only resolve with `tt1`
            BTreeMap::from([(
                action_params[1].clone(),
                BTreeSet::from([xx1.clone(), xx2.clone()]),
            )]),
            // This predicate is defined with type `tt1`, which has no subentities,
            // so only values of this type will be resolved
            BTreeMap::from([(action_params[1].clone(), BTreeSet::from([xx1, xx2]))]),
            // Same as above, no subentities
            BTreeMap::from([(action_params[2].clone(), BTreeSet::from([y1, y2]))]),
        ]);

        assert_eq!(res, correct_resolution)
    }
}
