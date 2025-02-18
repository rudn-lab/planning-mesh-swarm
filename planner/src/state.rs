use crate::{
    action::Action,
    evaluation::{ActionResolution, Evaluable, PredicateResolution, ResolutionContext},
    expression::{DnfMembers, Expression, NormalForm, Not, Primitives},
    object::{ObjectCollection, ObjectHandle},
    predicate::{ActionParameterRef, Predicate, PredicateDeclaration, ResolvedPredicate, Value},
    r#type::TypeCollection,
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

fn resolve_pred_as_parameter_map<F>(
    p: &Predicate,
    resolve: F,
) -> BTreeMap<ActionParameterRef, BTreeSet<ObjectHandle>>
where
    F: Fn() -> PredicateResolution,
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
        .map(|(_, k)| (*k, BTreeSet::default()))
        .collect();
    resolve().iter().for_each(|rp| {
        keys.iter().for_each(|&(i, k)| {
            res.entry(k).and_modify(|os| {
                os.insert(rp.values()[i]);
            });
        });
    });

    res
}

impl ResolutionContext for State {
    fn is_resolution_of(
        &self,
        predicate: &Predicate,
        resolved_predicate: &ResolvedPredicate,
        types: &TypeCollection,
        objects: &ObjectCollection,
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
                Value::ActionParam(ap) => objects.get(*v).is_some_and(|o| {
                    o.r#type == ap.r#type || types.get_subtypes(ap.r#type).contains(&o.r#type)
                }),
            })
    }

    fn resolve_predicate(
        &self,
        predicate: &Predicate,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> PredicateResolution {
        self.predicates
            .iter()
            .filter(|rp| self.is_resolution_of(predicate, rp, types, objects))
            .cloned()
            .collect()
    }

    fn resolve_negated_predicate(
        &self,
        predicate: &Not<Predicate>,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> PredicateResolution {
        let pos_predicate = predicate.predicates()[0].clone();

        let objects_for_param = resolve_pred_as_parameter_map(&pos_predicate, || {
            self.resolve_predicate(&pos_predicate, types, objects)
        });

        objects_for_param
            .iter()
            // Get all objects for all arguments that don't appear in the current state
            .map(|(r, obj)| {
                let all_objects_for_type = objects
                    .get_by_type(&r.r#type, types)
                    .iter()
                    .copied()
                    .collect::<BTreeSet<_>>();
                all_objects_for_type
                    .difference(obj)
                    .copied()
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
                    .copied()
                    .collect::<Vec<_>>();
                PredicateDeclaration {
                    name: pos_predicate.name(),
                    arguments: pos_predicate.arguments().to_vec(),
                }
                .as_resolved(&resolution)
            })
            .collect::<BTreeSet<_>>()
    }

    fn resolve_action(
        &self,
        action: Action,
        types: &TypeCollection,
        objects: &ObjectCollection,
    ) -> ActionResolution {
        let handle_primitives = |p: &Primitives| match p {
            Primitives::Not(np) => resolve_pred_as_parameter_map(&np.o, || {
                self.resolve_negated_predicate(np, types, objects)
            }),
            Primitives::Pred(p) => {
                resolve_pred_as_parameter_map(p, || self.resolve_predicate(p, types, objects))
            }
        };

        action
            .precondition
            .expression()
            .members()
            .iter()
            .filter_map(|m| match m {
                DnfMembers::And(and) => {
                    let o = and.o.iter().fold(
                        BTreeMap::default(),
                        |mut acc: BTreeMap<ActionParameterRef, BTreeSet<_>>, p| {
                            handle_primitives(p).iter().for_each(|(k, v)| {
                                acc.entry(*k)
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

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{
        action::ActionBuilder,
        expression::{And, Dnf, DnfMembers, Formula, FormulaMembers as FM, Primitives as Pr},
        object::ObjectCollection,
        predicate::*,
        r#type::*,
    };

    #[test]
    fn test_preciate_resolution() {
        let mut types = TypeCollection::default();
        let t = types.get_or_create("t");

        let mut objects = ObjectCollection::default();
        let x = objects.get_or_create("x", t);
        let y = objects.get_or_create("y", t);
        let b = objects.get_or_create("b", t);
        let c = objects.get_or_create("c", t);

        let p = PredicateDeclaration::new("foo", &[t, t]);
        let ps = p.as_specific(&[Value::Object(x), Value::Object(y)]);
        let rp = ps.as_resolved(&[]);
        // Just for the test creating a predicate with the same name
        // to simulate that there migth be several different
        // resolved versions of the same predicate in a state.
        // Normally names are unique, and this predicate should have
        // arguments like [ArgumentValue::ActionParameter],
        // which then get resolved and added to the state over the
        // course of the program.
        let p1 = PredicateDeclaration::new("foo", &[t, t])
            .as_specific(&[Value::Object(x), Value::Object(b)]);
        let rp1 = p1.as_resolved(&[]);

        let p2 = PredicateDeclaration::new("bar", &[t]);
        let ps2 = p2.as_specific(&[Value::Object(c)]);
        let rp2 = ps2.as_resolved(&[]);

        let state = State::default().with_predicates(&[rp.clone(), rp1.clone(), rp2.clone()]);

        // Expect only one predicate - the one we are resolving,
        // because it is already resolved
        assert_eq!(
            state.resolve_predicate(&ps, &types, &objects),
            BTreeSet::from([rp.clone()])
        );

        let ap0 = ActionParameterRef {
            parameter_handle: ParameterHandle { idx: 0 },
            r#type: t,
        };
        let mut ap1 = ap0;
        ap1.parameter_handle.idx = 1;
        let pp = p.as_specific(&[Value::ActionParam(ap0), Value::ActionParam(ap1)]);
        // Two predicates, because `rp` and `rp1` are both `foo` and have the same type of arguments
        assert_eq!(
            state.resolve_predicate(&pp, &types, &objects),
            BTreeSet::from([rp.clone(), rp1.clone()])
        );

        let pp2 = p2.as_specific(&[Value::ActionParam(ap0)]);
        // Three predicates, because there is one resolved `bar`
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(&pp2), &types, &objects),
            BTreeSet::from([
                p2.as_resolved(&[x]),
                p2.as_resolved(&[y]),
                p2.as_resolved(&[b])
            ])
        );

        // Predicate with no parameters doesn't get resolved
        let p3 = PredicateDeclaration::new("baz", &[]).as_specific(&[]);
        assert_eq!(
            state.resolve_predicate(&p3, &types, &objects),
            BTreeSet::default()
        );

        // Negated predicate that isn't in a state should get all permutations of values
        let p4 = PredicateDeclaration::new("qux", &[t]).as_specific(&[Value::ActionParam(ap0)]);
        assert_eq!(
            state.resolve_negated_predicate(&Not::new(&p4), &types, &objects),
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
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x = objects.get_or_create("x", t1);
        let y = objects.get_or_create("y", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2]);
        let p3 = PredicateDeclaration::new("p3", &[t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[Value::ActionParam(params[0])])),
                    FM::not(&FM::pred(p2.as_specific(&[Value::ActionParam(params[1])]))),
                    FM::pred(p3.as_specific(&[Value::ActionParam(params[1])])),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x]);
        let rp3 = p3.as_resolved(&[y]);
        let state = State::default().with_predicates(&[rp1, rp3]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0], BTreeSet::from([x])),
            (action_params[1], BTreeSet::from([y])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_action_resolution_with_not_predicate() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x = objects.get_or_create("x", t1);
        let y = objects.get_or_create("y", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[Value::ActionParam(params[0])])),
                    FM::not(&FM::pred(p2.as_specific(&[Value::ActionParam(params[1])]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x]);
        let state = State::default().with_predicates(&[rp1]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0], BTreeSet::from([x])),
            (action_params[1], BTreeSet::from([y])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_action_resolution_with_only_negated() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x1 = objects.get_or_create("x1", t1);
        let x2 = objects.get_or_create("x2", t1);
        let y1 = objects.get_or_create("y1", t2);
        let y2 = objects.get_or_create("y2", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::not(&FM::pred(p1.as_specific(&[Value::ActionParam(params[0])]))),
                    FM::not(&FM::pred(p2.as_specific(&[Value::ActionParam(params[1])]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1]);
        let rp2 = p2.as_resolved(&[y1]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0], BTreeSet::from([x2])),
            (action_params[1], BTreeSet::from([y2])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_impossible_action_resolution() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x1 = objects.get_or_create("x1", t1);
        let x2 = objects.get_or_create("x2", t1);
        let y1 = objects.get_or_create("y1", t2);
        let y2 = objects.get_or_create("y2", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::not(&FM::pred(p1.as_specific(&[Value::ActionParam(params[0])]))),
                    FM::not(&FM::pred(p2.as_specific(&[Value::ActionParam(params[1])]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let rp11 = p1.as_resolved(&[x1]);
        let rp12 = p1.as_resolved(&[x2]);
        let rp21 = p2.as_resolved(&[y1]);
        let state = State::default().with_predicates(&[rp11, rp12, rp21]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0], BTreeSet::from([])),
            (action_params[1], BTreeSet::from([y2])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_impossible_resolution_with_partially_defined_predicate() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x1 = objects.get_or_create("x1", t1);
        let x2 = objects.get_or_create("x2", t1);
        let y1 = objects.get_or_create("y1", t2);
        let y2 = objects.get_or_create("y2", t2);
        let y3 = objects.get_or_create("y3", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1, t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2, t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p1.as_specific(&[Value::ActionParam(params[0]), Value::Object(x2)])),
                    FM::not(&FM::pred(p2.as_specific(&[
                        Value::Object(y1),
                        Value::ActionParam(params[1]),
                    ]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::Object(x2), Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1]), Value::Object(y1)])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1, x2]);
        let rp2 = p2.as_resolved(&[y1, y2]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([BTreeMap::from([
            (action_params[0], BTreeSet::from([x1])),
            (action_params[1], BTreeSet::from([y1, y3])),
        ])]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_with_mixed_dnf() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x1 = objects.get_or_create("x1", t1);
        let x2 = objects.get_or_create("x2", t1);
        let y1 = objects.get_or_create("y1", t2);
        let y2 = objects.get_or_create("y2", t2);
        let y3 = objects.get_or_create("y3", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1, t1]);
        let p2 = PredicateDeclaration::new("p2", &[t2, t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, t2])
            .precondition(|params| {
                Dnf::new(&[
                    DnfMembers::and(&[
                        Pr::pred(
                            p1.as_specific(&[Value::ActionParam(params[0]), Value::Object(x2)]),
                        ),
                        Pr::not(
                            p2.as_specific(&[Value::Object(y1), Value::ActionParam(params[1])]),
                        ),
                    ]),
                    DnfMembers::prim(&Pr::not(
                        p1.as_specific(&[Value::Object(x2), Value::ActionParam(params[0])]),
                    )),
                    DnfMembers::prim(&Pr::pred(
                        p2.as_specific(&[Value::ActionParam(params[1]), Value::Object(y2)]),
                    )),
                    DnfMembers::prim(&Pr::pred(
                        p2.as_specific(&[Value::ActionParam(params[1]), Value::Object(y2)]),
                    )),
                    DnfMembers::and(&[]),
                ])
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::Object(x2), Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1]), Value::Object(y1)])),
                ])
            })
            .build();

        let rp1 = p1.as_resolved(&[x1, x2]);
        let rp2 = p2.as_resolved(&[y1, y2]);
        let state = State::default().with_predicates(&[rp1, rp2]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            BTreeMap::from([
                (action_params[0], BTreeSet::from([x1])),
                (action_params[1], BTreeSet::from([y1, y3])),
            ]),
            // Both `x1` and `x2` here, because the initial pred already doesn't
            // resolve because of the Object as the first parameter
            BTreeMap::from([(action_params[0], BTreeSet::from([x1, x2]))]),
            // This one is here only once, because of the set
            BTreeMap::from([(action_params[1], BTreeSet::from([y1]))]),
            // We do not expect the 4th element, because it's empty
        ]);

        assert_eq!(res, correct_resolution)
    }

    #[test]
    pub fn test_action_resolution_with_subtypes() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let tt1 = types.get_or_create("tt1");
        types.create_inheritance(tt1, t1).unwrap();
        let t2 = types.get_or_create("t2");

        let mut objects = ObjectCollection::default();
        let x1 = objects.get_or_create("x1", t1);
        let x2 = objects.get_or_create("x2", t1);
        let xx1 = objects.get_or_create("xx1", tt1);
        let xx2 = objects.get_or_create("xx2", tt1);
        let y1 = objects.get_or_create("y1", t2);
        let y2 = objects.get_or_create("y2", t2);

        let p1 = PredicateDeclaration::new("p1", &[t1]);
        let pp1 = PredicateDeclaration::new("pp1", &[tt1]);
        let p2 = PredicateDeclaration::new("p2", &[t2]);

        let action = ActionBuilder::new("action")
            .parameters(&[t1, tt1, t2])
            .precondition(|params| {
                Dnf::new(&[
                    DnfMembers::prim(&Pr::pred(p1.as_specific(&[Value::ActionParam(params[0])]))),
                    DnfMembers::prim(&Pr::pred(p1.as_specific(&[Value::ActionParam(params[1])]))),
                    DnfMembers::prim(&Pr::pred(pp1.as_specific(&[Value::ActionParam(params[1])]))),
                    DnfMembers::prim(&Pr::pred(p2.as_specific(&[Value::ActionParam(params[2])]))),
                ])
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p1.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p2.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let rp1_t1_1 = p1.as_resolved(&[x1]);
        let rp1_t1_2 = p1.as_resolved(&[x2]);
        let rp1_tt1_1 = p1.as_resolved(&[xx1]);
        let rp1_tt1_2 = p1.as_resolved(&[xx2]);

        let rpp1_tt1_1 = pp1.as_resolved(&[xx1]);
        let rpp1_tt1_2 = pp1.as_resolved(&[xx2]);

        let rp2_t2_1 = p2.as_resolved(&[y1]);
        let rp2_t2_2 = p2.as_resolved(&[y2]);
        let state = State::default().with_predicates(&[
            rp1_t1_1, rp1_t1_2, rp1_tt1_1, rp1_tt1_2, rpp1_tt1_1, rpp1_tt1_2, rp2_t2_1, rp2_t2_2,
        ]);

        let action_params = action.parameters.to_owned();
        let res = state.resolve_action(action, &types, &objects);
        assert!(!res.is_empty());

        let correct_resolution = BTreeSet::from([
            // Getting both `x`s and `xx`s, because `xx`s of type `tt1`,
            // and the predicate is declared with `t1` which is a supertype of `tt1`
            BTreeMap::from([(action_params[0], BTreeSet::from([x1, x2, xx1, xx2]))]),
            // This predicate is defined with `t1`, but it was turned into a specific
            // with `tt1`, so it should only resolve with `tt1`
            BTreeMap::from([(action_params[1], BTreeSet::from([xx1, xx2]))]),
            // This predicate is defined with type `tt1`, which has no subtypes,
            // so only values of this type will be resolved
            BTreeMap::from([(action_params[1], BTreeSet::from([xx1, xx2]))]),
            // Same as above, no subtypes
            BTreeMap::from([(action_params[2], BTreeSet::from([y1, y2]))]),
        ]);

        assert_eq!(res, correct_resolution)
    }
}
