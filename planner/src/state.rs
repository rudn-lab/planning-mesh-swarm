use crate::{
    action::Action,
    evaluation::{ActionResolution, Evaluable, PredicateResolution, ResolutionContext},
    expression::{Expression, NormalForm, Not, Primitives},
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
                Value::ActionParam(ap) => objects
                    .get(*v)
                    .map(|o| {
                        o.r#type == ap.r#type || types.get_subtypes(ap.r#type).contains(&o.r#type)
                    })
                    .is_some(),
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

        let all_objects_for_type = resolve_pred_as_parameter_map(&pos_predicate, || {
            self.resolve_predicate(&pos_predicate, types, objects)
        });

        all_objects_for_type
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
                crate::expression::DnfMembers::And(and) => {
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
                crate::expression::DnfMembers::Prim(p) => Some(handle_primitives(p)),
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
        expression::{And, Formula, FormulaMembers as FM, Primitives as Pr},
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
    pub fn test_simple_action_resolution() {
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

        let mut correct_resolution = BTreeMap::new();
        correct_resolution.insert(action_params[0], {
            let mut set = BTreeSet::new();
            set.insert(x);
            set
        });
        correct_resolution.insert(action_params[1], {
            let mut set = BTreeSet::new();
            set.insert(y);
            set
        });

        let mut correct = BTreeSet::new();
        correct.insert(correct_resolution);

        assert_eq!(res, correct)
    }

    #[test]
    pub fn test_action_resolution() {
        let mut types = TypeCollection::default();
        let t = types.get_or_create("foo");
        let t1 = types.get_or_create("bar");

        let mut objects = ObjectCollection::default();
        let x = objects.get_or_create("x", t);
        let y = objects.get_or_create("y", t);

        let p = PredicateDeclaration::new("foo", &[t]);
        let rp = p.as_resolved(&[x]);
        let p1 = PredicateDeclaration::new("bar", &[t1]);
        let p2 = PredicateDeclaration::new("baz", &[t1]);
        let rp2 = p2.as_resolved(&[y]);

        let _action = ActionBuilder::new("action")
            .parameters(&[t, t1])
            .precondition(|params| {
                Formula::new(FM::and(&[
                    FM::pred(p.as_specific(&[Value::ActionParam(params[0])])),
                    FM::not(&FM::pred(p1.as_specific(&[Value::ActionParam(params[0])]))),
                ]))
            })
            .effect(|params| {
                And::new(&[
                    Pr::not(p.as_specific(&[Value::ActionParam(params[0])])),
                    Pr::pred(p1.as_specific(&[Value::ActionParam(params[1])])),
                ])
            })
            .build();

        let _state = State::default().with_predicates(&[rp, rp2]);
    }
}
