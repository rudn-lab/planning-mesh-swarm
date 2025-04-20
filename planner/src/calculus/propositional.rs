use crate::{
    action::ActionParameter,
    calculus::{
        predicate::{
            GroundPredicate, IsPredicate, LiftedPredicate, PredicateError, ScopedPredicate,
        },
        truth_table::TruthTable,
        Evaluable, EvaluationContext,
    },
    entity::ObjectHandle,
};

use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
    vec::*,
};
use getset::Getters;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Formula<P: IsPredicate> {
    And(Vec<Formula<P>>),
    Or(Vec<Formula<P>>),
    Not(Box<Formula<P>>),
    Pred(P),
}

impl<P: IsPredicate> Formula<P> {
    pub fn and(operands: Vec<Formula<P>>) -> Self {
        Self::And(operands)
    }

    pub fn or(operands: Vec<Formula<P>>) -> Self {
        Self::Or(operands)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(operand: Formula<P>) -> Self {
        Self::Not(Box::new(operand))
    }

    pub fn pred(pred: P) -> Self {
        Self::Pred(pred)
    }

    pub fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            match self {
                Formula::And(and) => {
                    Box::new(and.iter().flat_map(Self::predicates)) as Box<dyn Iterator<Item = _>>
                }
                Formula::Or(or) => {
                    Box::new(or.iter().flat_map(Self::predicates)) as Box<dyn Iterator<Item = _>>
                }
                Formula::Not(not) => not.predicates(),
                Formula::Pred(p) => Box::new(core::iter::once(p)) as Box<dyn Iterator<Item = _>>,
            }
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

impl Evaluable<GroundPredicate> for Formula<GroundPredicate> {
    fn eval(&self, context: &impl EvaluationContext<GroundPredicate>) -> bool {
        match self {
            Formula::And(and) => and.iter().all(|f| Evaluable::eval(f, context)),
            Formula::Or(or) => or.iter().any(|f| Evaluable::eval(f, context)),
            Formula::Not(not) => !Evaluable::eval(&**not, context),
            Formula::Pred(p) => p.eval(context),
        }
    }
}

/// Predicate or negated predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Primitives<P: IsPredicate> {
    Pred(P),
    Not(P),
}

impl Primitives<LiftedPredicate> {
    pub fn as_scoped(
        &self,
        grounding: &BTreeMap<&ActionParameter, ObjectHandle>,
    ) -> Result<Primitives<ScopedPredicate>, PredicateError> {
        match self {
            Primitives::Pred(p) => p.as_scoped(grounding).map(Primitives::Pred),
            Primitives::Not(not) => not.as_scoped(grounding).map(Primitives::Not),
        }
    }
}

impl<P: IsPredicate> Primitives<P> {
    pub fn inner(&self) -> &P {
        match self {
            Primitives::Pred(p) | Primitives::Not(p) => p,
        }
    }
}

impl Evaluable<GroundPredicate> for Primitives<GroundPredicate> {
    fn eval(&self, context: &impl EvaluationContext<GroundPredicate>) -> bool {
        match self {
            Primitives::Pred(p) => p.eval(context),
            Primitives::Not(not) => !not.eval(context),
        }
    }
}

/// Full DNF.
///
/// In this DNF all predicates appear in each clause either as is or negated.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub(crate) struct Dnf<P: IsPredicate> {
    #[getset(get = "pub")]
    pub(super) clauses: BTreeSet<BTreeSet<Primitives<P>>>,
}

impl Dnf<LiftedPredicate> {
    pub fn as_scoped(
        &self,
        grounding: &BTreeMap<&ActionParameter, ObjectHandle>,
    ) -> Result<Dnf<ScopedPredicate>, PredicateError> {
        self.clauses
            .iter()
            .map(|c| {
                c.iter()
                    .map(|p| p.as_scoped(grounding))
                    .collect::<Result<BTreeSet<_>, _>>()
            })
            .collect::<Result<BTreeSet<_>, _>>()
            .map(|clauses| Dnf { clauses })
    }
}

impl Evaluable<GroundPredicate> for Dnf<GroundPredicate> {
    fn eval(&self, context: &impl EvaluationContext<GroundPredicate>) -> bool {
        self.clauses
            .iter()
            .any(|c| c.iter().all(|p| p.eval(context)))
    }
}

impl<P: IsPredicate> From<Formula<P>> for Dnf<P> {
    fn from(value: Formula<P>) -> Self {
        let tt = TruthTable::new(value);

        let predicates = tt
            .columns()
            .iter()
            .map(|v| (*v).clone())
            .collect::<Vec<_>>();

        let clauses = tt
            .only_true_rows()
            .map(|i| {
                predicates
                    .iter()
                    .enumerate()
                    .map(|(n, p)| {
                        if ((i >> n) & 1) == 1 {
                            Primitives::Pred(p.clone())
                        } else {
                            Primitives::Not(p.clone())
                        }
                    })
                    .collect::<BTreeSet<_>>()
            })
            .collect::<BTreeSet<_>>();

        Dnf { clauses }
    }
}

impl<P: IsPredicate> From<Dnf<P>> for Formula<P> {
    fn from(value: Dnf<P>) -> Self {
        Formula::Or(
            value
                .clauses
                .into_iter()
                .map(|c| {
                    Formula::And(
                        c.into_iter()
                            .map(|p| match p {
                                Primitives::Pred(p) => Formula::Pred(p),
                                Primitives::Not(not) => Formula::Not(Box::new(Formula::Pred(not))),
                            })
                            .collect_vec(),
                    )
                })
                .collect_vec(),
        )
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{
        calculus::predicate::{LiftedPredicate, PredicateBuilder, Value},
        entity::{EntityStorage, ObjectStorage, TypeStorage},
        state::State,
    };
    use gazebo::dupe::Dupe;

    use Formula as F;
    use Primitives as NF;

    #[test]
    fn test_basic_and() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let and = F::and(vec![
            F::pred(t.clone()),
            F::pred(t.clone()),
            F::pred(t.clone()),
        ]);
        assert!(and.eval(&state));

        let and = F::and(vec![
            F::pred(t.clone()),
            F::pred(f.clone()),
            F::pred(t.clone()),
        ]);
        assert!(!and.eval(&state));
    }

    #[test]
    fn test_basic_or() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let or = F::or(vec![
            F::pred(t.clone()),
            F::pred(t.clone()),
            F::pred(t.clone()),
        ]);
        assert!(or.eval(&state));

        let or = F::or(vec![
            F::pred(t.clone()),
            F::pred(f.clone()),
            F::pred(f.clone()),
        ]);
        assert!(or.eval(&state));

        let or = F::or(vec![
            F::pred(f.clone()),
            F::pred(f.clone()),
            F::pred(f.clone()),
        ]);
        assert!(!or.eval(&state));
    }

    #[test]
    fn test_basic_not() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let not = F::not(F::pred(f.clone()));
        assert!(not.eval(&state));

        let not = F::not(F::pred(t.clone()));
        assert!(!not.eval(&state));
    }

    #[test]
    fn test_formula() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let formula = F::and(vec![
            F::or(vec![F::pred(t.clone()), F::not(F::pred(t.clone()))]),
            F::pred(t.clone()),
            F::not(F::and(vec![F::pred(t.clone()), F::not(F::pred(t.clone()))])),
        ]);

        assert!(formula.eval(&state));
    }

    #[test]
    fn test_basic_dnf() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let dnf = Dnf {
            clauses: BTreeSet::from([
                BTreeSet::from([NF::Pred(t.clone()), NF::Pred(f.clone())]),
                BTreeSet::from([NF::Not(f.clone()), NF::Pred(t.clone())]),
                // Strictly speaking, this is incorrect as [Dnf] should be a full dnf
                BTreeSet::from([NF::Pred(f.clone())]),
            ]),
        };

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();

        // All predicates are the same
        let expression: F<LiftedPredicate> = F::and(vec![
            F::or(vec![F::pred(p.clone()), F::not(F::pred(p.clone()))]),
            F::pred(p.clone()),
            F::not(F::and(vec![F::pred(p.clone()), F::not(F::pred(p.clone()))])),
        ]);

        assert_eq!(1, expression.predicates().count());

        // All predicates are unique
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();

        let expression: F<LiftedPredicate> = F::and(vec![
            F::or(vec![F::pred(p), F::not(F::pred(p1))]),
            F::pred(p2),
            F::not(F::and(vec![F::pred(p3), F::not(F::pred(p4))])),
        ]);

        assert_eq!(5, expression.predicates().count());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();

        let expression: F<LiftedPredicate> = F::and(vec![
            F::or(vec![F::pred(p), F::not(F::pred(p1.clone()))]),
            F::pred(p2.clone()),
            F::not(F::and(vec![F::pred(p1), F::not(F::pred(p2))])),
        ]);

        assert_eq!(3, expression.predicates().count());
    }

    #[test]
    fn test_formula_to_dnf() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let a = entities.get_or_create_object("a", &t);
        let b = entities.get_or_create_object("b", &t);
        let c = entities.get_or_create_object("c", &t);
        let d = entities.get_or_create_object("d", &t);
        let e = entities.get_or_create_object("e", &t);

        let p = PredicateBuilder::new("foo")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&a)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&b)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&c)])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&d)])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&e)])
            .build()
            .unwrap();

        let formula = F::and(vec![
            F::or(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]),
            F::pred(p2.clone()),
            F::not(F::and(vec![
                F::pred(p3.clone()),
                F::not(F::pred(p4.clone())),
            ])),
        ]);

        let test_dnf: Dnf<_> = formula.into();
        let correct_dnf = Dnf {
            clauses: BTreeSet::from([
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Pred(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Pred(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Pred(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Pred(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Not(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Pred(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Pred(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Not(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Not(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Pred(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Not(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Pred(p4.clone()),
                ]),
                BTreeSet::from([
                    Primitives::Not(p.clone()),
                    Primitives::Not(p1.clone()),
                    Primitives::Pred(p2.clone()),
                    Primitives::Not(p3.clone()),
                    Primitives::Not(p4.clone()),
                ]),
            ]),
        };

        assert_eq!(test_dnf, correct_dnf);
    }
}
