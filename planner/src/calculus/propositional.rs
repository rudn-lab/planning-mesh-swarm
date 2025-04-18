use crate::{
    calculus::{
        evaluation::{Evaluable, EvaluationContext},
        predicate::IsPredicate,
    },
    truth_table::TruthTable,
};

use alloc::{boxed::Box, collections::BTreeSet, vec::*};
use getset::Getters;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Formula<P: IsPredicate<P>> {
    And(Vec<Formula<P>>),
    Or(Vec<Formula<P>>),
    Not(Box<Formula<P>>),
    Pred(P),
}

impl<P: IsPredicate<P>> Formula<P> {
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
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Formula<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Formula::And(and) => and.iter().all(|f| f.eval(context)),
            Formula::Or(or) => or.iter().any(|f| f.eval(context)),
            Formula::Not(not) => !not.eval(context),
            Formula::Pred(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
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

/// Predicate or negated predicate.
///
/// Appears in many normal forms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Primitives<P: IsPredicate<P>> {
    Pred(P),
    Not(P),
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Primitives<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::Pred(p) => p.eval(context),
            Self::Not(not) => !not.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        match self {
            Self::Pred(p) => Box::new(core::iter::once(p)) as Box<dyn Iterator<Item = _>>,
            Self::Not(not) => not.predicates(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum DnfClause<P: IsPredicate<P>> {
    And(BTreeSet<Primitives<P>>),
    #[allow(dead_code, reason = "This is used in tests.")]
    Prim(Primitives<P>),
}

impl<P: IsPredicate<P>> Evaluable<P, P> for DnfClause<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::And(and) => and.iter().all(|p| p.eval(context)),
            Self::Prim(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(match self {
            Self::And(and) => Box::new(
                and.iter()
                    .flat_map(Primitives::predicates)
                    .sorted()
                    .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
            ) as Box<dyn Iterator<Item = _>>,
            Self::Prim(p) => p.predicates(),
        })
    }
}

/// Full DNF.
///
/// In this DNF all predicates appear in each clause either as is or negated.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub(crate) struct Dnf<P: IsPredicate<P>> {
    pub(crate) clauses: BTreeSet<DnfClause<P>>,
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Dnf<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.clauses.iter().any(|c| c.eval(context))
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            self.clauses
                .iter()
                .flat_map(|c| c.predicates())
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

impl<P: IsPredicate<P>> From<Formula<P>> for Dnf<P> {
    fn from(value: Formula<P>) -> Self {
        let tt = TruthTable::new(&value);

        let predicates = tt
            .columns()
            .iter()
            .map(|v| (*v).clone())
            .collect::<Vec<_>>();

        let clauses = tt
            .only_true_rows()
            .map(|i| {
                DnfClause::And(
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
                        .collect::<BTreeSet<_>>(),
                )
            })
            .collect::<BTreeSet<_>>();

        Dnf { clauses }
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
    use alloc::vec::Vec;
    use gazebo::dupe::Dupe;

    use DnfClause as D;
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
                D::And(BTreeSet::from([NF::Pred(t.clone()), NF::Pred(f.clone())])),
                D::And(BTreeSet::from([NF::Not(f.clone()), NF::Pred(t.clone())])),
                D::Prim(NF::Pred(f.clone())),
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

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_dnf: Dnf<_> = formula.into();
        let test_tt = TruthTable::new(&test_dnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
