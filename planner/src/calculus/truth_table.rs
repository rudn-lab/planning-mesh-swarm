use crate::calculus::{
    predicate::IsPredicate,
    propositional::{Formula, Propositional},
};
use alloc::vec::Vec;

#[derive(Debug, Clone)]
pub(super) struct TruthTable<P>
where
    P: IsPredicate,
{
    curr_row: usize,
    formula: Formula<P>,
    size: usize,
}

impl<P> TruthTable<P>
where
    P: IsPredicate,
{
    pub fn new<F: Into<Formula<P>>>(formula: F) -> Self {
        let formula: Formula<P> = formula.into();
        let predicates = formula.predicates().collect::<Vec<_>>();

        let num_predicates = predicates.len();

        Self {
            curr_row: 0,
            formula,
            size: 2usize.checked_pow(num_predicates as u32).unwrap(),
        }
    }

    pub fn only_true_rows(self) -> FilteredTruthTable<P, true> {
        FilteredTruthTable { inner_iter: self }
    }

    #[allow(dead_code, reason = "Might be useful later.")]
    pub fn only_false_rows(self) -> FilteredTruthTable<P, false> {
        FilteredTruthTable { inner_iter: self }
    }

    pub fn curr_row(&self) -> usize {
        self.curr_row
    }

    pub fn columns(&self) -> Vec<&P> {
        self.formula.predicates().collect()
    }

    fn eval_predicate(&self, predicate: &P) -> bool {
        self.columns()
            .iter()
            .enumerate()
            // If the row number is represented in binary,
            // its 1s represent a unique set of predicates
            // for that row that should be true,
            // and 0s -- that should be false.
            // We filter only true ones.
            .filter(|(i, _)| ((self.curr_row() >> i) & 1) == 1)
            .any(|(_, &p)| p == predicate)
    }

    fn eval_formula(&self, formula: &Formula<P>) -> bool {
        match formula {
            Formula::And(and) => and.iter().all(|e| self.eval_formula(e)),
            Formula::Or(or) => or.iter().any(|e| self.eval_formula(e)),
            Formula::Not(not) => !self.eval_formula(not),
            Formula::Pred(p) => self.eval_predicate(p),
        }
    }
}

impl<P> Iterator for TruthTable<P>
where
    P: IsPredicate,
{
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_row < self.size {
            let ret = self.eval_formula(&self.formula);
            self.curr_row += 1;
            Some(ret)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilteredTruthTable<P, const F: bool>
where
    P: IsPredicate,
{
    inner_iter: TruthTable<P>,
}

impl<P, const F: bool> Iterator for FilteredTruthTable<P, F>
where
    P: IsPredicate,
{
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner_iter.next() {
            Some(v) => {
                if F == v {
                    Some(self.inner_iter.curr_row - 1)
                } else {
                    self.next()
                }
            }
            None => None,
        }
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use gazebo::dupe::Dupe;

    use super::*;
    use crate::{
        calculus::{
            predicate::{LiftedPredicate, PredicateBuilder, Value},
            propositional::Formula as F,
        },
        entity::{EntityStorage, ObjectStorage, TypeStorage},
    };

    #[test]
    fn test_size() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let x = entities.get_or_create_object("x", &t);
        let xx = entities.get_or_create_object("xx", &t);
        let y = entities.get_or_create_object("y", &t);

        // Even if a predicate has no arguments
        // it still might be checked by action precondition
        // like a flag
        let p = PredicateBuilder::new("bar")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f: F<LiftedPredicate> = F::and(vec![F::pred(p.clone()), F::pred(p)]);
        let tt = TruthTable::new(f);

        assert_eq!(1, tt.columns().len());
        assert_eq!(2, tt.count());

        // Two predicats with arguments
        let p = PredicateBuilder::new("bar")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("baz")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();
        let f = F::and(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(f);

        assert_eq!(2, tt.columns().len());
        assert_eq!(4, tt.count());

        // Same as above, but more variables, which doesn't matter
        let p = PredicateBuilder::new("bar")
            .arguments(vec![t.dupe(), t.dupe()])
            .values(vec![Value::object(&x), Value::object(&xx)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("baz")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();
        let f = F::and(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(f);

        assert_eq!(2, tt.columns().len());
        assert_eq!(4, tt.count());

        // One predicate with 2 variables, another with 1, another without
        let p = PredicateBuilder::new("quix")
            .arguments(vec![t.dupe(), t.dupe()])
            .values(vec![Value::object(&x), Value::object(&xx)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("corge")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("grault")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let f = F::and(vec![F::pred(p), F::pred(p1), F::pred(p2)]);
        let tt = TruthTable::new(f);

        assert_eq!(3, tt.columns().len());
        assert_eq!(8, tt.count());
    }

    #[test]
    fn test_validity() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);

        let p = PredicateBuilder::new("a")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("b")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();

        let f = F::and(vec![F::pred(p.clone()), F::pred(p1.clone())]);
        let tt = TruthTable::new(f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, false, true], tt);

        let f = F::and(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]);
        let tt = TruthTable::new(f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, false, false], tt);

        let f = F::and(vec![F::not(F::pred(p.clone())), F::pred(p1.clone())]);
        let tt = TruthTable::new(f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, true, false], tt);

        let f = F::or(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, true, true], tt);
    }

    #[test]
    fn only_true_rows_are_always_true() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);

        let p = PredicateBuilder::new("a")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("b")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();

        let f = F::and(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(f).only_true_rows().collect::<Vec<_>>();

        assert_eq!(vec![3], tt);
    }

    #[test]
    fn only_false_rows_are_always_false() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);

        let p = PredicateBuilder::new("a")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("a")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();

        let f = F::and(vec![F::pred(p.clone()), F::pred(p1.clone())]);
        let tt = TruthTable::new(f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 1, 2], tt);

        let f = F::and(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]);
        let tt = TruthTable::new(f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 2, 3], tt);

        let f = F::or(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0], tt);
    }
}
