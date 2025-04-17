use crate::calculus::{
    evaluation::{Evaluable, EvaluationContext},
    predicate::IsPredicate,
};
use alloc::vec::Vec;

impl<E: Evaluable<P, P>, P: IsPredicate<P>> EvaluationContext<P> for TruthTable<'_, E, P> {
    fn eval(&self, predicate: &P) -> bool {
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
}

#[derive(Debug, Clone)]
pub struct TruthTable<'a, E: Evaluable<P, P>, P: IsPredicate<P>> {
    curr_row: usize,
    formula: &'a E,
    predicates: Vec<&'a P>,
    size: usize,
}

impl<'a, E: Evaluable<P, P>, P: IsPredicate<P>> TruthTable<'a, E, P> {
    pub fn new(formula: &'a E) -> Self {
        let predicates = formula.predicates().collect::<Vec<_>>();

        let num_predicates = predicates.len();

        Self {
            curr_row: 0,
            formula,
            predicates,
            size: 2usize.checked_pow(num_predicates as u32).unwrap(),
        }
    }

    pub fn only_true_rows(self) -> FilteredTruthTable<'a, E, P, true> {
        FilteredTruthTable { inner_iter: self }
    }

    pub fn only_false_rows(self) -> FilteredTruthTable<'a, E, P, false> {
        FilteredTruthTable { inner_iter: self }
    }

    pub fn curr_row(&self) -> usize {
        self.curr_row
    }

    pub fn columns(&self) -> &[&P] {
        &self.predicates
    }
}

impl<E: Evaluable<P, P>, P: IsPredicate<P>> Iterator for TruthTable<'_, E, P> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_row < self.size {
            let ret = self.formula.eval(self);
            self.curr_row += 1;
            Some(ret)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilteredTruthTable<'a, E: Evaluable<P, P>, P: IsPredicate<P>, const F: bool> {
    inner_iter: TruthTable<'a, E, P>,
}

impl<E: Evaluable<P, P>, P: IsPredicate<P>, const F: bool> Iterator
    for FilteredTruthTable<'_, E, P, F>
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
            predicate::{PredicateBuilder, Value},
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
        let f = F::and(vec![F::pred(p.clone()), F::pred(p)]);
        let tt = TruthTable::new(&f);

        assert_eq!(1, tt.predicates.len());
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
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
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
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
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
        let tt = TruthTable::new(&f);

        assert_eq!(3, tt.predicates.len());
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
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, false, true], tt);

        let f = F::and(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]);
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, false, false], tt);

        let f = F::and(vec![F::not(F::pred(p.clone())), F::pred(p1.clone())]);
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, true, false], tt);

        let f = F::or(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

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
        let tt = TruthTable::new(&f).only_true_rows().collect::<Vec<_>>();

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
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 1, 2], tt);

        let f = F::and(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]);
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 2, 3], tt);

        let f = F::or(vec![F::pred(p), F::pred(p1)]);
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0], tt);
    }
}
