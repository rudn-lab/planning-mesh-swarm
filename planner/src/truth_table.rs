use crate::{
    evaluation::{Evaluable, EvaluationContext},
    predicate::Predicate,
};
use alloc::{boxed::Box, vec::Vec};

impl<E: Evaluable> EvaluationContext for TruthTable<E> {
    fn eval(&self, predicate: Box<dyn Predicate>) -> bool {
        self.columns()
            .iter()
            .enumerate()
            // If the row number is represented in binary,
            // its 1s represent a unique set of predicates
            // for that row that should be true,
            // and 0s -- that should be false.
            // We filter only true ones.
            .filter(|(i, _)| ((self.curr_row() >> i) & 1) == 1)
            .any(|(_, p)| p == &predicate)
    }
}

#[derive(Debug, Clone)]
pub struct TruthTable<T: Evaluable> {
    curr_row: usize,
    formula: T,
    predicates: Vec<Box<dyn Predicate>>,
    size: usize,
}

impl<T: Evaluable> TruthTable<T> {
    pub fn new(formula: &T) -> Self {
        // We are only concerned with predicates
        // that have arguments, because only they can change
        // how the expression is evaluated
        let predicates = formula
            .predicates()
            .into_iter()
            .filter(|p| !p.arguments().is_empty())
            .collect::<Vec<_>>();

        Self {
            curr_row: 0,
            formula: formula.clone(),
            predicates: predicates.to_vec(),
            size: 2usize.checked_pow(predicates.len() as u32).unwrap(),
        }
    }

    pub fn only_true_rows(self) -> FilteredTruthTable<T, true> {
        FilteredTruthTable { inner_iter: self }
    }

    pub fn only_false_rows(self) -> FilteredTruthTable<T, false> {
        FilteredTruthTable { inner_iter: self }
    }

    pub fn curr_row(&self) -> usize {
        self.curr_row
    }

    pub fn columns(&self) -> &[Box<dyn Predicate>] {
        &self.predicates
    }
}

impl<T: Evaluable> Iterator for TruthTable<T> {
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
pub struct FilteredTruthTable<T: Evaluable, const F: bool> {
    inner_iter: TruthTable<T>,
}

impl<T: Evaluable, const F: bool> Iterator for FilteredTruthTable<T, F> {
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
    use super::*;
    use crate::{
        expression::{FormulaMembers as FM, *},
        predicate::Pred,
        r#type::TypeCollection,
    };

    #[test]
    fn test_size() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");

        // Degenerative case, predicates have no arguments
        let p = Pred::new("bar", &[]);
        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p))]));
        let tt = TruthTable::new(&f);

        assert_eq!(0, tt.predicates.len());
        assert_eq!(1, tt.count());

        // Two predicats with arguments
        let p = Pred::new("bar", &[t]);
        let p1 = Pred::new("baz", &[t]);
        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());

        // Same as above, but more variables, which doesn't matter
        let p = Pred::new("bar", &[t, t]);
        let p1 = Pred::new("baz", &[t]);
        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());

        // One predicate with 2 variables, another with 1, another without
        let p = Pred::new("quux", &[t, t]);
        let p1 = Pred::new("corge", &[t]);
        let p2 = Pred::new("grault", &[]);
        let f = Formula::new(FM::and(&[
            FM::pred(Box::new(p)),
            FM::pred(Box::new(p1)),
            FM::pred(Box::new(p2)),
        ]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());
    }

    #[test]
    fn test_validity() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let p = Pred::new("a", &[t]);
        let p1 = Pred::new("b", &[t]);

        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, false, true], tt);

        let f = Formula::new(FM::and(&[
            FM::pred(Box::new(p)),
            FM::not(&FM::pred(Box::new(p1))),
        ]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, false, false], tt);

        let f = Formula::new(FM::and(&[
            FM::not(&FM::pred(Box::new(p))),
            FM::pred(Box::new(p1)),
        ]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, true, false], tt);

        let f = Formula::new(FM::or(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, true, true], tt);
    }

    #[test]
    fn only_true_rows_are_always_true() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let p = Pred::new("a", &[t]);
        let p1 = Pred::new("b", &[t]);

        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f).only_true_rows().collect::<Vec<_>>();

        assert_eq!(vec![3], tt);
    }

    #[test]
    fn only_false_rows_are_always_false() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let p = Pred::new("a", &[t]);
        let p1 = Pred::new("b", &[t]);

        let f = Formula::new(FM::and(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 1, 2], tt);

        let f = Formula::new(FM::and(&[
            FM::pred(Box::new(p)),
            FM::not(&FM::pred(Box::new(p1))),
        ]));
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0, 2, 3], tt);

        let f = Formula::new(FM::or(&[FM::pred(Box::new(p)), FM::pred(Box::new(p1))]));
        let tt = TruthTable::new(&f).only_false_rows().collect::<Vec<_>>();

        assert_eq!(vec![0], tt);
    }
}
