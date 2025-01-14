use crate::predicate::{Evaluable, EvaluationContext, Predicate};
use alloc::{rc::Rc, vec::Vec};

#[derive(Debug, Clone)]
pub struct TruthTable<T: Evaluable> {
    curr_iteration: usize,
    formula: T,
    predicates: Vec<Rc<Predicate>>,
    size: usize,
}

impl<T: Evaluable> TruthTable<T> {
    pub fn new(formula: &T) -> Self {
        // We are only concerned with predicates
        // that have parameters, because only they can change
        // how the expression is evaluated
        let mut predicates = formula
            .predicates()
            .into_iter()
            .filter(|p| !p.params().is_empty())
            .collect::<Vec<_>>();

        predicates.sort();
        predicates.dedup();

        Self {
            curr_iteration: 0,
            formula: formula.clone(),
            predicates: predicates.to_vec(),
            size: 2usize.checked_pow(predicates.len() as u32).unwrap(),
        }
    }

    pub fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.predicates.clone()
    }
}

impl<T: Evaluable> EvaluationContext for TruthTable<T> {
    fn eval(&self, predicate: &Predicate) -> bool {
        (0..self.predicates.len())
            .map(|n| (self.curr_iteration >> n) & 1)
            .enumerate()
            .filter_map(|(i, e)| if e == 1 { Some(i) } else { None })
            .map(|i| Rc::clone(&self.predicates[i]))
            .any(|v| *v == *predicate)
    }
}

impl<T: Evaluable> Iterator for TruthTable<T> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_iteration < self.size {
            let ret = self.formula.eval(self);
            self.curr_iteration += 1;
            Some(ret)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        expression::{FormulaMembers as FM, *},
        predicate::Predicate,
        r#type::Type,
    };

    #[test]
    fn test_size() {
        let t = Type::new("foo");

        // Degenerative case, predicates have no parameters
        let p = Rc::new(Predicate::new("bar", &[]));
        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p)]));
        let tt = TruthTable::new(&f);

        assert_eq!(0, tt.predicates.len());
        assert_eq!(1, tt.count());

        // Two same predicats get deduped
        let p = Rc::new(Predicate::new("bar", &[("x", t)]));
        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p)]));
        let tt = TruthTable::new(&f);

        assert_eq!(1, tt.predicates.len());
        assert_eq!(2, tt.count());

        // Two predicats with parameters
        let p = Rc::new(Predicate::new("bar", &[("x", t)]));
        let p1 = Rc::new(Predicate::new("baz", &[("x", t)]));
        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p1)]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());

        // Same as above, but more variables, which doesn't matter
        let p = Rc::new(Predicate::new("bar", &[("x", t), ("y", t)]));
        let p1 = Rc::new(Predicate::new("baz", &[("x", t)]));
        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p1)]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());

        // One predicate with 2 variables, another with 1, another without
        let p = Rc::new(Predicate::new("quux", &[("x", t), ("y", t)]));
        let p1 = Rc::new(Predicate::new("corge", &[("x", t)]));
        let p2 = Rc::new(Predicate::new("grault", &[]));
        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p1), FM::pred(&p2)]));
        let tt = TruthTable::new(&f);

        assert_eq!(2, tt.predicates.len());
        assert_eq!(4, tt.count());
    }

    #[test]
    fn test_validity() {
        let t = Type::new("foo");
        let p = Rc::new(Predicate::new("a", &[("x", t)]));
        let p1 = Rc::new(Predicate::new("b", &[("x", t)]));

        let f = Formula::new(FM::and(&[FM::pred(&p), FM::pred(&p1)]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, false, true], tt);

        let f = Formula::new(FM::and(&[FM::pred(&p), FM::not(&FM::pred(&p1))]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, false, false], tt);

        let f = Formula::new(FM::and(&[FM::not(&FM::pred(&p)), FM::pred(&p1)]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, false, true, false], tt);

        let f = Formula::new(FM::or(&[FM::pred(&p), FM::pred(&p1)]));
        let tt = TruthTable::new(&f).collect::<Vec<_>>();

        assert_eq!(vec![false, true, true, true], tt);
    }
}
