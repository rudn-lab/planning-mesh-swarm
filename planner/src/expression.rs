use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::vec;
use alloc::vec::*;

use crate::predicate::{Evaluable, Predicate};
use crate::state::State;

#[derive(Debug, Clone)]
pub struct And<T: Evaluable> {
    o: Vec<T>,
}

impl<T: Evaluable> And<T> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
        }
    }
}

impl<T: Evaluable> Evaluable for And<T> {
    fn eval(&self, state: &State) -> bool {
        self.o.iter().all(|e| Evaluable::eval(e, &state))
    }
}

#[derive(Debug, Clone)]
pub struct Or<T: Evaluable> {
    o: Vec<T>,
}

impl<T: Evaluable> Or<T> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
        }
    }
}

impl<T: Evaluable> Evaluable for Or<T> {
    fn eval(&self, state: &State) -> bool {
        self.o.iter().any(|e| Evaluable::eval(e, &state))
    }
}

#[derive(Debug, Clone)]
pub struct Not<T: Evaluable> {
    o: Box<T>,
}

impl<T: Evaluable> Not<T> {
    pub fn new(operand: T) -> Self {
        Self {
            o: Box::new(operand),
        }
    }
}

impl<T: Evaluable> Evaluable for Not<T> {
    fn eval(&self, state: &State) -> bool {
        !self.o.eval(&state)
    }
}

#[derive(Debug, Clone)]
pub enum FormulaMembers {
    And(And<FormulaMembers>),
    Or(Or<FormulaMembers>),
    Not(Not<FormulaMembers>),
    Pred(Rc<Predicate>),
}

impl FormulaMembers {
    fn predicates(&self) -> Vec<Rc<Predicate>> {
        match self {
            FormulaMembers::And(and) => and.o.iter().flat_map(FormulaMembers::predicates).collect(),
            FormulaMembers::Or(or) => or.o.iter().flat_map(FormulaMembers::predicates).collect(),
            FormulaMembers::Not(not) => not.o.predicates(),
            FormulaMembers::Pred(p) => vec![Rc::clone(p)],
        }
    }
}

impl Evaluable for FormulaMembers {
    fn eval(&self, state: &State) -> bool {
        match self {
            FormulaMembers::And(and) => and.eval(&state),
            FormulaMembers::Or(or) => or.eval(&state),
            FormulaMembers::Not(not) => not.eval(&state),
            FormulaMembers::Pred(p) => p.eval(&state),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Formula {
    e: FormulaMembers,
}

impl Formula {
    pub fn new(expression: FormulaMembers) -> Self {
        Self { e: expression }
    }
}

impl Evaluable for Formula {
    fn eval(&self, state: &State) -> bool {
        self.e.eval(&state)
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone)]
pub enum NfMembers {
    Not(Not<Rc<Predicate>>),
    Pred(Rc<Predicate>),
}

impl Evaluable for NfMembers {
    fn eval(&self, state: &State) -> bool {
        match self {
            NfMembers::Not(not) => not.eval(&state),
            NfMembers::Pred(p) => p.eval(&state),
        }
    }
}

#[derive(Debug, Clone)]
enum DnfMembers {
    And(And<NfMembers>),
    Prim(NfMembers),
}

impl Evaluable for DnfMembers {
    fn eval(&self, state: &State) -> bool {
        match self {
            DnfMembers::And(and) => and.eval(&state),
            DnfMembers::Prim(p) => p.eval(&state),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dnf {
    f: Or<DnfMembers>,
}

impl Evaluable for Dnf {
    fn eval(&self, state: &State) -> bool {
        self.f.eval(&state)
    }
}

impl Dnf {
    pub fn new(expression: Or<DnfMembers>) -> Self {
        Self { f: expression }
    }
}

impl From<Formula> for Dnf {
    fn from(value: Formula) -> Self {
        let _predicates = value.e.predicates();

        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum CnfMembers {
    Or(Or<NfMembers>),
    Prim(NfMembers),
}

impl Evaluable for CnfMembers {
    fn eval(&self, state: &State) -> bool {
        match self {
            CnfMembers::Or(or) => or.eval(&state),
            CnfMembers::Prim(p) => p.eval(&state),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cnf {
    f: And<CnfMembers>,
}

impl Cnf {
    pub fn new(expression: And<CnfMembers>) -> Self {
        Self { f: expression }
    }
}

impl Evaluable for Cnf {
    fn eval(&self, state: &State) -> bool {
        self.f.eval(&state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate::Predicate;
    use crate::state::State;
    use alloc::rc::Rc;

    #[test]
    fn test_basic_and() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let and = And::new(&[Rc::clone(&t), Rc::clone(&t), Rc::clone(&t)]);
        assert!(and.eval(&state));

        let and = And::new(&[Rc::clone(&t), Rc::clone(&f), Rc::clone(&t)]);
        assert!(!and.eval(&state));
    }

    #[test]
    fn test_basic_or() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let or = Or::new(&[Rc::clone(&t), Rc::clone(&t), Rc::clone(&t)]);
        assert!(or.eval(&state));

        let or = Or::new(&[Rc::clone(&t), Rc::clone(&f), Rc::clone(&f)]);
        assert!(or.eval(&state));

        let or = Or::new(&[Rc::clone(&f), Rc::clone(&f), Rc::clone(&f)]);
        assert!(!or.eval(&state));
    }

    #[test]
    fn test_basic_not() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let not = Not::new(Rc::clone(&f));
        assert!(not.eval(&state));

        let not = Not::new(Rc::clone(&t));
        assert!(!not.eval(&state));
    }

    use FormulaMembers as E;

    #[test]
    fn test_formula() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let formula = Formula::new(E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])),
            E::Pred(Rc::clone(&t)),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])))),
        ])));

        assert!(formula.eval(&state));
    }

    use CnfMembers as C;
    use DnfMembers as D;
    use NfMembers as NF;

    #[test]
    fn test_basic_dnf() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let dnf = Dnf::new(Or::new(&[
            D::And(And::new(&[
                NF::Pred(Rc::clone(&t)),
                NF::Pred(Rc::clone(&f)),
            ])),
            D::And(And::new(&[
                NF::Not(Not::new(Rc::clone(&f))),
                NF::Pred(Rc::clone(&t)),
            ])),
            D::Prim(NF::Pred(Rc::clone(&f))),
        ]));

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_basic_cnf() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let cnf = Cnf::new(And::new(&[
            C::Or(Or::new(&[NF::Pred(Rc::clone(&t)), NF::Pred(Rc::clone(&f))])),
            C::Or(Or::new(&[
                NF::Not(Not::new(Rc::clone(&f))),
                NF::Pred(Rc::clone(&t)),
            ])),
            C::Prim(NF::Pred(Rc::clone(&f))),
        ]));

        assert!(!cnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let t = Rc::new(Predicate::new("truth", &[]));

        let expression = E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])),
            E::Pred(Rc::clone(&t)),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])))),
        ]));

        assert_eq!(5, expression.predicates().len());
    }

    #[test]
    fn test_formula_to_dnf() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let _state = State::with_predicates(&[Rc::clone(&t)]);

        let formula = Formula::new(E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])),
            E::Pred(Rc::clone(&t)),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(Rc::clone(&t)),
                E::Not(Not::new(E::Pred(Rc::clone(&t)))),
            ])))),
        ])));

        let _test_dnf: Dnf = formula.into();
        let _dnf = Dnf::new(Or::new(&[]));

        // assert_eq!(dnf, test_dnf);
        unimplemented!()
    }

    #[test]
    fn test_formula_to_cnf() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let _f = Rc::new(Predicate::new("falsehood", &[]));
        let _state = State::with_predicates(&[Rc::clone(&t)]);

        unimplemented!()
    }
}
