use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::*;

use crate::predicate::{Evaluable, Predicate};

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
    fn eval(&self) -> bool {
        self.o.iter().all(Evaluable::eval)
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
    fn eval(&self) -> bool {
        self.o.iter().any(Evaluable::eval)
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
    fn eval(&self) -> bool {
        !self.o.eval()
    }
}

#[derive(Debug, Clone)]
pub enum FormulaMembers {
    And(And<FormulaMembers>),
    Or(Or<FormulaMembers>),
    Not(Not<FormulaMembers>),
    Pred(Predicate),
}

impl FormulaMembers {
    fn predicates(&self) -> Vec<Predicate> {
        match self {
            FormulaMembers::And(and) => and.o.iter().flat_map(FormulaMembers::predicates).collect(),
            FormulaMembers::Or(or) => or.o.iter().flat_map(FormulaMembers::predicates).collect(),
            FormulaMembers::Not(not) => not.o.predicates(),
            FormulaMembers::Pred(p) => vec![p.clone()],
        }
    }
}

impl Evaluable for FormulaMembers {
    fn eval(&self) -> bool {
        match self {
            FormulaMembers::And(and) => and.eval(),
            FormulaMembers::Or(or) => or.eval(),
            FormulaMembers::Not(not) => not.eval(),
            FormulaMembers::Pred(p) => p.eval(),
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
    fn eval(&self) -> bool {
        self.e.eval()
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone)]
pub enum NfMembers {
    Not(Not<Predicate>),
    Pred(Predicate),
}

impl Evaluable for NfMembers {
    fn eval(&self) -> bool {
        match self {
            NfMembers::Not(not) => not.eval(),
            NfMembers::Pred(p) => p.eval(),
        }
    }
}

#[derive(Debug, Clone)]
enum DnfMembers {
    And(And<NfMembers>),
    Prim(NfMembers),
}

impl Evaluable for DnfMembers {
    fn eval(&self) -> bool {
        match self {
            DnfMembers::And(and) => and.eval(),
            DnfMembers::Prim(p) => p.eval(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dnf {
    f: Or<DnfMembers>,
}

impl Evaluable for Dnf {
    fn eval(&self) -> bool {
        self.f.eval()
    }
}

impl Dnf {
    pub fn new(expression: Or<DnfMembers>) -> Self {
        Self { f: expression }
    }
}

impl From<Formula> for Dnf {
    fn from(value: Formula) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum CnfMembers {
    Or(Or<NfMembers>),
    Prim(NfMembers),
}

impl Evaluable for CnfMembers {
    fn eval(&self) -> bool {
        match self {
            CnfMembers::Or(or) => or.eval(),
            CnfMembers::Prim(p) => p.eval(),
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
    fn eval(&self) -> bool {
        self.f.eval()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate::Predicate;
    use string_interner::{backend::BufferBackend, StringInterner};

    // Using these just for the tests
    // IRL this should not be happening
    #[allow(non_snake_case)]
    fn TRUE() -> Predicate {
        Predicate::new("truth", || true)
    }

    #[allow(non_snake_case)]
    fn FALSE() -> Predicate {
        Predicate::new("falsehood", || false)
    }

    #[test]
    fn test_basic_and() {
        let and = And::new(&[TRUE(), TRUE(), TRUE()]);
        assert!(and.eval());

        let and = And::new(&[TRUE(), FALSE(), TRUE()]);
        assert!(!and.eval());
    }

    #[test]
    fn test_basic_or() {
        let or = Or::new(&[TRUE(), TRUE(), TRUE()]);
        assert!(or.eval());

        let or = Or::new(&[TRUE(), FALSE(), FALSE()]);
        assert!(or.eval());

        let or = Or::new(&[FALSE(), FALSE(), FALSE()]);
        assert!(!or.eval());
    }

    #[test]
    fn test_basic_not() {
        let not = Not::new(FALSE());
        assert!(not.eval());

        let not = Not::new(TRUE());
        assert!(!not.eval());
    }

    use FormulaMembers as E;

    #[test]
    fn test_formula() {
        let formula = Formula::new(E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])),
            E::Pred(TRUE()),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])))),
        ])));

        assert!(formula.eval());
    }

    use CnfMembers as C;
    use DnfMembers as D;
    use NfMembers as NF;

    #[test]
    fn test_basic_dnf() {
        let dnf = Dnf::new(Or::new(&[
            D::And(And::new(&[NF::Pred(TRUE()), NF::Pred(FALSE())])),
            D::And(And::new(&[NF::Not(Not::new(FALSE())), NF::Pred(TRUE())])),
            D::Prim(NF::Pred(FALSE())),
        ]));

        assert!(dnf.eval());
    }

    #[test]
    fn test_basic_cnf() {
        let cnf = Cnf::new(And::new(&[
            C::Or(Or::new(&[NF::Pred(TRUE()), NF::Pred(FALSE())])),
            C::Or(Or::new(&[NF::Not(Not::new(FALSE())), NF::Pred(TRUE())])),
            C::Prim(NF::Pred(FALSE())),
        ]));

        assert!(!cnf.eval());
    }

    #[test]
    fn test_expression_get_predicates() {
        let expression = E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])),
            E::Pred(TRUE()),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])))),
        ]));

        assert_eq!(5, expression.predicates().len());
    }

    #[test]
    fn test_formula_to_dnf() {
        let formula = Formula::new(E::And(And::new(&[
            E::Or(Or::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])),
            E::Pred(TRUE()),
            E::Not(Not::new(E::And(And::new(&[
                E::Pred(TRUE()),
                E::Not(Not::new(E::Pred(TRUE()))),
            ])))),
        ])));

        let test_dnf: Dnf = formula.into();

        let dnf = Dnf::new(Or::new(&[]));

        // assert_eq!(dnf, test_dnf);
        unimplemented!()
    }

    #[test]
    fn test_formula_to_cnf() {
        unimplemented!()
    }
}
