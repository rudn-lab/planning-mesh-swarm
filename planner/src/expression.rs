use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::vec;
use alloc::vec::*;
use itertools::Itertools;

use crate::{
    predicate::{Evaluable, EvaluationContext, Predicate},
    truth_table::TruthTable,
};

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
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.o.iter().all(|e| Evaluable::eval(e, context))
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
            .collect()
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
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.o.iter().any(|e| Evaluable::eval(e, context))
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct Not<T: Evaluable> {
    o: Box<T>,
}

impl<T: Evaluable> Not<T> {
    pub fn new(operand: &T) -> Self {
        Self {
            o: Box::new(operand.clone()),
        }
    }
}

impl<T: Evaluable> Evaluable for Not<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        !self.o.eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.o.predicates()
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
    pub fn and(operands: &[Self]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn or(operands: &[Self]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn not(operand: &Self) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: &Rc<Predicate>) -> Self {
        Self::Pred(Rc::clone(operand))
    }
}

impl Evaluable for FormulaMembers {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            Self::And(and) => and.eval(context),
            Self::Or(or) => or.eval(context),
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        match self {
            Self::And(and) => and
                .o
                .iter()
                .flat_map(Self::predicates)
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
                .collect(),
            Self::Or(or) => {
                or.o.iter()
                    .flat_map(Self::predicates)
                    .sorted()
                    .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
                    .collect()
            }
            Self::Not(not) => not.o.predicates(),
            Self::Pred(p) => vec![Rc::clone(p)],
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
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.e.eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.e.predicates()
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone)]
pub enum NfMembers {
    Not(Not<Rc<Predicate>>),
    Pred(Rc<Predicate>),
}

impl NfMembers {
    pub fn not(operand: &Rc<Predicate>) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: &Rc<Predicate>) -> Self {
        Self::Pred(Rc::clone(operand))
    }
}

impl Evaluable for NfMembers {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        match self {
            Self::Not(not) => not.o.predicates(),
            Self::Pred(p) => vec![Rc::clone(p)],
        }
    }
}

#[derive(Debug, Clone)]
pub enum DnfMembers {
    And(And<NfMembers>),
    Prim(NfMembers),
}

impl DnfMembers {
    pub fn and(operands: &[NfMembers]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn prim(operand: &NfMembers) -> Self {
        Self::Prim(operand.clone())
    }
}

impl Evaluable for DnfMembers {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            Self::And(and) => and.eval(context),
            Self::Prim(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        match self {
            Self::And(and) => and
                .o
                .iter()
                .flat_map(NfMembers::predicates)
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
                .collect(),
            Self::Prim(p) => p.predicates(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dnf {
    f: Or<DnfMembers>,
}

impl Evaluable for Dnf {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.f.predicates()
    }
}

impl Dnf {
    pub fn new(members: &[DnfMembers]) -> Self {
        Self {
            f: Or::new(members),
        }
    }
}

fn map_to_dnf<T: Evaluable>(value: T) -> Dnf {
    let tt = TruthTable::new(&value);
    let predicates = tt.columns();
    let conjunctions = tt
        .only_true_rows()
        .map(|i| {
            DnfMembers::and(
                &(0..predicates.len())
                    .map(move |n| (i >> n) & 1)
                    .enumerate()
                    .map(|(i, v)| {
                        if v == 1 {
                            NfMembers::pred(&predicates[i])
                        } else {
                            NfMembers::not(&predicates[i])
                        }
                    })
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();

    Dnf::new(&conjunctions)
}

/// You cannot just do
/// ```ignore
/// impl<T: Evaluable> From<T> for Dnf {
///     fn from(value: T) -> Self {
///         // code
///     }
/// }
/// ```
/// Because Dnf and Cnf already impl Evaluable,
/// and this conflicts with `impl<T> From<T> for T` in standard library.
/// So we have to implement all of them individually.
/// This macro saves us some writing.
macro_rules! impl_with_map {
    ($from:ty, $g1:ident $(: $t1:ident)? $(, $g:ident $(: $t:ident)?)* => $for:ty, $map:ident) => {
        impl<$g1 $(: $t1)? $(, $g $(: $t)?)*> From<$from> for $for {
            fn from(value: $from) -> Self {
                $map(value)
            }
        }
    };
    ($from:ty => $for:ty, $map:ident) => {
        impl From<$from> for $for {
            fn from(value: $from) -> Self {
                $map(value)
            }
        }
    };
}

impl_with_map!(And<T>, T: Evaluable => Dnf, map_to_dnf);
impl_with_map!(Or<T>, T: Evaluable => Dnf, map_to_dnf);
impl_with_map!(Not<T>, T: Evaluable => Dnf, map_to_dnf);
impl_with_map!(FormulaMembers => Dnf, map_to_dnf);
impl_with_map!(Formula => Dnf, map_to_dnf);
impl_with_map!(NfMembers => Dnf, map_to_dnf);
impl_with_map!(DnfMembers => Dnf, map_to_dnf);
impl_with_map!(CnfMembers => Dnf, map_to_dnf);
impl_with_map!(Cnf => Dnf, map_to_dnf);

#[derive(Debug, Clone)]
pub enum CnfMembers {
    Or(Or<NfMembers>),
    Prim(NfMembers),
}

impl CnfMembers {
    pub fn or(operands: &[NfMembers]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn prim(operand: &NfMembers) -> Self {
        Self::Prim(operand.clone())
    }
}

impl Evaluable for CnfMembers {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            CnfMembers::Or(or) => or.eval(context),
            CnfMembers::Prim(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        match self {
            Self::Or(or) => {
                or.o.iter()
                    .flat_map(NfMembers::predicates)
                    .sorted()
                    .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
                    .collect()
            }
            Self::Prim(p) => p.predicates(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cnf {
    f: And<CnfMembers>,
}

impl Cnf {
    pub fn new(members: &[CnfMembers]) -> Self {
        Self {
            f: And::new(members),
        }
    }
}

impl Evaluable for Cnf {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<Rc<Predicate>> {
        self.f.predicates()
    }
}

fn map_to_cnf<T: Evaluable>(value: T) -> Cnf {
    let tt = TruthTable::new(&value);
    let predicates = tt.columns();
    let disjunctions = tt
        .only_false_rows()
        .map(|i| {
            CnfMembers::or(
                &(0..predicates.len())
                    .map(move |n| (i >> n) & 1)
                    .enumerate()
                    .map(|(i, v)| {
                        if v == 0 {
                            NfMembers::pred(&predicates[i])
                        } else {
                            NfMembers::not(&predicates[i])
                        }
                    })
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();

    Cnf::new(&disjunctions)
}

impl_with_map!(And<T>, T: Evaluable => Cnf, map_to_cnf);
impl_with_map!(Or<T>, T: Evaluable => Cnf, map_to_cnf);
impl_with_map!(Not<T>, T: Evaluable => Cnf, map_to_cnf);
impl_with_map!(FormulaMembers => Cnf, map_to_cnf);
impl_with_map!(Formula => Cnf, map_to_cnf);
impl_with_map!(NfMembers => Cnf, map_to_cnf);
impl_with_map!(DnfMembers => Cnf, map_to_cnf);
impl_with_map!(Dnf => Cnf, map_to_cnf);
impl_with_map!(CnfMembers => Cnf, map_to_cnf);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate::Predicate;
    use crate::r#type::Type;
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

        let not = Not::new(&Rc::clone(&f));
        assert!(not.eval(&state));

        let not = Not::new(&Rc::clone(&t));
        assert!(!not.eval(&state));
    }

    use FormulaMembers as FM;

    #[test]
    fn test_formula() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(&t), FM::not(&FM::pred(&t))]),
            FM::pred(&t),
            FM::not(&FM::and(&[FM::pred(&t), FM::not(&FM::pred(&t))])),
        ]));

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

        let dnf = Dnf::new(&[
            D::and(&[NF::pred(&t), NF::pred(&f)]),
            D::and(&[NF::not(&f), NF::pred(&t)]),
            D::prim(&NF::pred(&f)),
        ]);

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_basic_cnf() {
        let t = Rc::new(Predicate::new("truth", &[]));
        let f = Rc::new(Predicate::new("falsehood", &[]));
        let state = State::with_predicates(&[Rc::clone(&t)]);

        let cnf = Cnf::new(&[
            C::or(&[NF::pred(&t), NF::pred(&f)]),
            C::or(&[NF::not(&f), NF::pred(&t)]),
            C::prim(&NF::pred(&f)),
        ]);

        assert!(!cnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let t = Rc::new(Predicate::new("foo", &[]));

        // All predicates are the smame
        let expression = FM::and(&[
            FM::or(&[FM::pred(&t), FM::not(&FM::pred(&t))]),
            FM::pred(&t),
            FM::not(&FM::and(&[FM::pred(&t), FM::not(&FM::pred(&t))])),
        ]);

        assert_eq!(1, expression.predicates().len());

        // All predicates are unique
        let t = Rc::new(Predicate::new("foo", &[]));
        let t1 = Rc::new(Predicate::new("bar", &[]));
        let t2 = Rc::new(Predicate::new("baz", &[]));
        let t3 = Rc::new(Predicate::new("qux", &[]));
        let t4 = Rc::new(Predicate::new("corge", &[]));

        let expression = FM::and(&[
            FM::or(&[FM::pred(&t), FM::not(&FM::pred(&t1))]),
            FM::pred(&t2),
            FM::not(&FM::and(&[FM::pred(&t3), FM::not(&FM::pred(&t4))])),
        ]);

        assert_eq!(5, expression.predicates().len());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let t = Rc::new(Predicate::new("foo", &[]));
        let t1 = Rc::new(Predicate::new("bar", &[]));
        let t2 = Rc::new(Predicate::new("baz", &[]));

        let expression = FM::and(&[
            FM::or(&[FM::pred(&t), FM::not(&FM::pred(&t1))]),
            FM::pred(&t2),
            FM::not(&FM::and(&[FM::pred(&t1), FM::not(&FM::pred(&t2))])),
        ]);

        assert_eq!(3, expression.predicates().len());
    }

    #[test]
    fn test_formula_to_dnf() {
        let t = Type::new("type");
        let p = Rc::new(Predicate::new("foo", &[("x", t)]));
        let p1 = Rc::new(Predicate::new("bar", &[("x", t)]));
        let p2 = Rc::new(Predicate::new("baz", &[("x", t)]));
        let p3 = Rc::new(Predicate::new("qux", &[("x", t)]));
        let p4 = Rc::new(Predicate::new("corge", &[("x", t)]));

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(&p), FM::not(&FM::pred(&p1))]),
            FM::pred(&p2),
            FM::not(&FM::and(&[FM::pred(&p3), FM::not(&FM::pred(&p4))])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_dnf: Dnf = formula.into();
        let test_tt = TruthTable::new(&test_dnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }

    #[test]
    fn test_formula_to_cnf() {
        let t = Type::new("type");
        let p = Rc::new(Predicate::new("foo", &[("x", t)]));
        let p1 = Rc::new(Predicate::new("bar", &[("x", t)]));
        let p2 = Rc::new(Predicate::new("baz", &[("x", t)]));
        let p3 = Rc::new(Predicate::new("qux", &[("x", t)]));
        let p4 = Rc::new(Predicate::new("corge", &[("x", t)]));

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(&p), FM::not(&FM::pred(&p1))]),
            FM::pred(&p2),
            FM::not(&FM::and(&[FM::pred(&p3), FM::not(&FM::pred(&p4))])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_cnf: Cnf = formula.into();
        let test_tt = TruthTable::new(&test_cnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
