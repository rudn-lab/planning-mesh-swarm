use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::*;
use itertools::Itertools;

use crate::{
    evaluation::{Evaluable, EvaluationContext},
    predicate::Predicate,
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup()
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup()
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

    pub fn from_box(operand: Box<T>) -> Self {
        Self { o: operand }
    }
}

impl<T: Evaluable> Evaluable for Not<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        !self.o.eval(context)
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        self.o.predicates()
    }
}

#[derive(Debug, Clone)]
pub enum FormulaMembers {
    And(And<FormulaMembers>),
    Or(Or<FormulaMembers>),
    Not(Not<FormulaMembers>),
    Pred(Box<dyn Predicate>),
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

    pub fn pred(operand: Box<dyn Predicate>) -> Self {
        Self::Pred(operand)
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        match self {
            Self::And(and) => and
                .o
                .iter()
                .flat_map(Self::predicates)
                .sorted()
                .dedup()
                .collect(),
            Self::Or(or) => {
                or.o.iter()
                    .flat_map(Self::predicates)
                    .sorted()
                    .dedup()
                    .collect()
            }
            Self::Not(not) => not.o.predicates(),
            Self::Pred(p) => vec![p.clone()],
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        self.e.predicates()
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone)]
pub enum NfMembers {
    Not(Not<Box<dyn Predicate>>),
    Pred(Box<dyn Predicate>),
}

impl NfMembers {
    pub fn not(operand: Box<dyn Predicate>) -> Self {
        Self::Not(Not::new(&Box::new(operand)))
    }

    pub fn pred(operand: Box<dyn Predicate>) -> Self {
        Self::Pred(operand)
    }
}

impl Evaluable for NfMembers {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        match self {
            Self::Not(not) => not.o.predicates(),
            Self::Pred(p) => vec![p.clone()],
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        match self {
            Self::And(and) => and
                .o
                .iter()
                .flat_map(NfMembers::predicates)
                .sorted()
                .dedup()
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
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
                            NfMembers::pred(predicates[i].clone())
                        } else {
                            NfMembers::not(predicates[i].clone())
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
// Thankfully, you can impl it for a reference
impl_with_map!(&T, T: Evaluable => Dnf, map_to_dnf);

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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
        match self {
            Self::Or(or) => {
                or.o.iter()
                    .flat_map(NfMembers::predicates)
                    .sorted()
                    .dedup()
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

    fn predicates(&self) -> Vec<Box<dyn Predicate>> {
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
                            NfMembers::pred(predicates[i].clone())
                        } else {
                            NfMembers::not(predicates[i].clone())
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
impl_with_map!(&T, T: Evaluable => Cnf, map_to_cnf);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::predicate::Pred;
    use crate::r#type::{Type, TypeCollection};
    use crate::state::State;

    #[test]
    fn test_basic_and() {
        let t = Pred::new("truth", &[]);
        let f = Pred::new("falsehood", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let and = And::new(&[Box::new(t), Box::new(t), Box::new(t)]);
        assert!(and.eval(&state));

        let and = And::new(&[Box::new(t), Box::new(f), Box::new(t)]);
        assert!(!and.eval(&state));
    }

    #[test]
    fn test_basic_or() {
        let t = Pred::new("truth", &[]);
        let f = Pred::new("falsehood", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let or = Or::new(&[Box::new(t), Box::new(t), Box::new(t)]);
        assert!(or.eval(&state));

        let or = Or::new(&[Box::new(t), Box::new(f), Box::new(f)]);
        assert!(or.eval(&state));

        let or = Or::new(&[Box::new(f), Box::new(f), Box::new(f)]);
        assert!(!or.eval(&state));
    }

    #[test]
    fn test_basic_not() {
        let t = Pred::new("truth", &[]);
        let f = Pred::new("falsehood", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let not = Not::new(&Box::new(f));
        assert!(not.eval(&state));

        let not = Not::new(&Box::new(t));
        assert!(!not.eval(&state));
    }

    use FormulaMembers as FM;

    #[test]
    fn test_formula() {
        let t = Pred::new("truth", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(Box::new(t)), FM::not(&FM::pred(Box::new(t)))]),
            FM::pred(Box::new(t)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(t)),
                FM::not(&FM::pred(Box::new(t))),
            ])),
        ]));

        assert!(formula.eval(&state));
    }

    use CnfMembers as C;
    use DnfMembers as D;
    use NfMembers as NF;

    #[test]
    fn test_basic_dnf() {
        let t = Pred::new("truth", &[]);
        let f = Pred::new("falsehood", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let dnf = Dnf::new(&[
            D::and(&[NF::pred(Box::new(t)), NF::pred(Box::new(f))]),
            D::and(&[NF::not(Box::new(f)), NF::pred(Box::new(t))]),
            D::prim(&NF::pred(Box::new(f))),
        ]);

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_basic_cnf() {
        let t = Pred::new("truth", &[]);
        let f = Pred::new("falsehood", &[]);
        let state = State::default().with_predicates(&[Box::new(t)]);

        let cnf = Cnf::new(&[
            C::or(&[NF::pred(Box::new(t)), NF::pred(Box::new(f))]),
            C::or(&[NF::not(Box::new(f)), NF::pred(Box::new(t))]),
            C::prim(&NF::pred(Box::new(f))),
        ]);

        assert!(!cnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let t = Pred::new("foo", &[]);

        // All predicates are the smame
        let expression = FM::and(&[
            FM::or(&[FM::pred(Box::new(t)), FM::not(&FM::pred(Box::new(t)))]),
            FM::pred(Box::new(t)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(t)),
                FM::not(&FM::pred(Box::new(t))),
            ])),
        ]);

        assert_eq!(1, expression.predicates().len());

        // All predicates are unique
        let t = Pred::new("foo", &[]);
        let t1 = Pred::new("bar", &[]);
        let t2 = Pred::new("baz", &[]);
        let t3 = Pred::new("qux", &[]);
        let t4 = Pred::new("corge", &[]);

        let expression = FM::and(&[
            FM::or(&[FM::pred(Box::new(t)), FM::not(&FM::pred(Box::new(t1)))]),
            FM::pred(Box::new(t2)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(t3)),
                FM::not(&FM::pred(Box::new(t4))),
            ])),
        ]);

        assert_eq!(5, expression.predicates().len());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let t = Pred::new("foo", &[]);
        let t1 = Pred::new("bar", &[]);
        let t2 = Pred::new("baz", &[]);

        let expression = FM::and(&[
            FM::or(&[FM::pred(Box::new(t)), FM::not(&FM::pred(Box::new(t1)))]),
            FM::pred(Box::new(t2)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(t1)),
                FM::not(&FM::pred(Box::new(t2))),
            ])),
        ]);

        assert_eq!(3, expression.predicates().len());
    }

    #[test]
    fn test_formula_to_dnf() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let p = Pred::new("foo", &[t]);
        let p1 = Pred::new("bar", &[t]);
        let p2 = Pred::new("baz", &[t]);
        let p3 = Pred::new("qux", &[t]);
        let p4 = Pred::new("corge", &[t]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(Box::new(p)), FM::not(&FM::pred(Box::new(p1)))]),
            FM::pred(Box::new(p2)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(p3)),
                FM::not(&FM::pred(Box::new(p4))),
            ])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_dnf: Dnf = formula.into();
        let test_tt = TruthTable::new(&test_dnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }

    #[test]
    fn test_formula_to_cnf() {
        let mut types = TypeCollection::default();
        let t = types.create("foo");
        let p = Pred::new("foo", &[t]);
        let p1 = Pred::new("bar", &[t]);
        let p2 = Pred::new("baz", &[t]);
        let p3 = Pred::new("qux", &[t]);
        let p4 = Pred::new("corge", &[t]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(Box::new(p)), FM::not(&FM::pred(Box::new(p1)))]),
            FM::pred(Box::new(p2)),
            FM::not(&FM::and(&[
                FM::pred(Box::new(p3)),
                FM::not(&FM::pred(Box::new(p4))),
            ])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_cnf: Cnf = formula.into();
        let test_tt = TruthTable::new(&test_cnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
