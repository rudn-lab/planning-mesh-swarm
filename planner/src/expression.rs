use core::{array::from_ref, ops::Deref};

use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::*;
use itertools::Itertools;
use paste::paste;

use crate::{
    evaluation::{Evaluable, EvaluationContext},
    predicate::Predicate,
    truth_table::TruthTable,
};

pub trait Expression<T: Evaluable> {
    fn members(&self) -> &[T];
}

#[derive(Debug, Clone)]
pub struct And<T: Evaluable> {
    pub(crate) o: Vec<T>,
}

impl<T: Evaluable> And<T> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
        }
    }
}

impl<T: Evaluable> Expression<T> for And<T> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable> Evaluable for And<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.o.iter().all(|e| e.eval(context))
    }

    fn predicates(&self) -> Vec<Predicate> {
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
    pub(crate) o: Vec<T>,
}

impl<T: Evaluable> Or<T> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
        }
    }
}

impl<T: Evaluable> Expression<T> for Or<T> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable> Evaluable for Or<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.o.iter().any(|e| e.eval(context))
    }

    fn predicates(&self) -> Vec<Predicate> {
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
    pub(crate) o: Box<T>,
}

impl<T: Evaluable> Not<T> {
    pub fn new(operand: &T) -> Self {
        Self {
            o: Box::new(operand.clone()),
        }
    }
}

impl<T: Evaluable> Expression<T> for Not<T> {
    fn members(&self) -> &[T] {
        from_ref(self.o.deref())
    }
}

impl<T: Evaluable> Evaluable for Not<T> {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        !self.o.eval(context)
    }

    fn predicates(&self) -> Vec<Predicate> {
        self.o.predicates()
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
    pub fn and(operands: &[Self]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn or(operands: &[Self]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn not(operand: &Self) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: Predicate) -> Self {
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

    fn predicates(&self) -> Vec<Predicate> {
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
            Self::Not(not) => not.predicates(),
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

    fn predicates(&self) -> Vec<Predicate> {
        self.e.predicates()
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone)]
pub enum Primitives {
    Not(Not<Predicate>),
    Pred(Predicate),
}

impl Primitives {
    pub fn not(operand: Predicate) -> Self {
        Self::Not(Not::new(&Box::new(operand)))
    }

    pub fn pred(operand: Predicate) -> Self {
        Self::Pred(operand)
    }
}

impl Evaluable for Primitives {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        match self {
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<Predicate> {
        match self {
            Self::Not(not) => not.predicates(),
            Self::Pred(p) => vec![p.clone()],
        }
    }
}

#[derive(Debug, Clone)]
pub enum DnfMembers {
    And(And<Primitives>),
    Prim(Primitives),
}

impl DnfMembers {
    pub fn and(operands: &[Primitives]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn prim(operand: &Primitives) -> Self {
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

    fn predicates(&self) -> Vec<Predicate> {
        match self {
            Self::And(and) => and
                .o
                .iter()
                .flat_map(Primitives::predicates)
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
                .collect(),
            Self::Prim(p) => p.predicates(),
        }
    }
}

pub trait NormalForm<T: Evaluable> {
    fn expression(&self) -> impl Expression<T>;
}

#[derive(Debug, Clone)]
pub struct Dnf {
    f: Or<DnfMembers>,
}

impl Dnf {
    pub fn new(members: &[DnfMembers]) -> Self {
        Self {
            f: Or::new(members),
        }
    }
}

impl<T: Evaluable> NormalForm<T> for Dnf
where
    Or<DnfMembers>: Expression<T>,
{
    fn expression(&self) -> impl Expression<T> {
        self.f.clone()
    }
}

impl Evaluable for Dnf {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<Predicate> {
        self.f.predicates()
    }
}

macro_rules! map_to {
    ($form:ident) => {
        paste! {
            fn [<map_to_ $form:lower>]<T: Evaluable>(value: T) -> $form {
                let tt = TruthTable::new(&value);

                macro_rules! cond_rows {
                    (Dnf) => { tt.only_true_rows() };
                    (Cnf) => { tt.only_false_rows() };
                }
                macro_rules! cond_value { (Dnf) => { 1 }; (Cnf) => { 0 }; }
                macro_rules! cond_method {
                    (Dnf) => { [<$form Members>]::and };
                    (Cnf) => { [<$form Members>]::or };
                }

                let predicates = tt.columns().to_vec();
                let expr = cond_rows!($form)
                    .map(|i| {
                        cond_method!($form)(
                            &predicates
                                .iter()
                                .enumerate()
                                .map(|(n, p)| {
                                    if ((i >> n) & 1) == cond_value!($form) {
                                        Primitives::pred(p.clone())
                                    } else {
                                        Primitives::not(p.clone())
                                    }
                                })
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<Vec<_>>();

                $form::new(&expr)
            }
        }
    };
}

map_to!(Dnf);
map_to!(Cnf);

/// You cannot just do
/// ```ignore
/// impl<T: Evaluable> From<T> for Dnf {
///     fn from(value: T) -> Self {
///         // code
///     }
/// }
/// ```
/// because Dnf and Cnf already impl Evaluable,
/// and this conflicts with `impl<T> From<T> for T` in standard library.
/// So we have to implement all of them individually.
/// This macro saves us some writing.
macro_rules! impl_with_map {
    ($from:ty, $g1:ident $(: $t1:ident)? $(, $g:ident $(: $t:ident)?)* => $for:ty) => {
        paste! {
            impl<$g1 $(: $t1)? $(, $g $(: $t)?)*> From<$from> for $for {
                fn from(value: $from) -> Self {
                    [<map_to_ $for:lower>](value)
                }
            }
        }
    };
    ($from:ty => $for:ty) => {
        paste! {
            impl From<$from> for $for {
                fn from(value: $from) -> Self {
                    [<map_to_ $for:lower>](value)
                }
            }
        }
    };
}

impl_with_map!(And<T>, T: Evaluable => Dnf);
impl_with_map!(Or<T>, T: Evaluable => Dnf);
impl_with_map!(Not<T>, T: Evaluable => Dnf);
impl_with_map!(FormulaMembers => Dnf);
impl_with_map!(Formula => Dnf);
impl_with_map!(Primitives => Dnf);
impl_with_map!(DnfMembers => Dnf);
impl_with_map!(CnfMembers => Dnf);
impl_with_map!(Cnf => Dnf);
// Thankfully, you can impl it for a reference
impl_with_map!(&T, T: Evaluable => Dnf);

#[derive(Debug, Clone)]
pub enum CnfMembers {
    Or(Or<Primitives>),
    Prim(Primitives),
}

impl CnfMembers {
    pub fn or(operands: &[Primitives]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn prim(operand: &Primitives) -> Self {
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

    fn predicates(&self) -> Vec<Predicate> {
        match self {
            Self::Or(or) => {
                or.o.iter()
                    .flat_map(Primitives::predicates)
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

    pub fn expression(&self) -> &And<CnfMembers> {
        &self.f
    }
}

impl<T: Evaluable> NormalForm<T> for Cnf
where
    And<CnfMembers>: Expression<T>,
{
    fn expression(&self) -> impl Expression<T> {
        self.f.clone()
    }
}

impl Evaluable for Cnf {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<Predicate> {
        self.f.predicates()
    }
}

impl_with_map!(And<T>, T: Evaluable => Cnf);
impl_with_map!(Or<T>, T: Evaluable => Cnf);
impl_with_map!(Not<T>, T: Evaluable => Cnf);
impl_with_map!(FormulaMembers => Cnf);
impl_with_map!(Formula => Cnf);
impl_with_map!(Primitives => Cnf);
impl_with_map!(DnfMembers => Cnf);
impl_with_map!(Dnf => Cnf);
impl_with_map!(CnfMembers => Cnf);
impl_with_map!(&T, T: Evaluable => Cnf);

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::object::ObjectCollection;
    use crate::predicate::{PredicateDeclaration, ResolvedPredicate, Value};
    use crate::r#type::TypeCollection;
    use alloc::vec::Vec;

    /// A simple evaluator used to check expression evaluation.
    ///
    /// Models the basic principle of the [State]:
    /// if [Evaluator] contains a predicate then it evaluates to true,
    /// and false otherwise.
    /// This evaluator just compares the names, as predicate values
    /// don't matter for these tests.
    /// Most of the predicates have no values.
    /// And those that do, have them only so that they will
    /// not be skipped by the [TruthTable] when it is being built.
    struct Evaluator {
        true_predicates: Vec<ResolvedPredicate>,
    }

    impl Evaluator {
        pub fn new(true_predicates: &[ResolvedPredicate]) -> Self {
            Self {
                true_predicates: true_predicates.to_vec(),
            }
        }
    }

    impl EvaluationContext for Evaluator {
        fn eval(&self, predicate: Predicate) -> bool {
            self.true_predicates
                .iter()
                .any(|tp| tp.name() == predicate.name())
        }
    }

    #[test]
    fn test_basic_and() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let f = PredicateDeclaration::new("falsehood", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let and = And::new(&[t.clone(), t.clone(), t.clone()]);
        assert!(and.eval(&evaluator));

        let and = And::new(&[t.clone(), f.clone(), t.clone()]);
        assert!(!and.eval(&evaluator));
    }

    #[test]
    fn test_basic_or() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let f = PredicateDeclaration::new("falsehood", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let or = Or::new(&[t.clone(), t.clone(), t.clone()]);
        assert!(or.eval(&evaluator));

        let or = Or::new(&[t.clone(), f.clone(), f.clone()]);
        assert!(or.eval(&evaluator));

        let or = Or::new(&[f.clone(), f.clone(), f.clone()]);
        assert!(!or.eval(&evaluator));
    }

    #[test]
    fn test_basic_not() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let f = PredicateDeclaration::new("falsehood", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let not = Not::new(&f.clone());
        assert!(not.eval(&evaluator));

        let not = Not::new(&t.clone());
        assert!(!not.eval(&evaluator));
    }

    use FormulaMembers as FM;

    #[test]
    fn test_formula() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(t.clone()), FM::not(&FM::pred(t.clone()))]),
            FM::pred(t.clone()),
            FM::not(&FM::and(&[
                FM::pred(t.clone()),
                FM::not(&FM::pred(t.clone())),
            ])),
        ]));

        assert!(formula.eval(&evaluator));
    }

    use CnfMembers as C;
    use DnfMembers as D;
    use Primitives as NF;

    #[test]
    fn test_basic_dnf() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let f = PredicateDeclaration::new("falsehood", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let dnf = Dnf::new(&[
            D::and(&[NF::pred(t.clone()), NF::pred(f.clone())]),
            D::and(&[NF::not(f.clone()), NF::pred(t.clone())]),
            D::prim(&NF::pred(f.clone())),
        ]);

        assert!(dnf.eval(&evaluator));
    }

    #[test]
    fn test_basic_cnf() {
        let t = PredicateDeclaration::new("truth", &[]).as_specific(&[]);
        let f = PredicateDeclaration::new("falsehood", &[]).as_specific(&[]);
        let evaluator = Evaluator::new(&[t.as_resolved(&[])]);

        let cnf = Cnf::new(&[
            C::or(&[NF::pred(t.clone()), NF::pred(f.clone())]),
            C::or(&[NF::not(f.clone()), NF::pred(t.clone())]),
            C::prim(&NF::pred(f.clone())),
        ]);

        assert!(!cnf.eval(&evaluator));
    }

    #[test]
    fn test_expression_get_predicates() {
        let t = PredicateDeclaration::new("foo", &[]).as_specific(&[]);

        // All predicates are the smame
        let expression = FM::and(&[
            FM::or(&[FM::pred(t.clone()), FM::not(&FM::pred(t.clone()))]),
            FM::pred(t.clone()),
            FM::not(&FM::and(&[
                FM::pred(t.clone()),
                FM::not(&FM::pred(t.clone())),
            ])),
        ]);

        assert_eq!(1, expression.predicates().len());

        // All predicates are unique
        let t = PredicateDeclaration::new("foo", &[]).as_specific(&[]);
        let t1 = PredicateDeclaration::new("bar", &[]).as_specific(&[]);
        let t2 = PredicateDeclaration::new("baz", &[]).as_specific(&[]);
        let t3 = PredicateDeclaration::new("qux", &[]).as_specific(&[]);
        let t4 = PredicateDeclaration::new("corge", &[]).as_specific(&[]);

        let expression = FM::and(&[
            FM::or(&[FM::pred(t.clone()), FM::not(&FM::pred(t1.clone()))]),
            FM::pred(t2.clone()),
            FM::not(&FM::and(&[
                FM::pred(t3.clone()),
                FM::not(&FM::pred(t4.clone())),
            ])),
        ]);

        assert_eq!(5, expression.predicates().len());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let t = PredicateDeclaration::new("foo", &[]).as_specific(&[]);
        let t1 = PredicateDeclaration::new("bar", &[]).as_specific(&[]);
        let t2 = PredicateDeclaration::new("baz", &[]).as_specific(&[]);

        let expression = FM::and(&[
            FM::or(&[FM::pred(t.clone()), FM::not(&FM::pred(t1.clone()))]),
            FM::pred(t2.clone()),
            FM::not(&FM::and(&[
                FM::pred(t1.clone()),
                FM::not(&FM::pred(t2.clone())),
            ])),
        ]);

        assert_eq!(3, expression.predicates().len());
    }

    #[test]
    fn test_formula_to_dnf() {
        let mut types = TypeCollection::default();
        let t = types.get_or_create("foo");

        let mut objects = ObjectCollection::default();
        let a = objects.get_or_create("a", &t);
        let b = objects.get_or_create("b", &t);
        let c = objects.get_or_create("c", &t);
        let d = objects.get_or_create("d", &t);
        let e = objects.get_or_create("e", &t);

        let p = PredicateDeclaration::new("foo", &[&t]).as_specific(&[Value::Object(a)]);
        let p1 = PredicateDeclaration::new("bar", &[&t]).as_specific(&[Value::Object(b)]);
        let p2 = PredicateDeclaration::new("baz", &[&t]).as_specific(&[Value::Object(c)]);
        let p3 = PredicateDeclaration::new("qux", &[&t]).as_specific(&[Value::Object(d)]);
        let p4 = PredicateDeclaration::new("corge", &[&t]).as_specific(&[Value::Object(e)]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(p.clone()), FM::not(&FM::pred(p1.clone()))]),
            FM::pred(p2.clone()),
            FM::not(&FM::and(&[
                FM::pred(p3.clone()),
                FM::not(&FM::pred(p4.clone())),
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
        let t = types.get_or_create("foo");

        let mut objects = ObjectCollection::default();
        let a = objects.get_or_create("a", &t);
        let b = objects.get_or_create("b", &t);
        let c = objects.get_or_create("c", &t);
        let d = objects.get_or_create("d", &t);
        let e = objects.get_or_create("e", &t);

        let p = PredicateDeclaration::new("foo", &[&t]).as_specific(&[Value::Object(a)]);
        let p1 = PredicateDeclaration::new("bar", &[&t]).as_specific(&[Value::Object(b)]);
        let p2 = PredicateDeclaration::new("baz", &[&t]).as_specific(&[Value::Object(c)]);
        let p3 = PredicateDeclaration::new("qux", &[&t]).as_specific(&[Value::Object(d)]);
        let p4 = PredicateDeclaration::new("corge", &[&t]).as_specific(&[Value::Object(e)]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(p.clone()), FM::not(&FM::pred(p1.clone()))]),
            FM::pred(p2.clone()),
            FM::not(&FM::and(&[
                FM::pred(p3.clone()),
                FM::not(&FM::pred(p4.clone())),
            ])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_cnf: Cnf = formula.into();
        let test_tt = TruthTable::new(&test_cnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
