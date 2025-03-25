use core::{array::from_ref, marker::PhantomData, ops::Deref};

use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::*;
use itertools::Itertools;
use paste::paste;

use crate::{
    calculus::{
        evaluation::{Evaluable, EvaluationContext},
        predicate::{IsPredicate, Predicate},
    },
    truth_table::TruthTable,
};

pub trait Expression<T: Evaluable<P>, P: IsPredicate<P>> {
    fn members(&self) -> &[T];
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct And<T: Evaluable<P>, P: IsPredicate<P>> {
    o: Vec<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P>, P: IsPredicate<P>> And<T, P> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
            p: PhantomData,
        }
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Expression<T, P> for And<T, P> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Evaluable<P> for And<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.o.iter().all(|e| e.eval(context))
    }

    fn predicates(&self) -> Vec<&P> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Or<T: Evaluable<P>, P: IsPredicate<P>> {
    o: Vec<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Or<T, P> {
    pub fn new(operands: &[T]) -> Self {
        Self {
            o: operands.to_vec(),
            p: PhantomData,
        }
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Expression<T, P> for Or<T, P> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Evaluable<P> for Or<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.o.iter().any(|e| e.eval(context))
    }

    fn predicates(&self) -> Vec<&P> {
        self.o
            .iter()
            .flat_map(Evaluable::predicates)
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Not<T: Evaluable<P>, P: IsPredicate<P>> {
    o: Box<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Not<T, P> {
    pub fn new(operand: &T) -> Self {
        Self {
            o: Box::new(operand.clone()),
            p: PhantomData,
        }
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Expression<T, P> for Not<T, P> {
    fn members(&self) -> &[T] {
        from_ref(self.o.deref())
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> Evaluable<P> for Not<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        !self.o.eval(context)
    }

    fn predicates(&self) -> Vec<&P> {
        self.o.predicates()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormulaMembers<P: IsPredicate<P>> {
    And(And<FormulaMembers<P>, P>),
    Or(Or<FormulaMembers<P>, P>),
    Not(Not<FormulaMembers<P>, P>),
    Pred(P),
}

impl<P: IsPredicate<P>> FormulaMembers<P> {
    pub fn and(operands: &[Self]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn or(operands: &[Self]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn not(operand: &Self) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: P) -> Self {
        Self::Pred(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for FormulaMembers<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::And(and) => and.eval(context),
            Self::Or(or) => or.eval(context),
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<&P> {
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
            Self::Pred(p) => vec![p],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Formula<P: IsPredicate<P>> {
    e: FormulaMembers<P>,
}

impl<P: IsPredicate<P>> Formula<P> {
    pub fn new(expression: FormulaMembers<P>) -> Self {
        Self { e: expression }
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for Formula<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.e.eval(context)
    }

    fn predicates(&self) -> Vec<&P> {
        self.e.predicates()
    }
}

/// Common normal form members that appear in both
/// CNF and DNF at the lowest level
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitives<P: IsPredicate<P>> {
    Not(Not<P, P>),
    Pred(P),
}

impl<P: IsPredicate<P>> Primitives<P> {
    pub fn not(operand: P) -> Self {
        Self::Not(Not::new(&Box::new(operand)))
    }

    pub fn pred(operand: P) -> Self {
        Self::Pred(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for Primitives<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<&P> {
        match self {
            Self::Not(not) => not.predicates(),
            Self::Pred(p) => vec![p],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DnfMembers<P: IsPredicate<P>> {
    And(And<Primitives<P>, P>),
    Prim(Primitives<P>),
}

impl<P: IsPredicate<P>> DnfMembers<P> {
    pub fn and(operands: &[Primitives<P>]) -> Self {
        Self::And(And::new(operands))
    }

    pub fn prim(operand: &Primitives<P>) -> Self {
        Self::Prim(operand.clone())
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for DnfMembers<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::And(and) => and.eval(context),
            Self::Prim(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<&P> {
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

pub trait NormalForm<T: Evaluable<P>, P: IsPredicate<P>> {
    fn expression(&self) -> &impl Expression<T, P>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dnf<P: IsPredicate<P>> {
    f: Or<DnfMembers<P>, P>,
}

impl<P: IsPredicate<P>> Dnf<P> {
    pub fn new(members: &[DnfMembers<P>]) -> Self {
        Self {
            f: Or::new(members),
        }
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> NormalForm<T, P> for Dnf<P>
where
    Or<DnfMembers<P>, P>: Expression<T, P>,
{
    fn expression(&self) -> &impl Expression<T, P> {
        &self.f
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for Dnf<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<&P> {
        self.f.predicates()
    }
}

macro_rules! map_to {
    ($form:ident) => {
        paste! {
            fn [<map_to_ $form:lower>]<T: Evaluable<Predicate>>(value: T) -> $form<Predicate> {
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

                let predicates = tt
                    .columns()
                    .into_iter()
                    .map(|v| (*v).clone())
                    .collect::<Vec<_>>();
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
/// impl<T: Evaluable<P>, P: Pred<P>> From<T> for Dnf {
///     fn from(value: T) -> Self {
///         // code
///     }
/// }
/// ```
/// because Dnf and Cnf already impl Evaluable,
/// and this conflicts with `impl<T> From<T> for T` in standard library.
/// So we have to implement all of them individually.
/// This macro saves us some writing, probably.
#[crabtime::function]
fn impl_with_map(input: TokenStream) -> String {
    struct Params {
        generics: syn::Generics,
        type_from: syn::Ident,
        from_generics: syn::AngleBracketedGenericArguments,
        type_into: syn::Ident,
        into_generics: syn::AngleBracketedGenericArguments,
    }

    impl syn::parse::Parse for Params {
        fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
            let generics = input.parse()?;
            let _arrow: syn::Token![=>] = input.parse()?;
            let type_from = input.parse()?;
            let from_generics = input.parse()?;
            let _arrow: syn::Token![=>] = input.parse()?;
            let type_into = input.parse()?;
            let into_generics = input.parse()?;

            Ok(Params {
                generics,
                type_from,
                from_generics,
                type_into,
                into_generics,
            })
        }
    }

    let input: Params = syn::parse2(input).unwrap();

    let generics = input
        .generics
        .params
        .iter()
        .map(|v| match v {
            syn::GenericParam::Type(t) => format!(
                "{}: {}",
                t.ident,
                t.bounds
                    .iter()
                    .map(|v| match v {
                        syn::TypeParamBound::Trait(t) => {
                            let t = t.path.segments.last().unwrap();
                            format!(
                                "{}<{}>",
                                t.ident,
                                match &t.arguments {
                                    syn::PathArguments::AngleBracketed(a) => a
                                        .args
                                        .iter()
                                        .map(|v| match v {
                                            syn::GenericArgument::Type(t) => match t {
                                                syn::Type::Path(p) => p
                                                    .path
                                                    .segments
                                                    .last()
                                                    .unwrap()
                                                    .ident
                                                    .to_string(),
                                                _ => unreachable!(),
                                            },
                                            _ => unreachable!(),
                                        })
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                    _ => unreachable!(),
                                }
                            )
                        }
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>()
                    .join(" + ")
            ),
            _ => unreachable!(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    let type_from = input.type_from.to_string();
    let from_generics = input
        .from_generics
        .args
        .iter()
        .map(|v| match v {
            syn::GenericArgument::Type(t) => match t {
                syn::Type::Path(p) => p.path.segments.last().unwrap().ident.to_string(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    let type_into = input.type_into.to_string();
    let low_type_into = type_into.to_lowercase();
    let into_generics = input
        .into_generics
        .args
        .iter()
        .map(|v| match v {
            syn::GenericArgument::Type(t) => match t {
                syn::Type::Path(p) => p.path.segments.last().unwrap().ident.to_string(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    let out = crabtime::quote!(
        impl<{{generics}}> From<{{type_from}}<{{from_generics}}>> for {{type_into}}<{{into_generics}}> {
            fn from(value: {{type_from}}<{{from_generics}}>) -> Self {
                map_to_{{low_type_into}}(value)
            }
        }
    );
    // println!("[ERROR] {:#?}", out);
    out
}

impl_with_map!(<T: Evaluable<Predicate>> => And<T, Predicate> => Dnf<Predicate>);
impl_with_map!(<T: Evaluable<Predicate>> => Or<T, Predicate> => Dnf<Predicate>);
impl_with_map!(<T: Evaluable<Predicate>> => Not<T, Predicate> => Dnf<Predicate>);
impl_with_map!(<> => FormulaMembers<Predicate> => Dnf<Predicate>);
impl_with_map!(<> => Formula<Predicate> => Dnf<Predicate>);
impl_with_map!(<> => Primitives<Predicate> => Dnf<Predicate>);
impl_with_map!(<> => DnfMembers<Predicate> => Dnf<Predicate>);
impl_with_map!(<> => CnfMembers<Predicate> => Dnf<Predicate>);
impl_with_map!(<> => Cnf<Predicate> => Dnf<Predicate>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CnfMembers<P: IsPredicate<P>> {
    Or(Or<Primitives<P>, P>),
    Prim(Primitives<P>),
}

impl<P: IsPredicate<P>> CnfMembers<P> {
    pub fn or(operands: &[Primitives<P>]) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn prim(operand: &Primitives<P>) -> Self {
        Self::Prim(operand.clone())
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for CnfMembers<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            CnfMembers::Or(or) => or.eval(context),
            CnfMembers::Prim(p) => p.eval(context),
        }
    }

    fn predicates(&self) -> Vec<&P> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cnf<P: IsPredicate<P>> {
    f: And<CnfMembers<P>, P>,
}

impl<P: IsPredicate<P>> Cnf<P> {
    pub fn new(members: &[CnfMembers<P>]) -> Self {
        Self {
            f: And::new(members),
        }
    }

    pub fn expression(&self) -> &And<CnfMembers<P>, P> {
        &self.f
    }
}

impl<T: Evaluable<P>, P: IsPredicate<P>> NormalForm<T, P> for Cnf<P>
where
    And<CnfMembers<P>, P>: Expression<T, P>,
{
    fn expression(&self) -> &impl Expression<T, P> {
        &self.f
    }
}

impl<P: IsPredicate<P>> Evaluable<P> for Cnf<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.f.eval(context)
    }

    fn predicates(&self) -> Vec<&P> {
        self.f.predicates()
    }
}

impl_with_map!(<T: Evaluable<Predicate>> => And<T, Predicate> => Cnf<Predicate>);
impl_with_map!(<T: Evaluable<Predicate>> => Or<T, Predicate> => Cnf<Predicate>);
impl_with_map!(<T: Evaluable<Predicate>> => Not<T, Predicate> => Cnf<Predicate>);
impl_with_map!(<> => FormulaMembers<Predicate> => Cnf<Predicate>);
impl_with_map!(<> => Formula<Predicate> => Cnf<Predicate>);
impl_with_map!(<> => Primitives<Predicate> => Cnf<Predicate>);
impl_with_map!(<> => DnfMembers<Predicate> => Cnf<Predicate>);
impl_with_map!(<> => Dnf<Predicate> => Cnf<Predicate>);
impl_with_map!(<> => CnfMembers<Predicate> => Cnf<Predicate>);

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::calculus::predicate::{PredicateBuilder, Value};
    use crate::entity::{EntityStorage, ObjectStorage, TypeStorage};
    use crate::state::State;
    use alloc::vec::Vec;

    #[test]
    fn test_basic_and() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let and = And::new(&[t.clone(), t.clone(), t.clone()]);
        assert!(and.eval(&state));

        let and = And::new(&[t.clone(), f.clone(), t.clone()]);
        assert!(!and.eval(&state));
    }

    #[test]
    fn test_basic_or() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let or = Or::new(&[t.clone(), t.clone(), t.clone()]);
        assert!(or.eval(&state));

        let or = Or::new(&[t.clone(), f.clone(), f.clone()]);
        assert!(or.eval(&state));

        let or = Or::new(&[f.clone(), f.clone(), f.clone()]);
        assert!(!or.eval(&state));
    }

    #[test]
    fn test_basic_not() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let not = Not::new(&f.clone());
        assert!(not.eval(&state));

        let not = Not::new(&t.clone());
        assert!(!not.eval(&state));
    }

    use FormulaMembers as FM;

    #[test]
    fn test_formula() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(t.clone()), FM::not(&FM::pred(t.clone()))]),
            FM::pred(t.clone()),
            FM::not(&FM::and(&[
                FM::pred(t.clone()),
                FM::not(&FM::pred(t.clone())),
            ])),
        ]));

        assert!(formula.eval(&state));
    }

    use CnfMembers as C;
    use DnfMembers as D;
    use Primitives as NF;

    #[test]
    fn test_basic_dnf() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let dnf = Dnf::new(&[
            D::and(&[NF::pred(t.clone()), NF::pred(f.clone())]),
            D::and(&[NF::not(f.clone()), NF::pred(t.clone())]),
            D::prim(&NF::pred(f.clone())),
        ]);

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_basic_cnf() {
        let t = PredicateBuilder::new("true")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let state = State::default().with_predicates(&[t.clone()]);

        let cnf = Cnf::new(&[
            C::or(&[NF::pred(t.clone()), NF::pred(f.clone())]),
            C::or(&[NF::not(f.clone()), NF::pred(t.clone())]),
            C::prim(&NF::pred(f.clone())),
        ]);

        assert!(!cnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let p = PredicateBuilder::new("foo")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();

        // All predicates are the smame
        let expression = FM::and(&[
            FM::or(&[FM::pred(p.clone()), FM::not(&FM::pred(p.clone()))]),
            FM::pred(p.clone()),
            FM::not(&FM::and(&[
                FM::pred(p.clone()),
                FM::not(&FM::pred(p.clone())),
            ])),
        ]);

        assert_eq!(1, expression.predicates().len());

        // All predicates are unique
        let p = PredicateBuilder::new("foo")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();

        let expression = FM::and(&[
            FM::or(&[FM::pred(p), FM::not(&FM::pred(p1))]),
            FM::pred(p2),
            FM::not(&FM::and(&[FM::pred(p3), FM::not(&FM::pred(p4))])),
        ]);

        assert_eq!(5, expression.predicates().len());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let p = PredicateBuilder::new("foo")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();

        let expression = FM::and(&[
            FM::or(&[FM::pred(p), FM::not(&FM::pred(p1.clone()))]),
            FM::pred(p2.clone()),
            FM::not(&FM::and(&[FM::pred(p1), FM::not(&FM::pred(p2))])),
        ]);

        assert_eq!(3, expression.predicates().len());
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
            .arguments(&[&t])
            .values(&[&Value::object(&a)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(&[&t])
            .values(&[&Value::object(&b)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(&[&t])
            .values(&[&Value::object(&c)])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(&[&t])
            .values(&[&Value::object(&d)])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(&[&t])
            .values(&[&Value::object(&e)])
            .build()
            .unwrap();

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(p.clone()), FM::not(&FM::pred(p1.clone()))]),
            FM::pred(p2.clone()),
            FM::not(&FM::and(&[
                FM::pred(p3.clone()),
                FM::not(&FM::pred(p4.clone())),
            ])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_dnf: Dnf<Predicate> = formula.into();
        let test_tt = TruthTable::new(&test_dnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }

    #[test]
    fn test_formula_to_cnf() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let a = entities.get_or_create_object("a", &t);
        let b = entities.get_or_create_object("b", &t);
        let c = entities.get_or_create_object("c", &t);
        let d = entities.get_or_create_object("d", &t);
        let e = entities.get_or_create_object("e", &t);

        let p = PredicateBuilder::new("foo")
            .arguments(&[&t])
            .values(&[&Value::object(&a)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(&[&t])
            .values(&[&Value::object(&b)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(&[&t])
            .values(&[&Value::object(&c)])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(&[&t])
            .values(&[&Value::object(&d)])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(&[&t])
            .values(&[&Value::object(&e)])
            .build()
            .unwrap();

        let formula = Formula::new(FM::and(&[
            FM::or(&[FM::pred(p.clone()), FM::not(&FM::pred(p1.clone()))]),
            FM::pred(p2.clone()),
            FM::not(&FM::and(&[
                FM::pred(p3.clone()),
                FM::not(&FM::pred(p4.clone())),
            ])),
        ]));

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_cnf: Cnf<Predicate> = formula.into();
        let test_tt = TruthTable::new(&test_cnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
