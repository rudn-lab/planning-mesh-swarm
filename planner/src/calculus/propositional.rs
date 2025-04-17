use core::{array::from_ref, marker::PhantomData, ops::Deref};

use alloc::boxed::Box;
use alloc::vec::*;
use itertools::Itertools;
use paste::paste;

use crate::{
    calculus::{
        evaluation::{Evaluable, EvaluationContext},
        predicate::IsPredicate,
    },
    truth_table::TruthTable,
};

pub trait Expression<T: Evaluable<P, P>, P: IsPredicate<P>> {
    fn members(&self) -> &[T];
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct And<T: Evaluable<P, P>, P: IsPredicate<P>> {
    pub(super) o: Vec<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> And<T, P> {
    pub fn new(operands: Vec<T>) -> Self {
        Self {
            o: operands,
            p: PhantomData,
        }
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Expression<T, P> for And<T, P> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Evaluable<P, P> for And<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.o.iter().all(|e| e.eval(context))
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            self.o
                .iter()
                .flat_map(Evaluable::predicates)
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Or<T: Evaluable<P, P>, P: IsPredicate<P>> {
    pub(super) o: Vec<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Or<T, P> {
    pub fn new(operands: Vec<T>) -> Self {
        Self {
            o: operands,
            p: PhantomData,
        }
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Expression<T, P> for Or<T, P> {
    fn members(&self) -> &[T] {
        self.o.as_slice()
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Evaluable<P, P> for Or<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.o.iter().any(|e| e.eval(context))
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            self.o
                .iter()
                .flat_map(Evaluable::predicates)
                .sorted()
                .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Not<T: Evaluable<P, P>, P: IsPredicate<P>> {
    pub(super) o: Box<T>,
    p: PhantomData<P>,
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Not<T, P> {
    pub fn new(operand: T) -> Self {
        Self {
            o: Box::new(operand),
            p: PhantomData,
        }
    }
}

impl<P: IsPredicate<P>> Not<P, P> {
    pub fn inner(&self) -> &P {
        &self.o
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Expression<T, P> for Not<T, P> {
    fn members(&self) -> &[T] {
        from_ref(self.o.deref())
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> Evaluable<P, P> for Not<T, P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        !self.o.eval(context)
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        self.o.predicates()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Formula<P: IsPredicate<P>> {
    And(And<Formula<P>, P>),
    Or(Or<Formula<P>, P>),
    Not(Not<Formula<P>, P>),
    Pred(P),
}

impl<P: IsPredicate<P>> Formula<P> {
    pub fn and(operands: Vec<Self>) -> Self {
        Self::And(And::new(operands))
    }

    pub fn or(operands: Vec<Self>) -> Self {
        Self::Or(Or::new(operands))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(operand: Self) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: P) -> Self {
        Self::Pred(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Formula<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Formula::And(and) => and.eval(context),
            Formula::Or(or) => or.eval(context),
            Formula::Not(not) => not.eval(context),
            Formula::Pred(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            match self {
                Formula::And(and) => {
                    Box::new(and.o.iter().flat_map(Self::predicates)) as Box<dyn Iterator<Item = _>>
                }
                Formula::Or(or) => {
                    Box::new(or.o.iter().flat_map(Self::predicates)) as Box<dyn Iterator<Item = _>>
                }
                Formula::Not(not) => not.predicates(),
                Formula::Pred(p) => Box::new(core::iter::once(p)) as Box<dyn Iterator<Item = _>>,
            }
            .sorted()
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

/// Predicate or negated predicate.
///
/// Appears in many normal forms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitives<P: IsPredicate<P>> {
    Not(Not<P, P>),
    Pred(P),
}

impl<P: IsPredicate<P>> Primitives<P> {
    pub fn not(operand: P) -> Self {
        Self::Not(Not::new(operand))
    }

    pub fn pred(operand: P) -> Self {
        Self::Pred(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Primitives<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::Not(not) => not.eval(context),
            Self::Pred(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        match self {
            Self::Not(not) => not.predicates(),
            Self::Pred(p) => Box::new(core::iter::once(p)) as Box<dyn Iterator<Item = _>>,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DnfMembers<P: IsPredicate<P>> {
    And(And<Primitives<P>, P>),
    Prim(Primitives<P>),
}

impl<P: IsPredicate<P>> DnfMembers<P> {
    pub fn and(operands: Vec<Primitives<P>>) -> Self {
        Self::And(And::new(operands))
    }

    pub fn prim(operand: Primitives<P>) -> Self {
        Self::Prim(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for DnfMembers<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            Self::And(and) => and.eval(context),
            Self::Prim(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        Box::new(
            match self {
                Self::And(and) => Box::new(and.o.iter().flat_map(Primitives::predicates).sorted())
                    as Box<dyn Iterator<Item = _>>,
                Self::Prim(p) => p.predicates(),
            }
            .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
        )
    }
}

pub trait NormalForm<T: Evaluable<P, P>, P: IsPredicate<P>> {
    fn expression(&self) -> &impl Expression<T, P>;
}

/// Full DNF.
///
/// In this DNF all predicates appear in each clause either as is or negated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dnf<P: IsPredicate<P>> {
    f: Or<DnfMembers<P>, P>,
}

impl<P: IsPredicate<P>> Dnf<P> {
    /// WARN: Use with caution!
    /// Do not use it where it can construct a partial DNF.
    /// Partial DNF (which doesn't contain all predicates in each clause)
    /// can cause issues in the action evaluation step.
    ///
    /// If you want to construct a DNF prefer using .into().
    pub(crate) fn new(members: Vec<DnfMembers<P>>) -> Self {
        Self {
            f: Or::new(members),
        }
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> NormalForm<T, P> for Dnf<P>
where
    Or<DnfMembers<P>, P>: Expression<T, P>,
{
    fn expression(&self) -> &impl Expression<T, P> {
        &self.f
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Dnf<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.f.eval(context)
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        self.f.predicates()
    }
}

macro_rules! map_to {
    ($form:ident) => {
        paste! {
            fn [<map_to_ $form:lower>]<T: Evaluable<P, P>, P: IsPredicate<P>>(value: T) -> $form<P> {
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
                            predicates
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

                $form::new(expr)
            }
        }
    };
}

map_to!(Dnf);
map_to!(Cnf);

/// You cannot just do
/// ```ignore
/// impl<T: Evaluable<P, P>, P: Pred<P>> From<T> for Dnf {
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

impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => And<T, P> => Dnf<P>);
impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => Or<T, P> => Dnf<P>);
impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => Not<T, P> => Dnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Formula<P> => Dnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Primitives<P> => Dnf<P>);
impl_with_map!(<P: IsPredicate<P>> => DnfMembers<P> => Dnf<P>);
impl_with_map!(<P: IsPredicate<P>> => CnfMembers<P> => Dnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Cnf<P> => Dnf<P>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CnfMembers<P: IsPredicate<P>> {
    Or(Or<Primitives<P>, P>),
    Prim(Primitives<P>),
}

impl<P: IsPredicate<P>> CnfMembers<P> {
    pub fn or(operands: Vec<Primitives<P>>) -> Self {
        Self::Or(Or::new(operands))
    }

    pub fn prim(operand: Primitives<P>) -> Self {
        Self::Prim(operand)
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for CnfMembers<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        match self {
            CnfMembers::Or(or) => or.eval(context),
            CnfMembers::Prim(p) => p.eval(context),
        }
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        match self {
            Self::Or(or) => Box::new(
                or.o.iter()
                    .flat_map(Primitives::predicates)
                    .sorted()
                    .dedup_by(|x, y| x.unique_marker() == y.unique_marker()),
            ) as Box<dyn Iterator<Item = _>>,
            Self::Prim(p) => p.predicates(),
        }
    }
}

/// Full CNF.
///
/// In this CNF all predicates appear in each clause either as is or negated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cnf<P: IsPredicate<P>> {
    f: And<CnfMembers<P>, P>,
}

impl<P: IsPredicate<P>> Cnf<P> {
    /// WARN: Use with caution!
    /// Do not use it where it can construct a partial CNF.
    ///
    /// If you want to construct a CNF prefer using .into().
    pub(crate) fn new(members: Vec<CnfMembers<P>>) -> Self {
        Self {
            f: And::new(members),
        }
    }

    pub fn expression(&self) -> &And<CnfMembers<P>, P> {
        &self.f
    }
}

impl<T: Evaluable<P, P>, P: IsPredicate<P>> NormalForm<T, P> for Cnf<P>
where
    And<CnfMembers<P>, P>: Expression<T, P>,
{
    fn expression(&self) -> &impl Expression<T, P> {
        &self.f
    }
}

impl<P: IsPredicate<P>> Evaluable<P, P> for Cnf<P> {
    fn eval(&self, context: &impl EvaluationContext<P>) -> bool {
        self.f.eval(context)
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a P> + 'a> {
        self.f.predicates()
    }
}

impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => And<T, P> => Cnf<P>);
impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => Or<T, P> => Cnf<P>);
impl_with_map!(<T: Evaluable<P, P>, P: IsPredicate<P>> => Not<T, P> => Cnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Formula<P> => Cnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Primitives<P> => Cnf<P>);
impl_with_map!(<P: IsPredicate<P>> => DnfMembers<P> => Cnf<P>);
impl_with_map!(<P: IsPredicate<P>> => Dnf<P> => Cnf<P>);
impl_with_map!(<P: IsPredicate<P>> => CnfMembers<P> => Cnf<P>);

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{
        calculus::predicate::{PredicateBuilder, Value},
        entity::{EntityStorage, ObjectStorage, TypeStorage},
        state::State,
    };
    use alloc::vec::Vec;
    use gazebo::dupe::Dupe;

    use CnfMembers as C;
    use DnfMembers as D;
    use Formula as F;
    use Primitives as NF;

    #[test]
    fn test_basic_and() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let and = And::new(vec![t.clone(), t.clone(), t.clone()]);
        assert!(and.eval(&state));

        let and = And::new(vec![t.clone(), f.clone(), t.clone()]);
        assert!(!and.eval(&state));
    }

    #[test]
    fn test_basic_or() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let or = Or::new(vec![t.clone(), t.clone(), t.clone()]);
        assert!(or.eval(&state));

        let or = Or::new(vec![t.clone(), f.clone(), f.clone()]);
        assert!(or.eval(&state));

        let or = Or::new(vec![f.clone(), f.clone(), f.clone()]);
        assert!(!or.eval(&state));
    }

    #[test]
    fn test_basic_not() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let not = Not::new(f.clone());
        assert!(not.eval(&state));

        let not = Not::new(t.clone());
        assert!(!not.eval(&state));
    }

    #[test]
    fn test_formula() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let formula = F::and(vec![
            F::or(vec![F::pred(t.clone()), F::not(F::pred(t.clone()))]),
            F::pred(t.clone()),
            F::not(F::and(vec![F::pred(t.clone()), F::not(F::pred(t.clone()))])),
        ]);

        assert!(formula.eval(&state));
    }

    #[test]
    fn test_basic_dnf() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let dnf = Dnf::new(vec![
            D::and(vec![NF::pred(t.clone()), NF::pred(f.clone())]),
            D::and(vec![NF::not(f.clone()), NF::pred(t.clone())]),
            D::prim(NF::pred(f.clone())),
        ]);

        assert!(dnf.eval(&state));
    }

    #[test]
    fn test_basic_cnf() {
        let t = PredicateBuilder::new("true")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let f = PredicateBuilder::new("false")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let state = State::default().with_predicates(vec![t.clone()]);

        let cnf = Cnf::new(vec![
            C::or(vec![NF::pred(t.clone()), NF::pred(f.clone())]),
            C::or(vec![NF::not(f.clone()), NF::pred(t.clone())]),
            C::prim(NF::pred(f.clone())),
        ]);

        assert!(!cnf.eval(&state));
    }

    #[test]
    fn test_expression_get_predicates() {
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();

        // All predicates are the smame
        let expression = F::and(vec![
            F::or(vec![F::pred(p.clone()), F::not(F::pred(p.clone()))]),
            F::pred(p.clone()),
            F::not(F::and(vec![F::pred(p.clone()), F::not(F::pred(p.clone()))])),
        ]);

        assert_eq!(1, expression.predicates().count());

        // All predicates are unique
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();

        let expression = F::and(vec![
            F::or(vec![F::pred(p), F::not(F::pred(p1))]),
            F::pred(p2),
            F::not(F::and(vec![F::pred(p3), F::not(F::pred(p4))])),
        ]);

        assert_eq!(5, expression.predicates().count());

        // Predicates are "reused" after "transformation".
        // Look at Predicate.unique_marker.
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();

        let expression = F::and(vec![
            F::or(vec![F::pred(p), F::not(F::pred(p1.clone()))]),
            F::pred(p2.clone()),
            F::not(F::and(vec![F::pred(p1), F::not(F::pred(p2))])),
        ]);

        assert_eq!(3, expression.predicates().count());
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
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&a)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&b)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&c)])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&d)])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&e)])
            .build()
            .unwrap();

        let formula = F::and(vec![
            F::or(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]),
            F::pred(p2.clone()),
            F::not(F::and(vec![
                F::pred(p3.clone()),
                F::not(F::pred(p4.clone())),
            ])),
        ]);

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_dnf: Dnf<_> = formula.into();
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
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&a)])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&b)])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("baz")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&c)])
            .build()
            .unwrap();
        let p3 = PredicateBuilder::new("qux")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&d)])
            .build()
            .unwrap();
        let p4 = PredicateBuilder::new("corge")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&e)])
            .build()
            .unwrap();

        let formula = F::and(vec![
            F::or(vec![F::pred(p.clone()), F::not(F::pred(p1.clone()))]),
            F::pred(p2.clone()),
            F::not(F::and(vec![
                F::pred(p3.clone()),
                F::not(F::pred(p4.clone())),
            ])),
        ]);

        let tt = TruthTable::new(&formula).collect::<Vec<_>>();

        let test_cnf: Cnf<_> = formula.into();
        let test_tt = TruthTable::new(&test_cnf).collect::<Vec<_>>();

        assert_eq!(tt, test_tt);
    }
}
