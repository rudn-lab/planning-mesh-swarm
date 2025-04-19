use crate::{
    calculus::{
        predicate::{GoalPredicate, GroundedPredicate, IsPredicate},
        propositional::*,
        Evaluable, EvaluationContext,
    },
    entity::{ObjectHandle, TypeHandle},
    problem::BuildError,
    sealed::Sealed,
};
use alloc::{boxed::Box, collections::BTreeMap, vec::Vec};
use core::marker::PhantomData;
use gazebo::dupe::Dupe;
use getset::Getters;
use itertools::Itertools;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub struct BoundVariable {
    pub(crate) parameter_idx: usize,
    pub(crate) r#type: TypeHandle,
}

#[allow(private_bounds)]
pub trait QuantifierType: Sealed {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForAllQuantifier;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExistsQuantifier;

impl QuantifierType for ForAllQuantifier {}
impl QuantifierType for ExistsQuantifier {}

impl Sealed for ForAllQuantifier {}
impl Sealed for ExistsQuantifier {}

/// Expression in first-order logic
pub trait FOExpression: PartialEq {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Quantifiers<E: FOExpression, P: IsPredicate> {
    ForAll(ForAll<E, P>),
    Exists(Exists<E, P>),
}

pub type ForAll<E: FOExpression, P: IsPredicate> = Quantifier<E, P, ForAllQuantifier>;
pub type Exists<E: FOExpression, P: IsPredicate> = Quantifier<E, P, ExistsQuantifier>;

#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Quantifier<E: FOExpression, P: IsPredicate, T: QuantifierType> {
    #[getset(get = "pub")]
    variables: Vec<BoundVariable>,
    #[getset(get = "pub")]
    expression: Box<E>,
    p: PhantomData<P>,
    t: PhantomData<T>,
}

impl<E: FOExpression, P: IsPredicate> ForAll<E, P> {
    fn negated<F: Fn(E) -> E>(self, negator: F) -> Exists<E, P> {
        Exists {
            variables: self.variables,
            expression: Box::new(negator(*self.expression)),
            p: PhantomData,
            t: PhantomData,
        }
    }
}

impl<E: FOExpression, P: IsPredicate> Exists<E, P> {
    fn negated<F: Fn(E) -> E>(self, negator: F) -> ForAll<E, P> {
        ForAll {
            variables: self.variables,
            expression: Box::new(negator(*self.expression)),
            p: PhantomData,
            t: PhantomData,
        }
    }
}

impl<P: IsPredicate, T: QuantifierType> From<Quantifier<QuantifiedFormula<P>, P, T>>
    for Quantifier<Nnf<P>, P, T>
{
    fn from(value: Quantifier<QuantifiedFormula<P>, P, T>) -> Self {
        Self {
            variables: value.variables,
            expression: Box::new((*value.expression).into()),
            p: PhantomData,
            t: PhantomData,
        }
    }
}

#[allow(private_bounds)]
pub trait QuantifierBuilderState: Sealed {}

#[derive(Debug, Clone)]
pub struct New;
#[derive(Debug, Clone)]
pub struct HasParameters;
#[derive(Debug, Clone)]
pub struct HasExpression;

impl QuantifierBuilderState for New {}
impl QuantifierBuilderState for HasParameters {}
impl QuantifierBuilderState for HasExpression {}

impl Sealed for New {}
impl Sealed for HasParameters {}
impl Sealed for HasExpression {}

#[derive(Debug, Clone)]
pub struct QuantifierBuilder<E, R, P, T, S>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
    T: QuantifierType,
    S: QuantifierBuilderState,
{
    parameters: Option<Vec<BoundVariable>>,
    add_expression: Option<Box<E>>,
    state: PhantomData<S>,
    qtype: PhantomData<T>,
    p: PhantomData<P>,
}

impl<E, R, P> QuantifierBuilder<E, R, P, ForAllQuantifier, New>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
{
    pub fn forall(
        parameters: Vec<TypeHandle>,
    ) -> QuantifierBuilder<E, R, P, ForAllQuantifier, HasParameters> {
        QuantifierBuilder {
            parameters: Some(Self::params(parameters)),
            add_expression: None,
            state: PhantomData,
            qtype: PhantomData,
            p: PhantomData,
        }
    }
}

impl<E, R, P> QuantifierBuilder<E, R, P, ExistsQuantifier, New>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
{
    pub fn exists(
        parameters: Vec<TypeHandle>,
    ) -> QuantifierBuilder<E, R, P, ExistsQuantifier, HasParameters> {
        QuantifierBuilder {
            parameters: Some(Self::params(parameters)),
            add_expression: None,
            state: PhantomData,
            qtype: PhantomData,
            p: PhantomData,
        }
    }
}
impl<E, R, P, T> QuantifierBuilder<E, R, P, T, New>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
    T: QuantifierType,
{
    fn params(parameters: Vec<TypeHandle>) -> Vec<BoundVariable> {
        parameters
            .into_iter()
            .enumerate()
            .map(|(i, t)| BoundVariable {
                parameter_idx: i,
                r#type: t,
            })
            .collect()
    }
}

impl<E, R, P, T> QuantifierBuilder<E, R, P, T, HasParameters>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
    T: QuantifierType,
{
    pub fn expression(self, add_expression: E) -> QuantifierBuilder<E, R, P, T, HasExpression> {
        QuantifierBuilder {
            parameters: self.parameters,
            add_expression: Some(Box::new(add_expression)),
            state: PhantomData,
            qtype: PhantomData,
            p: PhantomData,
        }
    }
}

impl<E, R, P> QuantifierBuilder<E, R, P, ForAllQuantifier, HasExpression>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
{
    pub fn build(self) -> Result<ForAll<R, P>, BuildError> {
        self.add_expression.unwrap()(self.parameters.as_ref().unwrap()).map(|exp| ForAll {
            variables: self.parameters.unwrap(),
            expression: Box::new(exp),
            p: PhantomData,
            t: PhantomData,
        })
    }
}

impl<E, R, P> QuantifierBuilder<E, R, P, ExistsQuantifier, HasExpression>
where
    E: FnMut(&[BoundVariable]) -> Result<R, BuildError>,
    R: FOExpression,
    P: IsPredicate,
{
    pub fn build(self) -> Result<Exists<R, P>, BuildError> {
        self.add_expression.unwrap()(self.parameters.as_ref().unwrap()).map(|exp| Exists {
            variables: self.parameters.unwrap(),
            expression: Box::new(exp),
            p: PhantomData,
            t: PhantomData,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QuantifiedFormula<P: IsPredicate> {
    Quantifier(Quantifiers<QuantifiedFormula<P>, P>),
    And(Vec<QuantifiedFormula<P>>),
    Or(Vec<QuantifiedFormula<P>>),
    Not(Box<QuantifiedFormula<P>>),
    Pred(P),
}

impl<P: IsPredicate> FOExpression for QuantifiedFormula<P> {}

impl<P: IsPredicate> QuantifiedFormula<P> {
    pub fn forall(operand: ForAll<QuantifiedFormula<P>, P>) -> Self {
        Self::Quantifier(Quantifiers::ForAll(operand))
    }

    pub fn exists(operand: Exists<QuantifiedFormula<P>, P>) -> Self {
        Self::Quantifier(Quantifiers::Exists(operand))
    }

    pub fn and(operands: Vec<Self>) -> Self {
        Self::And(operands)
    }

    pub fn or(operands: Vec<Self>) -> Self {
        Self::Or(operands)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(operand: Self) -> Self {
        Self::Not(Box::new(operand))
    }

    pub fn pred(operand: P) -> Self {
        Self::Pred(operand)
    }
}

/// Negation Normal Form, where all negations are applied only to predicates
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Nnf<P: IsPredicate> {
    Quantifier(Quantifiers<Nnf<P>, P>),
    And(Vec<Nnf<P>>),
    Or(Vec<Nnf<P>>),
    Prim(Primitives<P>),
}

impl<P: IsPredicate> FOExpression for Nnf<P> {}

impl<P: IsPredicate> Nnf<P> {
    pub fn forall(operand: ForAll<Nnf<P>, P>) -> Self {
        Self::Quantifier(Quantifiers::ForAll(operand))
    }

    pub fn exists(operand: Exists<Nnf<P>, P>) -> Self {
        Self::Quantifier(Quantifiers::Exists(operand))
    }

    pub fn and(operands: Vec<Self>) -> Self {
        Self::And(operands)
    }

    pub fn or(operands: Vec<Self>) -> Self {
        Self::Or(operands)
    }

    pub fn pred(operand: P) -> Self {
        Self::Prim(Primitives::Pred(operand))
    }

    pub fn npred(operand: P) -> Self {
        Self::Prim(Primitives::Not(operand))
    }
}

impl<P: IsPredicate> From<QuantifiedFormula<P>> for Nnf<P> {
    fn from(value: QuantifiedFormula<P>) -> Self {
        use QuantifiedFormula as QF;
        use Quantifiers as Q;
        match value {
            QF::Quantifier(q) => match q {
                Q::ForAll(fa) => Nnf::Quantifier(Q::ForAll(fa.into())),
                Q::Exists(ex) => Nnf::Quantifier(Q::Exists(ex.into())),
            },
            QF::And(and) => Nnf::and(and.into_iter().map(|m| m.into()).collect_vec()),
            QF::Or(or) => Nnf::or(or.into_iter().map(|m| m.into()).collect_vec()),
            QF::Not(not) => match *not {
                QF::Quantifier(q) => match q {
                    Quantifiers::ForAll(fa) => QF::Quantifier(Q::Exists(fa.negated(QF::not))),
                    Quantifiers::Exists(ex) => QF::Quantifier(Q::ForAll(ex.negated(QF::not))),
                }
                .into(),
                QF::And(and) => Nnf::or(and.into_iter().map(|m| QF::not(m).into()).collect_vec()),
                QF::Or(or) => Nnf::and(or.into_iter().map(|m| QF::not(m).into()).collect_vec()),
                QF::Not(not) => (*not).into(),
                QF::Pred(p) => Nnf::Prim(Primitives::Not(p)),
            },
            QF::Pred(p) => Nnf::Prim(Primitives::Pred(p)),
        }
    }
}

/// [Quantifier]s without expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QuantifierSymbol {
    ForAll(Vec<BoundVariable>),
    Exists(Vec<BoundVariable>),
}

#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Pdnf<P: IsPredicate> {
    #[getset(get = "pub")]
    prefix: Vec<QuantifierSymbol>,
    #[getset(get = "pub")]
    matrix: Dnf<P>,
}

impl<P: IsPredicate> FOExpression for Pdnf<P> {}

impl Evaluable<GroundedPredicate> for Pdnf<GoalPredicate> {
    fn eval(&self, context: &impl EvaluationContext<GroundedPredicate>) -> bool {
        fn eval_with_prefix(
            prefix: &[QuantifierSymbol],
            var_assignment: &mut BTreeMap<BoundVariable, ObjectHandle>,
            matrix: &Dnf<GoalPredicate>,
            context: &impl EvaluationContext<GroundedPredicate>,
        ) -> bool {
            let handle_predicate = |p: &GoalPredicate| -> bool {
                p.remove_bound(var_assignment)
                    .map(|gp| gp.eval(context))
                    .unwrap_or(false)
            };

            let handle_primitives = |p: &Primitives<GoalPredicate>| -> bool {
                match p {
                    Primitives::Pred(p) => handle_predicate(p),
                    Primitives::Not(not) => !handle_predicate(not),
                }
            };

            let Some((head, tail)) = prefix.split_first() else {
                // All of the prefix is applied.
                return matrix
                    .clauses
                    .iter()
                    .any(|clause| clause.iter().all(handle_primitives));
            };

            fn with_var_assignment<F>(
                var_assignment: &mut BTreeMap<BoundVariable, ObjectHandle>,
                bindings: Vec<(BoundVariable, ObjectHandle)>,
                mut f: F,
            ) -> bool
            where
                F: FnMut(&mut BTreeMap<BoundVariable, ObjectHandle>) -> bool,
            {
                let past_keys = var_assignment.keys().cloned().collect_vec();
                // Update the variables that we check in this branch
                var_assignment.extend(bindings);

                let res = f(var_assignment);

                var_assignment.retain(|k, _| past_keys.contains(k));
                res
            }

            match head {
                QuantifierSymbol::ForAll(variables) => variables
                    .iter()
                    .map(|v| v.r#type.get_objects().into_iter().map(|e| (v.dupe(), e)))
                    .multi_cartesian_product()
                    .all(|v| {
                        with_var_assignment(var_assignment, v, |x| {
                            eval_with_prefix(tail, x, matrix, context)
                        })
                    }),
                QuantifierSymbol::Exists(variables) => variables
                    .iter()
                    .map(|v| v.r#type.get_objects().into_iter().map(|e| (v.dupe(), e)))
                    .multi_cartesian_product()
                    .any(|v| {
                        with_var_assignment(var_assignment, v, |x| {
                            eval_with_prefix(tail, x, matrix, context)
                        })
                    }),
            }
        }

        eval_with_prefix(&self.prefix, &mut BTreeMap::new(), &self.matrix, context)
    }
}

impl<P: IsPredicate> From<Nnf<P>> for Pdnf<P> {
    fn from(value: Nnf<P>) -> Self {
        fn collect_quantifiers<P: IsPredicate>(
            value: Nnf<P>,
            prefix: &mut Vec<QuantifierSymbol>,
        ) -> Formula<P> {
            match value {
                Nnf::Quantifier(q) => {
                    let exp = match q {
                        Quantifiers::ForAll(q) => {
                            prefix.push(QuantifierSymbol::ForAll(q.variables));
                            *q.expression
                        }
                        Quantifiers::Exists(q) => {
                            prefix.push(QuantifierSymbol::Exists(q.variables));
                            *q.expression
                        }
                    };

                    collect_quantifiers(exp, prefix)
                }
                Nnf::And(and) => Formula::and(
                    and.into_iter()
                        .map(|v| collect_quantifiers(v, prefix))
                        .collect_vec(),
                ),
                Nnf::Or(or) => Formula::or(
                    or.into_iter()
                        .map(|v| collect_quantifiers(v, prefix))
                        .collect_vec(),
                ),
                Nnf::Prim(p) => match p {
                    Primitives::Pred(p) => Formula::pred(p),
                    Primitives::Not(not) => Formula::not(Formula::pred(not)),
                },
            }
        }

        let mut prefix = Vec::new();
        let quantifierless_nnf = collect_quantifiers(value, &mut prefix);
        let dnf = quantifierless_nnf.into();

        Pdnf {
            prefix,
            matrix: dnf,
        }
    }
}

impl<P: IsPredicate> From<QuantifiedFormula<P>> for Pdnf<P> {
    fn from(value: QuantifiedFormula<P>) -> Self {
        let nnf: Nnf<_> = value.into();
        nnf.into()
    }
}

impl<P: IsPredicate, T: Into<Dnf<P>>> From<T> for Pdnf<P> {
    fn from(value: T) -> Self {
        Self {
            prefix: Vec::new(),
            matrix: value.into(),
        }
    }
}

#[cfg(test)]
#[coverage(off)]
mod test {
    use super::{QuantifiedFormula as QF, *};
    use crate::{
        calculus::{
            predicate::{GoalValue, PredicateBuilder, Value},
            propositional::Formula as F,
            truth_table::TruthTable,
        },
        entity::{EntityStorage, ObjectStorage, TypeStorage},
        state::State,
    };

    #[test]
    fn test_formula_to_nnf_simple() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let a = entities.get_or_create_object("a", &t);
        let b = entities.get_or_create_object("b", &t);

        let p = PredicateBuilder::new("p")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&a)])
            .build()
            .unwrap();
        let q = PredicateBuilder::new("q")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&b)])
            .build()
            .unwrap();

        let formula = QF::not(QF::not(QF::or(vec![
            QF::pred(p.clone()),
            QF::not(QF::pred(q.clone())),
        ])));

        let nnf: Nnf<_> = formula.into();
        let correct_nnf = Nnf::or(vec![Nnf::pred(p.clone()), Nnf::npred(q.clone())]);

        assert_eq!(nnf, correct_nnf);
    }

    #[test]
    fn test_formula_to_nnf_de_morgan() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("bar");

        let x = entities.get_or_create_object("x", &t);
        let y = entities.get_or_create_object("y", &t);

        let p = PredicateBuilder::new("r")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let q = PredicateBuilder::new("s")
            .arguments(vec![t.dupe()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();

        let formula = QF::not(QF::and(vec![QF::pred(p.clone()), QF::pred(q.clone())]));
        let nnf: Nnf<_> = formula.into();
        let correct_nnf = Nnf::or(vec![Nnf::npred(p.clone()), Nnf::npred(q.clone())]);

        assert_eq!(nnf, correct_nnf);

        let formula = QF::not(QF::or(vec![QF::pred(p.clone()), QF::pred(q.clone())]));
        let nnf: Nnf<_> = formula.into();
        let correct_nnf = Nnf::and(vec![Nnf::npred(p.clone()), Nnf::npred(q.clone())]);

        assert_eq!(nnf, correct_nnf);
    }

    #[test]
    fn test_formula_to_nnf_forall_not() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("bar");

        let x = entities.get_or_create_object("x", &t);

        let p = PredicateBuilder::new("p").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);

        let formula = QF::not(QF::forall(
            QuantifierBuilder::forall(vec![t.dupe()])
                .expression(|params| {
                    Ok(QF::and(vec![
                        QF::pred(p.values(vec![Value::object(&x)]).build().unwrap()),
                        QF::pred(q.values(vec![Value::bound(&params[0])]).build().unwrap()),
                    ]))
                })
                .build()
                .unwrap(),
        ));
        let nnf: Nnf<_> = formula.into();

        let correct_nnf = Nnf::exists(
            QuantifierBuilder::exists(vec![t.dupe()])
                .expression(|params| {
                    Ok(Nnf::or(vec![
                        Nnf::npred(p.values(vec![Value::object(&x)]).build().unwrap()),
                        Nnf::npred(q.values(vec![Value::bound(&params[0])]).build().unwrap()),
                    ]))
                })
                .build()
                .unwrap(),
        );

        assert_eq!(nnf, correct_nnf);
    }

    #[test]
    fn test_formula_to_nnf_exists_not() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("bar");

        let a = entities.get_or_create_object("a", &t);

        let p = PredicateBuilder::new("f").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("g").arguments(vec![t.dupe()]);

        let formula = QF::not(QF::exists(
            QuantifierBuilder::exists(vec![t.dupe()])
                .expression(|params| {
                    Ok(QF::or(vec![
                        QF::not(QF::pred(
                            p.values(vec![Value::bound(&params[0])]).build().unwrap(),
                        )),
                        QF::pred(q.values(vec![Value::object(&a)]).build().unwrap()),
                    ]))
                })
                .build()
                .unwrap(),
        ));
        let nnf: Nnf<_> = formula.into();

        let correct_nnf = Nnf::forall(
            QuantifierBuilder::forall(vec![t.dupe()])
                .expression(|params| {
                    Ok(Nnf::and(vec![
                        Nnf::pred(p.values(vec![Value::bound(&params[0])]).build().unwrap()),
                        Nnf::npred(q.values(vec![Value::object(&a)]).build().unwrap()),
                    ]))
                })
                .build()
                .unwrap(),
        );

        assert_eq!(nnf, correct_nnf);
    }

    #[test]
    fn test_formula_to_nnf_nested_quantifiers() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("baz");

        let p = PredicateBuilder::new("p").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);
        let r = PredicateBuilder::new("r").arguments(vec![t.dupe()]);

        // Original formula:
        // `NOT FORALL x. (p(x) OR ∃y. NOT (q(x) AND r(y)))`
        let formula = QF::not(QF::forall(
            QuantifierBuilder::forall(vec![t.dupe()])
                .expression(|params| {
                    let x = &params[0];
                    Ok(QF::or(vec![
                        QF::pred(p.values(vec![Value::bound(x)]).build().unwrap()),
                        QF::exists(
                            QuantifierBuilder::exists(vec![t.dupe()])
                                .expression(|params| {
                                    let y = &params[0];
                                    Ok(QF::not(QF::and(vec![
                                        QF::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                                        QF::pred(r.values(vec![Value::bound(y)]).build().unwrap()),
                                    ])))
                                })
                                .build()
                                .unwrap(),
                        ),
                    ]))
                })
                .build()
                .unwrap(),
        ));
        let nnf: Nnf<_> = formula.into();

        let correct_nnf = Nnf::exists(
            QuantifierBuilder::exists(vec![t.dupe()])
                .expression(|params| {
                    let x = &params[0];
                    Ok(Nnf::and(vec![
                        Nnf::npred(p.values(vec![Value::bound(x)]).build().unwrap()),
                        Nnf::forall(
                            QuantifierBuilder::forall(vec![t.dupe()])
                                .expression(|params| {
                                    let y = &params[0];
                                    Ok(Nnf::and(vec![
                                        Nnf::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                                        Nnf::pred(r.values(vec![Value::bound(y)]).build().unwrap()),
                                    ]))
                                })
                                .build()
                                .unwrap(),
                        ),
                    ]))
                })
                .build()
                .unwrap(),
        );

        assert_eq!(nnf, correct_nnf);
    }

    #[test]
    fn test_pdnf_exists_or() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let p = PredicateBuilder::new("p").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);

        let exists = QuantifierBuilder::exists(vec![t.dupe()])
            .expression(|params| {
                let x = &params[0];
                Ok(QF::or(vec![
                    QF::not(QF::pred(p.values(vec![Value::bound(x)]).build().unwrap())),
                    QF::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                ]))
            })
            .build()
            .unwrap();

        let formula = QF::exists(exists.clone());

        let pdnf: Pdnf<_> = formula.into();

        let quantifierless_formula = F::or(vec![
            F::not(F::pred(
                p.values(vec![Value::bound(&exists.variables[0])])
                    .build()
                    .unwrap(),
            )),
            F::pred(
                q.values(vec![Value::bound(&exists.variables[0])])
                    .build()
                    .unwrap(),
            ),
        ]);
        let correct_prefix = vec![QuantifierSymbol::Exists(exists.variables)];

        assert_eq!(pdnf.prefix, correct_prefix);
        let pdnf_matrix_tt = TruthTable::new(pdnf.matrix).collect::<Vec<_>>();
        let correct_pdnf_matrix_tt = TruthTable::new(quantifierless_formula).collect::<Vec<_>>();
        assert_eq!(pdnf_matrix_tt, correct_pdnf_matrix_tt);
    }

    #[test]
    fn test_pdnf_nested_quantifiers_and_or() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let p = PredicateBuilder::new("p").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);
        let r = PredicateBuilder::new("r").arguments(vec![t.dupe()]);

        let forall = QuantifierBuilder::forall(vec![t.dupe()])
            .expression(|params| {
                let x = &params[0];
                let inner = QuantifierBuilder::exists(vec![t.dupe()])
                    .expression(|params| {
                        let y = &params[0];
                        Ok(QF::and(vec![
                            QF::not(QF::pred(p.values(vec![Value::bound(x)]).build().unwrap())),
                            QF::or(vec![
                                QF::pred(q.values(vec![Value::bound(y)]).build().unwrap()),
                                QF::pred(r.values(vec![Value::bound(x)]).build().unwrap()),
                            ]),
                        ]))
                    })
                    .build()
                    .unwrap();
                Ok(QF::exists(inner))
            })
            .build()
            .unwrap();

        let formula = QF::forall(forall.clone());
        let pdnf: Pdnf<_> = formula.into();

        let x = &forall.variables[0];
        let QF::Quantifier(Quantifiers::Exists(inner)) = *forall.expression else {
            panic!("Not a quantifier.");
        };
        let y = &inner.variables[0];

        let quantifierless_formula = F::and(vec![
            F::not(F::pred(p.values(vec![Value::bound(x)]).build().unwrap())),
            F::or(vec![
                F::pred(q.values(vec![Value::bound(y)]).build().unwrap()),
                F::pred(r.values(vec![Value::bound(x)]).build().unwrap()),
            ]),
        ]);

        let correct_prefix = vec![
            QuantifierSymbol::ForAll(vec![x.clone()]),
            QuantifierSymbol::Exists(vec![y.clone()]),
        ];

        assert_eq!(pdnf.prefix, correct_prefix);
        let pdnf_matrix_tt = TruthTable::new(pdnf.matrix).collect::<Vec<_>>();
        let correct_pdnf_matrix_tt = TruthTable::new(quantifierless_formula).collect::<Vec<_>>();
        assert_eq!(pdnf_matrix_tt, correct_pdnf_matrix_tt);
    }

    #[test]
    fn test_pdnf_quantifiers_not_at_front() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let p = PredicateBuilder::new("p").arguments(vec![]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);
        let r = PredicateBuilder::new("r").arguments(vec![t.dupe()]);

        // Build `EXISTS x. (q(x) OR FORALL y. r(y))`
        let exists = QuantifierBuilder::exists(vec![t.dupe()])
            .expression(|params_x| {
                let x = &params_x[0];
                let inner = QuantifierBuilder::forall(vec![t.dupe()])
                    .expression(|params_y| {
                        let y = &params_y[0];
                        Ok(QF::pred(r.values(vec![Value::bound(y)]).build().unwrap()))
                    })
                    .build()
                    .unwrap();

                Ok(QF::or(vec![
                    QF::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                    QF::forall(inner),
                ]))
            })
            .build()
            .unwrap();

        // Final formula: `p AND EXISTS x. (q(x) OR FORALL y. r(y))`
        let formula = QF::and(vec![
            QF::pred(p.values(vec![]).build().unwrap()),
            QF::exists(exists.clone()),
        ]);

        let pdnf: Pdnf<_> = formula.into();

        let x = &exists.variables[0];
        let QF::Or(or) = *exists.expression else {
            panic!("Not an or.");
        };
        let QF::Quantifier(Quantifiers::ForAll(ref inner)) = or[1] else {
            panic!("Not a quantifier.");
        };
        let y = &inner.variables[0];

        let quantifierless_formula = F::and(vec![
            F::pred(p.values(vec![]).build().unwrap()),
            F::or(vec![
                F::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                F::pred(r.values(vec![Value::bound(y)]).build().unwrap()),
            ]),
        ]);

        let correct_prefix = vec![
            QuantifierSymbol::Exists(vec![x.clone()]),
            QuantifierSymbol::ForAll(vec![y.clone()]),
        ];

        assert_eq!(pdnf.prefix, correct_prefix);
        let pdnf_matrix_tt = TruthTable::new(pdnf.matrix).collect::<Vec<_>>();
        let correct_pdnf_matrix_tt = TruthTable::new(quantifierless_formula).collect::<Vec<_>>();
        assert_eq!(pdnf_matrix_tt, correct_pdnf_matrix_tt);
    }

    #[test]
    fn test_pdnf_multiple_quantifiers_same_level_nested() {
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("foo");

        let p = PredicateBuilder::new("p").arguments(vec![t.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t.dupe()]);
        let r = PredicateBuilder::new("r").arguments(vec![t.dupe()]);
        let s = PredicateBuilder::new("s").arguments(vec![]);

        // `EXISTS z. r(y)`
        let exists_y = QuantifierBuilder::exists(vec![t.dupe()])
            .expression(|params| {
                Ok(QF::pred(
                    r.values(vec![Value::bound(&params[0])]).build().unwrap(),
                ))
            })
            .build()
            .unwrap();

        // `EXISTS y. (q(x) OR EXISTS z. r(y))`
        let exists_x = QuantifierBuilder::exists(vec![t.dupe()])
            .expression(|params| {
                Ok(QF::or(vec![
                    QF::pred(q.values(vec![Value::bound(&params[0])]).build().unwrap()),
                    QF::exists(exists_y.clone()),
                ]))
            })
            .build()
            .unwrap();

        // `FORALL x. p(z)`
        let forall_z = QuantifierBuilder::forall(vec![t.dupe()])
            .expression(|params| {
                Ok(QF::pred(
                    p.values(vec![Value::bound(&params[0])]).build().unwrap(),
                ))
            })
            .build()
            .unwrap();

        // Full formula: `not((EXISTS x. (q(x) OR EXISTS y. r(y))) AND (FORALL z. p(z)) AND s)`
        let formula = QF::not(QF::and(vec![
            QF::exists(exists_x.clone()),
            QF::forall(forall_z.clone()),
            QF::pred(s.values(vec![]).build().unwrap()),
        ]));

        let pdnf: Pdnf<_> = formula.into();

        let x = &exists_x.variables[0];
        let y = &exists_y.variables[0];
        let z = &forall_z.variables[0];

        let quantifierless_formula = F::not(F::and(vec![
            F::or(vec![
                F::pred(q.values(vec![Value::bound(x)]).build().unwrap()),
                F::pred(r.values(vec![Value::bound(y)]).build().unwrap()),
            ]),
            F::pred(p.values(vec![Value::bound(z)]).build().unwrap()),
            F::pred(s.values(vec![]).build().unwrap()),
        ]));

        // Opposite quantifiers because of negation
        let correct_prefix = vec![
            QuantifierSymbol::ForAll(vec![x.clone()]),
            QuantifierSymbol::ForAll(vec![y.clone()]),
            QuantifierSymbol::Exists(vec![z.clone()]),
        ];

        assert_eq!(pdnf.prefix, correct_prefix);
        let pdnf_matrix_tt = TruthTable::new(pdnf.matrix).collect::<Vec<_>>();
        let correct_pdnf_matrix_tt = TruthTable::new(quantifierless_formula).collect::<Vec<_>>();
        assert_eq!(pdnf_matrix_tt, correct_pdnf_matrix_tt);
    }

    #[test]
    fn test_formula_evaluation_success() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let a = entities.get_or_create_object("a", &t1);
        let b1 = entities.get_or_create_object("b1", &t2);
        let b2 = entities.get_or_create_object("b2", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t1.dupe()]);

        let f = QF::forall(
            QuantifierBuilder::forall(vec![t1.dupe()])
                .expression(|x_params| {
                    Ok(QF::exists(
                        QuantifierBuilder::exists(vec![t2.dupe()])
                            .expression(|y_params| {
                                Ok(QF::and(vec![
                                    QF::pred(
                                        p.values(vec![
                                            GoalValue::bound(&x_params[0]),
                                            GoalValue::bound(&y_params[0]),
                                        ])
                                        .build()
                                        .unwrap(),
                                    ),
                                    QF::pred(
                                        q.values(vec![GoalValue::bound(&x_params[0])])
                                            .build()
                                            .unwrap(),
                                    ),
                                ]))
                            })
                            .build()
                            .unwrap(),
                    ))
                })
                .build()
                .unwrap(),
        );

        let pdnf: Pdnf<_> = f.into();

        let gp1 = p.values(vec![a.dupe(), b1.dupe()]).build().unwrap();
        let gp2 = p.values(vec![a.dupe(), b2.dupe()]).build().unwrap();
        let gq1 = q.values(vec![a.dupe()]).build().unwrap();

        let state = State::default().with_predicates(vec![gp1, gp2, gq1]);

        assert!(pdnf.eval(&state));
    }

    #[test]
    fn test_formula_evaluation_missing_q_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let a = entities.get_or_create_object("a", &t1);
        let b1 = entities.get_or_create_object("b1", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t1.dupe()]);

        let f = QF::forall(
            QuantifierBuilder::forall(vec![t1.dupe()])
                .expression(|x_params| {
                    Ok(QF::exists(
                        QuantifierBuilder::exists(vec![t2.dupe()])
                            .expression(|y_params| {
                                Ok(QF::and(vec![
                                    QF::pred(
                                        p.values(vec![
                                            GoalValue::bound(&x_params[0]),
                                            GoalValue::bound(&y_params[0]),
                                        ])
                                        .build()
                                        .unwrap(),
                                    ),
                                    QF::pred(
                                        q.values(vec![GoalValue::bound(&x_params[0])])
                                            .build()
                                            .unwrap(),
                                    ),
                                ]))
                            })
                            .build()
                            .unwrap(),
                    ))
                })
                .build()
                .unwrap(),
        );

        let pdnf: Pdnf<_> = f.into();

        let gp = p.values(vec![a.dupe(), b1.dupe()]).build().unwrap();

        // `q(a)` is missing
        let state = State::default().with_predicates(vec![gp]);

        assert!(!pdnf.eval(&state));
    }

    #[test]
    fn test_formula_evaluation_missing_p_match() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let a = entities.get_or_create_object("a", &t1);
        let _ = entities.get_or_create_object("b1", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t1.dupe()]);

        let f = QF::forall(
            QuantifierBuilder::forall(vec![t1.dupe()])
                .expression(|x_params| {
                    Ok(QF::exists(
                        QuantifierBuilder::exists(vec![t2.dupe()])
                            .expression(|y_params| {
                                Ok(QF::and(vec![
                                    QF::pred(
                                        p.values(vec![
                                            GoalValue::bound(&x_params[0]),
                                            GoalValue::bound(&y_params[0]),
                                        ])
                                        .build()
                                        .unwrap(),
                                    ),
                                    QF::pred(
                                        q.values(vec![GoalValue::bound(&x_params[0])])
                                            .build()
                                            .unwrap(),
                                    ),
                                ]))
                            })
                            .build()
                            .unwrap(),
                    ))
                })
                .build()
                .unwrap(),
        );

        let pdnf: Pdnf<_> = f.into();

        let gq = q.values(vec![a.dupe()]).build().unwrap();

        // No matching `p(a, b1)`
        let state = State::default().with_predicates(vec![gq]);

        assert!(!pdnf.eval(&state));
    }

    #[test]
    fn test_exists_with_or() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let a = entities.get_or_create_object("a", &t1);
        let b = entities.get_or_create_object("b", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);
        let q = PredicateBuilder::new("q").arguments(vec![t1.dupe()]);

        let f = QF::exists(
            QuantifierBuilder::exists(vec![t1.dupe()])
                .expression(|x_params| {
                    Ok(QF::or(vec![
                        QF::pred(
                            p.values(vec![GoalValue::bound(&x_params[0]), GoalValue::object(&b)])
                                .build()
                                .unwrap(),
                        ),
                        QF::pred(
                            q.values(vec![GoalValue::bound(&x_params[0])])
                                .build()
                                .unwrap(),
                        ),
                    ]))
                })
                .build()
                .unwrap(),
        );

        let pdnf: Pdnf<_> = f.into();

        // Only `q(a)` is true - should still satisfy the initial disjunction
        let gq = q.values(vec![a.dupe()]).build().unwrap();
        let state = State::default().with_predicates(vec![gq]);

        assert!(pdnf.eval(&state));
    }

    #[test]
    fn test_exists_forall_with_negation() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");

        let a = entities.get_or_create_object("a", &t1);
        let _ = entities.get_or_create_object("b1", &t2);
        let b2 = entities.get_or_create_object("b2", &t2);

        let p = PredicateBuilder::new("p").arguments(vec![t1.dupe(), t2.dupe()]);

        let f = QF::exists(
            QuantifierBuilder::exists(vec![t1.dupe()])
                .expression(|x_params| {
                    Ok(QF::forall(
                        QuantifierBuilder::forall(vec![t2.dupe()])
                            .expression(|y_params| {
                                Ok(QF::not(QF::pred(
                                    p.values(vec![
                                        GoalValue::bound(&x_params[0]),
                                        GoalValue::bound(&y_params[0]),
                                    ])
                                    .build()
                                    .unwrap(),
                                )))
                            })
                            .build()
                            .unwrap(),
                    ))
                })
                .build()
                .unwrap(),
        );

        let pdnf: Pdnf<_> = f.into();

        // No predicates - all `NOT p(x, y)` are true
        let state = State::default();

        assert!(pdnf.eval(&state));

        // Add p(a, b2) - now it should fail
        let gp = p.values(vec![a.dupe(), b2.dupe()]).build().unwrap();
        let state_with_violation = State::default().with_predicates(vec![gp]);

        assert!(!pdnf.eval(&state_with_violation));
    }
}
