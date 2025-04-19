use crate::{
    action::ActionParameter,
    calculus::{first_order::BoundVariable, Evaluable, EvaluationContext},
    entity::{ObjectHandle, TypeHandle},
    sealed::Sealed,
    util::named::Named,
    InternerSymbol, Marker, INTERNER,
};
use alloc::{
    collections::{BTreeMap, BTreeSet},
    vec,
    vec::Vec,
};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::{CopyGetters, Getters};
use itertools::Itertools;

#[derive(Debug)]
pub enum PredicateError {
    TypeMismatch,
    WrongNumberOfValues,
    MissingGroundingParameter,
    MissingBoundVariable,
}

#[allow(private_bounds)]
pub trait IsPredicate: Debug + Eq + Ord + Clone + Sealed {
    fn unique_marker(&self) -> Marker;
}

pub trait PredicateValue: Debug + Clone + PartialEq + Ord {
    fn r#type(&self) -> TypeHandle;
}

/// All values that a predicate can take when it appears in an action definition
#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Object(ObjectHandle),
    ActionParameter(ActionParameter),
    BoundVariable(BoundVariable),
}

impl Value {
    pub fn object(handle: &ObjectHandle) -> Self {
        Self::Object(handle.dupe())
    }

    pub fn param(param: &ActionParameter) -> Self {
        Self::ActionParameter(param.dupe())
    }

    pub fn bound(var: &BoundVariable) -> Self {
        Self::BoundVariable(var.dupe())
    }
}

impl PredicateValue for Value {
    fn r#type(&self) -> TypeHandle {
        match self {
            Value::Object(o) => o.r#type(),
            Value::ActionParameter(ap) => ap.r#type.dupe(),
            Value::BoundVariable(bv) => bv.r#type.dupe(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum EvalStrat {
    State,
    Equality,
}

/// A predicate how it is used in actions.
#[derive(Debug, Clone, Getters)]
pub struct Predicate<V: PredicateValue> {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    arguments: Vec<TypeHandle>,
    #[getset(get = "pub")]
    values: Vec<V>,
    /// A marker to help distinguish real predicates from copies
    /// made when transforming an expression.
    ///
    /// Used to distinguish predicates with the same name and arguments
    /// when doing normalization. For example:
    /// `p XOR q` can be transformed into the following CNF: `(NOT p OR NOT q) AND (p OR q)`
    /// In this example even if p and q are the same predicate,
    /// they are still independent variables in the expression.
    /// On the other hand, in the CNF above the two `p`s and `q`s are the same predicate
    /// __and__ the same variables, and the expression should not be treated as having 4 inputs,
    /// but only 2.
    #[getset(get = "pub")]
    unique_marker: Marker,
    eval_strat: EvalStrat,
}

impl Evaluable<GroundedPredicate> for GroundedPredicate {
    fn eval(&self, context: &impl EvaluationContext<GroundedPredicate>) -> bool {
        match self.eval_strat {
            EvalStrat::State => context
                .matching_predicates(&self.into())
                .map(|preds| preds.iter().any(|v| v == self))
                .unwrap_or(false),
            EvalStrat::Equality => {
                let mut iter = self.values.iter();
                if let Some(first) = iter.next() {
                    iter.all(|e| e == first)
                } else {
                    true
                }
            }
        }
    }
}

impl<V: PredicateValue> IsPredicate for Predicate<V> {
    default fn unique_marker(&self) -> Marker {
        self.unique_marker
    }
}

impl<V: PredicateValue> Named for Predicate<V> {
    fn name(&self) -> InternerSymbol {
        self.name
    }
}

impl<V: PredicateValue> PartialEq for Predicate<V> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arguments == other.arguments && self.values == other.values
    }
}

impl<V: PredicateValue> Eq for Predicate<V> {}

impl<V: PredicateValue> Ord for Predicate<V> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        (&self.name, &self.arguments, &self.values).cmp(&(
            &other.name,
            &other.arguments,
            &other.values,
        ))
    }
}

impl<V: PredicateValue> PartialOrd for Predicate<V> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<V: PredicateValue> Sealed for Predicate<V> {}

pub type LiftedPredicate = Predicate<Value>;

/// Variable in a predicate
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiftedValue<'a> {
    ActionParameter(usize, &'a ActionParameter),
    BoundVariable(usize, &'a BoundVariable),
}

impl LiftedValue<'_> {
    pub fn idx(&self) -> usize {
        match self {
            LiftedValue::ActionParameter(i, _) | LiftedValue::BoundVariable(i, _) => *i,
        }
    }
}

impl LiftedPredicate {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// Creates [GroundedPredicate]s from this one.
    ///
    /// Takes a mapping between action parameters and concrete objects to
    /// ground with. The mapping can have additional action parameters
    /// even if they aren't used in this predicate.
    ///
    /// This method outputs multiple [GroundedPredicate]s
    /// for each permutation of [BoundVariable]s in the original predicate.
    /// This simplifies handling of [BoundVariable]s, as this can only be
    /// called only after [Action](crate::action::Action)'s precondition
    /// was successfully grounded.
    pub fn into_grounded(
        &self,
        grounding: &BTreeMap<&ActionParameter, ObjectHandle>,
    ) -> Result<BTreeSet<GroundedPredicate>, PredicateError> {
        self.values
            .iter()
            .map(|v| match v {
                Value::Object(o) => Ok(vec![o.dupe()]),
                Value::ActionParameter(ap) => grounding
                    .get(ap)
                    .ok_or(PredicateError::MissingGroundingParameter)
                    .and_then(|v| {
                        if v.r#type().inherits_or_eq(&ap.r#type) {
                            Ok(vec![v.dupe()])
                        } else {
                            Err(PredicateError::TypeMismatch)
                        }
                    }),
                Value::BoundVariable(bv) => Ok(bv.r#type.get_objects()),
            })
            .collect::<Result<Vec<Vec<ObjectHandle>>, PredicateError>>()
            .map(|values| {
                values
                    .into_iter()
                    .multi_cartesian_product()
                    .map(|values| GroundedPredicate {
                        name: self.name,
                        arguments: self.arguments.clone(),
                        values,
                        unique_marker: self.unique_marker,
                        eval_strat: self.eval_strat,
                    })
                    .collect::<BTreeSet<GroundedPredicate>>()
            })
    }

    /// Returns only [LiftedValue]s
    /// and their indices from this predicate's values array
    pub fn lifted_values(&self) -> Vec<LiftedValue<'_>> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| match v {
                Value::Object(_) => None,
                Value::ActionParameter(ap) => Some(LiftedValue::ActionParameter(i, ap)),
                Value::BoundVariable(bp) => Some(LiftedValue::BoundVariable(i, bp)),
            })
            .collect::<Vec<_>>()
    }

    /// Returns only [ActionParameter]s
    /// and their indices from this predicate's values array
    pub fn action_parameters(&self) -> Vec<(usize, &ActionParameter)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| match v {
                Value::Object(_) => None,
                Value::ActionParameter(ap) => Some((i, ap)),
                Value::BoundVariable(_) => None,
            })
            .collect::<Vec<_>>()
    }
}

impl PredicateValue for ObjectHandle {
    fn r#type(&self) -> TypeHandle {
        self.r#type()
    }
}

/// A [LiftedPredicate] where all [LiftedValue]s
/// were replaced by concrete [Object](ObjectHandle)s.
pub type GroundedPredicate = Predicate<ObjectHandle>;

impl GroundedPredicate {
    /// Checks whether this predicate is a possible groundings of another predicate
    pub fn is_grounded_from(&self, predicate: &LiftedPredicate) -> bool {
        if predicate.name() != self.name() || predicate.arguments().len() != self.arguments().len()
        {
            return false;
        }

        self.values()
            .iter()
            .zip(predicate.values())
            .all(|(ro, v)| match v {
                Value::Object(o) => o == ro,
                Value::ActionParameter(ap) => ro.r#type().inherits_or_eq(&ap.r#type),
                Value::BoundVariable(var) => ro.r#type().inherits_or_eq(&var.r#type),
            })
    }
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub enum GoalValue {
    Object(ObjectHandle),
    BoundVariable(BoundVariable),
}

impl GoalValue {
    pub fn object(handle: &ObjectHandle) -> Self {
        Self::Object(handle.dupe())
    }

    pub fn bound(var: &BoundVariable) -> Self {
        Self::BoundVariable(var.dupe())
    }
}

impl PredicateValue for GoalValue {
    fn r#type(&self) -> TypeHandle {
        match self {
            GoalValue::Object(o) => o.r#type(),
            GoalValue::BoundVariable(bv) => bv.r#type.dupe(),
        }
    }
}

pub type GoalPredicate = Predicate<GoalValue>;

impl GoalPredicate {
    /// Creates a [GroundedPredicate] with the [BoundVariable]s
    /// in `var_assignment`.
    pub(crate) fn remove_bound(
        &self,
        var_assignment: &BTreeMap<BoundVariable, ObjectHandle>,
    ) -> Result<GroundedPredicate, PredicateError> {
        self.values
            .iter()
            .map(|v| match v {
                GoalValue::Object(o) => Ok(o.dupe()),
                GoalValue::BoundVariable(bv) => var_assignment
                    .get(bv)
                    .map(|v| v.dupe())
                    .ok_or(PredicateError::MissingBoundVariable),
            })
            .collect::<Result<_, _>>()
            .map(|values| GroundedPredicate {
                name: self.name,
                arguments: self.arguments.clone(),
                values,
                unique_marker: self.unique_marker,
                eval_strat: self.eval_strat,
            })
    }
}

#[allow(private_bounds)]
pub trait PredicateBuilderState: Sealed {}

#[derive(Debug, Clone)]
pub struct New;
#[derive(Debug, Clone)]
pub struct Equality;
#[derive(Debug, Clone)]
pub struct HasName;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HasArguments;
#[derive(Debug, Clone)]
pub struct HasValues;
#[derive(Debug, Clone)]
pub struct HasGroundedValues;

impl PredicateBuilderState for New {}
impl PredicateBuilderState for Equality {}
impl PredicateBuilderState for HasName {}
impl PredicateBuilderState for HasArguments {}
impl PredicateBuilderState for HasValues {}

impl Sealed for New {}
impl Sealed for Equality {}
impl Sealed for HasName {}
impl Sealed for HasArguments {}
impl Sealed for HasValues {}

#[derive(Debug, Clone, PartialEq, Eq, CopyGetters)]
pub struct PredicateBuilder<S: PredicateBuilderState> {
    #[getset(get_copy = "pub")]
    name: InternerSymbol,
    arguments: Vec<TypeHandle>,
    values: Vec<ObjectHandle>,
    state: PhantomData<S>,
}

impl PredicateBuilder<New> {
    pub fn new(name: &str) -> PredicateBuilder<HasName> {
        PredicateBuilder {
            name: INTERNER.lock().get_or_intern(name),
            arguments: Vec::new(),
            values: Vec::new(),
            state: PhantomData,
        }
    }

    pub fn equality<V: PredicateValue>(first: V, second: V) -> Predicate<V> {
        Predicate {
            name: INTERNER.lock().get_or_intern("="),
            arguments: vec![first.r#type(), second.r#type()],
            values: vec![first, second],
            unique_marker: Marker::new(),
            eval_strat: EvalStrat::Equality,
        }
    }
}

impl PredicateBuilder<HasName> {
    pub fn arguments(self, arguments: Vec<TypeHandle>) -> PredicateBuilder<HasArguments> {
        PredicateBuilder {
            name: self.name,
            arguments,
            values: self.values,
            state: PhantomData,
        }
    }
}

pub type PredicateDefinition = PredicateBuilder<HasArguments>;

impl Named for PredicateDefinition {
    fn name(&self) -> InternerSymbol {
        self.name
    }
}

impl PredicateDefinition {
    pub fn arguments(&self) -> &[TypeHandle] {
        &self.arguments
    }
}

pub struct PredicateFinalizer<V: PredicateValue> {
    definition: PredicateDefinition,
    values: Vec<V>,
}

impl PredicateBuilder<HasArguments> {
    pub fn values<V: PredicateValue>(&self, values: Vec<V>) -> PredicateFinalizer<V> {
        PredicateFinalizer {
            definition: self.clone(),
            values,
        }
    }
}

impl<V: PredicateValue> PredicateFinalizer<V> {
    pub fn build(self) -> Result<Predicate<V>, PredicateError> {
        let name = self.definition.name;
        let arguments = self.definition.arguments;
        let values = self.values;

        if arguments.len() != values.len() {
            return Err(PredicateError::WrongNumberOfValues);
        }

        if !values
            .iter()
            .map(|v| v.r#type())
            .zip(&arguments)
            .all(|(t, a)| t.inherits_or_eq(a))
        {
            return Err(PredicateError::TypeMismatch);
        }

        Ok(Predicate {
            name,
            arguments: arguments.to_vec(),
            values,
            unique_marker: Marker::new(),
            eval_strat: EvalStrat::State,
        })
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{
        entity::{EntityStorage, ObjectStorage, TypeStorage},
        state::State,
    };

    use super::*;
    use core::assert;

    #[test]
    fn test_predicate_equality() {
        // Same predicates, even though the marker is different for each one
        let p: LiftedPredicate = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1: LiftedPredicate = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        assert!(p == p1);
        assert!(p.unique_marker != p1.unique_marker);

        // Different because of marker and name
        let p: LiftedPredicate = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1: LiftedPredicate = PredicateBuilder::new("bar")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();

        assert!(p != p1);

        // Same, because of the marker and all other args
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");
        let x = entities.get_or_create_object("x", &t);

        let p = PredicateBuilder::new("foo")
            .arguments(vec![t.clone()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments(vec![t.clone()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t1 = entities.get_or_create_type("t1");
        let y = entities.get_or_create_object("y", &t1);

        let p = PredicateBuilder::new("foo")
            .arguments(vec![t.clone()])
            .values(vec![Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments(vec![t1.clone()])
            .values(vec![Value::object(&y)])
            .build()
            .unwrap();
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);
    }

    #[test]
    fn test_builder() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");
        let _ = entities.create_inheritance(&t2, &t1);
        let o1 = entities.get_or_create_object("o1", &t1);
        let o2 = entities.get_or_create_object("o2", &t2);

        let p = PredicateBuilder::new("foo").arguments(vec![t1.clone(), t2.clone()]);

        let p1 = p
            .values(vec![Value::object(&o1), Value::object(&o1)])
            .build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let p1 = p.values(vec![o1.dupe(), o1.dupe()]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let ap1 = ActionParameter {
            parameter_idx: 0,
            r#type: t1.clone(),
        };
        let ap2 = ActionParameter {
            parameter_idx: 1,
            r#type: t2.dupe(),
        };

        let p1 = p
            .values(vec![Value::param(&ap1), Value::param(&ap1)])
            .build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));
        let p1 = p
            .values(vec![Value::param(&ap1), Value::param(&ap2)])
            .build()
            .unwrap();

        let gp1 = p1.into_grounded(&BTreeMap::from([(&ap1, o2.dupe()), (&ap2, o1.dupe())]));
        assert!(matches!(gp1, Err(PredicateError::TypeMismatch)));

        let ap3 = ActionParameter {
            parameter_idx: 2,
            r#type: t2,
        };
        let gp1 = p1.into_grounded(&BTreeMap::from([
            (&ap1, o1.dupe()),
            (&ap2, o2.dupe()),
            (&ap3, o1.dupe()),
        ]));
        // More action parameters is not an error, they are ignored
        assert!(gp1.is_ok());

        let gp1 = p1.into_grounded(&BTreeMap::from([(&ap2, o2), (&ap3, o1)]));
        assert!(matches!(
            gp1,
            Err(PredicateError::MissingGroundingParameter)
        ));
    }

    #[test]
    fn test_builder_shortcut_equality() {
        let p1: LiftedPredicate = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        assert_eq!(p1, p2);

        let gp1: LiftedPredicate = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let gp2 = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        assert_eq!(gp1, gp2);
    }

    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    #[test]
    fn test_grounding_equality() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");
        let _ = entities.create_inheritance(&t2, &t1);
        let o1 = entities.get_or_create_object("o1", &t1);
        let o2 = entities.get_or_create_object("o2", &t2);

        let pb1 = PredicateBuilder::new("foo").arguments(vec![t1.clone(), t2.clone()]);
        let p1 = pb1
            .values(vec![Value::object(&o1), Value::object(&o2)])
            .build()
            .unwrap();
        let gp1 = p1.into_grounded(&BTreeMap::new()).unwrap();
        let gp2 = pb1.values(vec![o1, o2]).build().unwrap();

        assert_eq!(gp1, BTreeSet::from([gp2]));
    }

    #[test]
    fn test_equality_predicate() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");
        let t3 = entities.get_or_create_type("t3");
        let _ = entities.create_inheritance(&t2, &t1);
        let o1 = entities.get_or_create_object("o1", &t1);
        let o2 = entities.get_or_create_object("o2", &t2);
        let o3 = entities.get_or_create_object("o3", &t3);

        let state = State::default();

        let p = PredicateBuilder::equality(o1.dupe(), o1.dupe());
        assert!(p.eval(&state));

        let p = PredicateBuilder::equality(o1.dupe(), o2.dupe());
        assert!(!p.eval(&state));

        let p = PredicateBuilder::equality(o1.dupe(), o3.dupe());
        assert!(!p.eval(&state));
    }
}
