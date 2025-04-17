use crate::{
    action::ActionParameter,
    calculus::{
        evaluation::{Evaluable, EvaluationContext},
        first_order::BoundVariable,
    },
    entity::{ObjectHandle, TypeHandle},
    sealed::Sealed,
    util::named::Named,
    InternerSymbol, Marker, INTERNER,
};
use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
    vec,
    vec::Vec,
};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::{CopyGetters, Getters};
use itertools::Itertools;

#[allow(private_bounds)]
pub trait IsPredicate<P: IsPredicate<P>>:
    Debug + Eq + Ord + Clone + Evaluable<P, P> + Sealed
{
    fn arguments(&self) -> &Vec<TypeHandle>;
    fn unique_marker(&self) -> Marker;
}

impl Sealed for Predicate {}
impl Sealed for ResolvedPredicate {}

impl IsPredicate<Predicate> for Predicate {
    fn arguments(&self) -> &Vec<TypeHandle> {
        &self.arguments
    }

    fn unique_marker(&self) -> Marker {
        self.unique_marker
    }
}

impl IsPredicate<ResolvedPredicate> for ResolvedPredicate {
    fn arguments(&self) -> &Vec<TypeHandle> {
        &self.arguments
    }

    fn unique_marker(&self) -> Marker {
        self.unique_marker
    }
}

/// A predicate how it is used in actions.
#[derive(Debug, Clone, Getters)]
pub struct Predicate {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    arguments: Vec<TypeHandle>,
    #[getset(get = "pub")]
    values: Vec<Value>,
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
}

impl Predicate {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// Creates [ResolvedPredicate]s from this one.
    ///
    /// Takes a mapping between action parameters and concrete objects to
    /// resolve with. The mapping can have additional action parameters
    /// even if they aren't used in this predicate.
    ///
    /// This method outputs multiple [ResolvedPredicate]s
    /// for each permutation of [BoundVariable]s in the original predicate.
    /// This simplifies handling of [BoundVariable]s, as this can only be
    /// called only after [Action](crate::action::Action)'s precondition
    /// was successfully resolved.
    pub fn into_resolved(
        &self,
        resolution: &BTreeMap<&ActionParameter, ObjectHandle>,
    ) -> Result<BTreeSet<ResolvedPredicate>, PredicateError> {
        self.values
            .iter()
            .map(|v| match v {
                Value::Object(o) => Ok(vec![o.dupe()]),
                Value::ActionParameter(ap) => resolution
                    .get(ap)
                    .ok_or(PredicateError::MissingResolutionParameter)
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
                    .map(|values| ResolvedPredicate {
                        name: self.name,
                        arguments: self.arguments.clone(),
                        values,
                        unique_marker: self.unique_marker,
                    })
                    .collect::<BTreeSet<ResolvedPredicate>>()
            })
    }

    /// Creates multiple [ResolvedPredicate]s
    /// for each permutation of [BoundVariable]s
    /// in the original predicate.
    ///
    /// WARN: for now should only be used for goal evaluation,
    /// so will panic when original predicate contains [ActionParameter]s.
    pub(crate) fn remove_bound(
        &self,
        var_assignment: &BTreeMap<BoundVariable, ObjectHandle>,
    ) -> Result<ResolvedPredicate, PredicateError> {
        self.values
            .iter()
            .map(|v| match v {
                Value::Object(o) => Ok(o.dupe()),
                Value::ActionParameter(_) => {
                    panic!("This should only be used for predicates in problem goal.")
                }
                Value::BoundVariable(bv) => var_assignment
                    .get(bv)
                    .map(|v| v.dupe())
                    .ok_or(PredicateError::MissingBoundVariable),
            })
            .collect::<Result<_, _>>()
            .map(|values| ResolvedPredicate {
                name: self.name,
                arguments: self.arguments.clone(),
                values,
                unique_marker: self.unique_marker,
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

impl Evaluable<Predicate, Predicate> for Predicate {
    fn eval(&self, context: &impl EvaluationContext<Predicate>) -> bool {
        context.eval(self)
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Predicate> + 'a> {
        Box::new(core::iter::once(self))
    }
}

impl Named for Predicate {
    fn name(&self) -> InternerSymbol {
        self.name
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arguments == other.arguments && self.values == other.values
    }
}

impl Eq for Predicate {}

impl Ord for Predicate {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        (&self.name, &self.arguments, &self.values).cmp(&(
            &other.name,
            &other.arguments,
            &other.values,
        ))
    }
}

impl PartialOrd for Predicate {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// A fully resolved [Predicate] how it is stored in a state.
#[derive(Debug, Clone, Getters)]
pub struct ResolvedPredicate {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    arguments: Vec<TypeHandle>,
    #[getset(get = "pub")]
    values: Vec<ObjectHandle>,
    #[getset(get = "pub")]
    unique_marker: Marker,
}

impl ResolvedPredicate {
    /// Checks whether this predicate is a possible resolution of another predicate
    pub fn is_resolution_of(&self, predicate: &Predicate) -> bool {
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

impl Evaluable<ResolvedPredicate, ResolvedPredicate> for ResolvedPredicate {
    fn eval(&self, context: &impl EvaluationContext<ResolvedPredicate>) -> bool {
        context.eval(self)
    }

    fn predicates<'a>(&'a self) -> Box<dyn Iterator<Item = &'a ResolvedPredicate> + 'a> {
        Box::new(core::iter::once(self))
    }
}

impl Named for ResolvedPredicate {
    fn name(&self) -> InternerSymbol {
        self.name
    }
}

impl PartialEq for ResolvedPredicate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arguments == other.arguments && self.values == other.values
    }
}

impl Eq for ResolvedPredicate {}

impl Ord for ResolvedPredicate {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        (&self.name, &self.arguments, &self.values).cmp(&(
            &other.name,
            &other.arguments,
            &other.values,
        ))
    }
}

impl PartialOrd for ResolvedPredicate {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub enum PredicateError {
    TypeMismatch,
    WrongNumberOfValues,
    MissingResolutionParameter,
    MissingBoundVariable,
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

#[allow(private_bounds)]
pub trait PredicateBuilderState: Sealed {}

#[derive(Debug, Clone)]
pub struct New;
#[derive(Debug, Clone)]
pub struct HasName;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HasArguments;
#[derive(Debug, Clone)]
pub struct HasValues;
#[derive(Debug, Clone)]
pub struct HasResolvedValues;

impl PredicateBuilderState for New {}
impl PredicateBuilderState for HasName {}
impl PredicateBuilderState for HasArguments {}
impl PredicateBuilderState for HasValues {}
impl PredicateBuilderState for HasResolvedValues {}

impl Sealed for New {}
impl Sealed for HasName {}
impl Sealed for HasArguments {}
impl Sealed for HasValues {}
impl Sealed for HasResolvedValues {}

#[derive(Debug, Clone, PartialEq, Eq, CopyGetters)]
pub struct PredicateBuilder<S: PredicateBuilderState> {
    #[getset(get_copy = "pub")]
    name: InternerSymbol,
    arguments: Vec<TypeHandle>,
    values: Vec<Value>,
    resolved_values: Vec<ObjectHandle>,
    state: PhantomData<S>,
}

impl PredicateBuilder<New> {
    pub fn new(name: &str) -> PredicateBuilder<HasName> {
        PredicateBuilder {
            name: INTERNER.lock().get_or_intern(name),
            arguments: Vec::new(),
            values: Vec::new(),
            resolved_values: Vec::new(),
            state: PhantomData,
        }
    }
}

impl PredicateBuilder<HasName> {
    pub fn arguments(self, arguments: Vec<TypeHandle>) -> PredicateBuilder<HasArguments> {
        PredicateBuilder {
            name: self.name,
            arguments,
            values: self.values,
            resolved_values: self.resolved_values,
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

impl PredicateBuilder<HasArguments> {
    pub fn arguments(&self) -> &[TypeHandle] {
        &self.arguments
    }

    pub fn values(&self, values: Vec<Value>) -> PredicateBuilder<HasValues> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values,
            resolved_values: Vec::new(),
            state: PhantomData,
        }
    }

    pub fn resolved_values(
        &self,
        resolved_values: Vec<ObjectHandle>,
    ) -> PredicateBuilder<HasResolvedValues> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values: Vec::new(),
            resolved_values,
            state: PhantomData,
        }
    }
}

impl PredicateBuilder<HasValues> {
    pub fn build(self) -> Result<Predicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments;
        let values = self.values;

        if arguments.len() != values.len() {
            return Err(PredicateError::WrongNumberOfValues);
        }

        if !values
            .iter()
            .map(|v| match v {
                Value::Object(o) => o.r#type(),
                Value::ActionParameter(ap) => ap.r#type.dupe(),
                Value::BoundVariable(var) => var.r#type.dupe(),
            })
            .zip(&arguments)
            .all(|(t, a)| t.inherits_or_eq(a))
        {
            return Err(PredicateError::TypeMismatch);
        }

        Ok(Predicate {
            name,
            arguments: arguments.to_vec(),
            values: values.to_vec(),
            unique_marker: Marker::new(),
        })
    }
}

impl PredicateBuilder<HasResolvedValues> {
    pub fn build(self) -> Result<ResolvedPredicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments;
        let values = self.resolved_values;

        if arguments.len() != values.len() {
            return Err(PredicateError::WrongNumberOfValues);
        }

        if !values
            .iter()
            .zip(&arguments)
            .all(|(v, a)| v.r#type().inherits_or_eq(a))
        {
            return Err(PredicateError::TypeMismatch);
        }

        Ok(ResolvedPredicate {
            name,
            arguments: arguments.to_vec(),
            values: values.to_vec(),
            unique_marker: Marker::new(),
        })
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::entity::{EntityStorage, ObjectStorage, TypeStorage};

    use super::*;
    use core::assert;

    #[test]
    fn test_equality() {
        // Same predicates, even though the marker is different for each one
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        assert!(p == p1);
        assert!(p.unique_marker != p1.unique_marker);

        // Different because of marker and name
        let p = PredicateBuilder::new("foo")
            .arguments(vec![])
            .values(vec![])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
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

        let p1 = p.resolved_values(vec![o1.dupe(), o1.dupe()]).build();
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

        let pr1 = p1.into_resolved(&BTreeMap::from([(&ap1, o2.dupe()), (&ap2, o1.dupe())]));
        assert!(matches!(pr1, Err(PredicateError::TypeMismatch)));

        let ap3 = ActionParameter {
            parameter_idx: 2,
            r#type: t2,
        };
        let pr1 = p1.into_resolved(&BTreeMap::from([
            (&ap1, o1.dupe()),
            (&ap2, o2.dupe()),
            (&ap3, o1.dupe()),
        ]));
        // More action parameters is not an error, they are ignored
        assert!(pr1.is_ok());

        let pr1 = p1.into_resolved(&BTreeMap::from([(&ap2, o2), (&ap3, o1)]));
        assert!(matches!(
            pr1,
            Err(PredicateError::MissingResolutionParameter)
        ));
    }

    #[test]
    fn test_builder_shortcut_equality() {
        let p1 = PredicateBuilder::new("foo")
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

        let pr1 = PredicateBuilder::new("foo")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        let pr2 = PredicateBuilder::new("foo")
            .arguments(vec![])
            .resolved_values(vec![])
            .build()
            .unwrap();
        assert_eq!(pr1, pr2);
    }

    #[test]
    fn test_resolution_equality() {
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
        let pr1 = p1.into_resolved(&BTreeMap::new()).unwrap();
        let pr2 = pb1.resolved_values(vec![o1, o2]).build().unwrap();

        assert_eq!(pr1, BTreeSet::from([pr2]));
    }
}
