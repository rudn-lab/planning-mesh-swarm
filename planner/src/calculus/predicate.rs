use crate::{
    action::ActionParameter,
    calculus::evaluation::{Evaluable, EvaluationContext},
    entity::{ObjectHandle, TypeHandle},
    sealed::Sealed,
    util::named::Named,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{boxed::Box, collections::BTreeMap, vec::Vec};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::{CopyGetters, Getters};
use rand::Rng;

#[allow(private_bounds)]
pub trait IsPredicate<P: IsPredicate<P>>: Eq + Ord + Clone + Evaluable<P> + Sealed {
    fn unique_marker(&self) -> u32;
}

impl Sealed for Predicate {}
impl Sealed for ResolvedPredicate {}

impl IsPredicate<Predicate> for Predicate {
    fn unique_marker(&self) -> u32 {
        self.unique_marker
    }
}

impl IsPredicate<ResolvedPredicate> for ResolvedPredicate {
    fn unique_marker(&self) -> u32 {
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
    unique_marker: u32,
}

impl Predicate {
    #[allow(
        clippy::mutable_key_type,
        reason = "See SmartHandle Hash and Ord impls."
    )]
    /// Creates a new resolved predicate.
    ///
    /// Takes a mapping between action parameters and concrete objects to
    /// resolve with. Crucially, the mapping can have additional action parameters
    /// even if they aren't used in this predicate.
    pub fn into_resolved<'a>(
        &'a self,
        resolution: &'a BTreeMap<&'a ActionParameter, ObjectHandle>,
    ) -> Result<ResolvedPredicate, PredicateError> {
        self.values
            .iter()
            .map(|v| match v {
                Value::Object(o) => Ok(o),
                Value::ActionParameter(ap) => resolution
                    .get(ap)
                    .ok_or(PredicateError::MissingResolutionParameter)
                    .and_then(|v| {
                        if v.r#type().inherits_or_eq(&ap.r#type) {
                            Ok(v)
                        } else {
                            Err(PredicateError::TypeMismatch)
                        }
                    }),
            })
            .map(|v| v.cloned())
            .collect::<Result<Vec<ObjectHandle>, PredicateError>>()
            .map(|values| ResolvedPredicate {
                name: self.name,
                arguments: self.arguments.clone(),
                values,
                unique_marker: self.unique_marker,
            })
    }

    /// Returns only [Value::ActionParameter]
    /// and their indices from this predicate's values array
    pub fn action_parameters(&self) -> Vec<(usize, &ActionParameter)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| match v {
                Value::Object(_) => None,
                Value::ActionParameter(ap) => Some((i, ap)),
            })
            .collect::<Vec<_>>()
    }
}

impl Evaluable<Predicate> for Predicate {
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

/// A fully resolved predicate how it is stored in a state.
#[derive(Debug, Clone, Getters)]
pub struct ResolvedPredicate {
    #[getset(get = "pub")]
    name: InternerSymbol,
    #[getset(get = "pub")]
    arguments: Vec<TypeHandle>,
    #[getset(get = "pub")]
    values: Vec<ObjectHandle>,
    #[getset(get = "pub")]
    unique_marker: u32,
}

impl ResolvedPredicate {
    /// Checks whether this predicate is a possible resolution of another predicate
    pub fn is_resolution_of(&self, predicate: &Predicate) -> bool {
        if predicate.name() != self.name() || predicate.arguments().len() != self.arguments().len()
        {
            return false;
        }

        predicate
            .values()
            .iter()
            .zip(self.values())
            .all(|(av, v)| match av {
                Value::Object(o) => o == v,
                Value::ActionParameter(ap) => v.r#type().inherits_or_eq(&ap.r#type),
            })
    }
}

impl Evaluable<ResolvedPredicate> for ResolvedPredicate {
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
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Object(ObjectHandle),
    ActionParameter(ActionParameter),
}

impl Value {
    pub fn object(handle: &ObjectHandle) -> Self {
        Self::Object(handle.dupe())
    }

    pub fn param(param: &ActionParameter) -> Self {
        Self::ActionParameter(param.dupe())
    }
}

#[allow(private_bounds)]
pub trait PredicateBuilderState: Sealed {}

#[derive(Debug, Clone)]
pub struct New;
#[derive(Debug, Clone)]
pub struct HasName;
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, CopyGetters)]
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
    pub fn arguments(self, arguments: &[TypeHandle]) -> PredicateBuilder<HasArguments> {
        PredicateBuilder {
            name: self.name,
            arguments: arguments.to_vec(),
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

    pub fn values(&self, values: &[Value]) -> PredicateBuilder<HasValues> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values: values.to_vec(),
            resolved_values: Vec::new(),
            state: PhantomData,
        }
    }

    pub fn resolved_values(&self, values: &[ObjectHandle]) -> PredicateBuilder<HasResolvedValues> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values: Vec::new(),
            resolved_values: values.to_vec(),
            state: PhantomData,
        }
    }
}

impl PredicateBuilder<HasValues> {
    pub fn build(self) -> Result<Predicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments;
        let unique_marker = RANDOM.lock().gen();
        let values = self.values;

        if arguments.len() != values.len() {
            return Err(PredicateError::WrongNumberOfValues);
        }

        if !values
            .iter()
            .map(|v| match v {
                Value::Object(o) => o.r#type(),
                Value::ActionParameter(ap) => ap.r#type.dupe(),
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
            unique_marker,
        })
    }
}

impl PredicateBuilder<HasResolvedValues> {
    pub fn build(self) -> Result<ResolvedPredicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments;
        let unique_marker = RANDOM.lock().gen();
        let values = self.resolved_values;

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
            unique_marker,
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
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("foo")
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        assert!(p == p1);
        assert!(p.unique_marker != p1.unique_marker);

        // Different because of marker and name
        let p = PredicateBuilder::new("foo")
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        let p1 = PredicateBuilder::new("bar")
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();

        assert!(p != p1);

        // Same, because of the marker and all other args
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");
        let x = entities.get_or_create_object("x", &t);

        let p = PredicateBuilder::new("foo")
            .arguments(&[t.clone()])
            .values(&[Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments(&[t.clone()])
            .values(&[Value::object(&x)])
            .build()
            .unwrap();
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t1 = entities.get_or_create_type("t1");
        let y = entities.get_or_create_object("y", &t1);

        let p = PredicateBuilder::new("foo")
            .arguments(&[t.clone()])
            .values(&[Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments(&[t1.clone()])
            .values(&[Value::object(&y)])
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

        let p = PredicateBuilder::new("foo").arguments(&[t1.clone(), t2.clone()]);

        let p1 = p.values(&[Value::object(&o1), Value::object(&o1)]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let p1 = p.resolved_values(&[o1.dupe(), o1.dupe()]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let ap1 = ActionParameter {
            parameter_idx: 0,
            r#type: t1.clone(),
        };
        let ap2 = ActionParameter {
            parameter_idx: 1,
            r#type: t2.dupe(),
        };

        let p1 = p.values(&[Value::param(&ap1), Value::param(&ap1)]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));
        let p1 = p
            .values(&[Value::param(&ap1), Value::param(&ap2)])
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
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("foo")
            .arguments(&[])
            .values(&[])
            .build()
            .unwrap();
        assert_eq!(p1, p2);

        let pr1 = PredicateBuilder::new("foo")
            .arguments(&[])
            .resolved_values(&[])
            .build()
            .unwrap();
        let pr2 = PredicateBuilder::new("foo")
            .arguments(&[])
            .resolved_values(&[])
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

        let pb1 = PredicateBuilder::new("foo").arguments(&[t1.clone(), t2.clone()]);
        let p1 = pb1
            .values(&[Value::object(&o1), Value::object(&o2)])
            .build()
            .unwrap();
        let pr1 = p1.into_resolved(&BTreeMap::new()).unwrap();
        let pr2 = pb1.resolved_values(&[o1, o2]).build().unwrap();

        assert_eq!(pr1, pr2);
    }
}
