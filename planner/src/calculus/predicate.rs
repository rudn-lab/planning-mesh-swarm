use crate::{
    action::ActionParameter,
    entity::{ObjectHandle, TypeHandle},
    evaluation::{Evaluable, EvaluationContext},
    sealed::Sealed,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{vec, vec::Vec};
use core::{fmt::Debug, marker::PhantomData};
use gazebo::dupe::Dupe;
use getset::Getters;
use rand::Rng;

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

#[derive(Debug)]
pub enum PredicateError {
    TypeMismatch,
    WrongNumberOfResolutions,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Object(ObjectHandle),
    ActionParam(ActionParameter),
}

#[derive(Debug, Clone)]
pub struct PredicateBuilder<S: PredicateBuilderState, const N: usize> {
    name: InternerSymbol,
    arguments: Option<[TypeHandle; N]>,
    values: Option<[Value; N]>,
    resolved_values: Option<[ObjectHandle; N]>,
    state: PhantomData<S>,
}

impl<const N: usize> PredicateBuilder<New, N> {
    pub fn new(name: &str) -> PredicateBuilder<HasName, N> {
        PredicateBuilder {
            name: INTERNER.lock().get_or_intern(name),
            arguments: None,
            values: None,
            resolved_values: None,
            state: PhantomData,
        }
    }
}

impl<const N: usize> PredicateBuilder<HasName, N> {
    pub fn arguments(self, arguments: [&TypeHandle; N]) -> PredicateBuilder<HasArguments, N> {
        PredicateBuilder {
            name: self.name,
            arguments: Some(arguments.map(Dupe::dupe)),
            values: None,
            resolved_values: None,
            state: PhantomData,
        }
    }
}

impl<const N: usize> PredicateBuilder<HasArguments, N> {
    pub fn values(&self, values: [Value; N]) -> PredicateBuilder<HasValues, N> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values: Some(values),
            resolved_values: None,
            state: PhantomData,
        }
    }

    pub fn resolved_values(
        &self,
        values: [&ObjectHandle; N],
    ) -> PredicateBuilder<HasResolvedValues, N> {
        PredicateBuilder {
            name: self.name,
            arguments: self.arguments.clone(),
            values: None,
            resolved_values: Some(values.map(Dupe::dupe)),
            state: PhantomData,
        }
    }
}

impl PredicateBuilder<HasArguments, 0> {
    pub fn build(self) -> Predicate {
        let unique_marker = RANDOM.lock().gen();
        Predicate {
            name: self.name,
            arguments: self.arguments.unwrap().to_vec(),
            values: vec![],
            unique_marker,
        }
    }

    pub fn build_resolved(self) -> ResolvedPredicate {
        let unique_marker = RANDOM.lock().gen();
        ResolvedPredicate {
            name: self.name,
            arguments: self.arguments.unwrap().to_vec(),
            values: vec![],
            unique_marker,
        }
    }
}

impl<const N: usize> PredicateBuilder<HasValues, N> {
    pub fn build(self) -> Result<Predicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments.unwrap();
        let unique_marker = RANDOM.lock().gen();
        let values = self.values.unwrap();

        if !values.iter().zip(&arguments).all(|(v, a)| match v {
            Value::Object(o) => o.r#type().inherits_or_eq(a),
            Value::ActionParam(ap) => ap.r#type.inherits_or_eq(a),
        }) {
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

impl<const N: usize> PredicateBuilder<HasResolvedValues, N> {
    pub fn build(self) -> Result<ResolvedPredicate, PredicateError> {
        let name = self.name;
        let arguments = self.arguments.unwrap();
        let unique_marker = RANDOM.lock().gen();
        let values = self.resolved_values.unwrap();

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

impl Value {
    pub fn object(handle: &ObjectHandle) -> Self {
        Self::Object(handle.dupe())
    }

    pub fn param(param: &ActionParameter) -> Self {
        Self::ActionParam(param.dupe())
    }
}

/// A predicate how it is used in actions.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, Getters)]
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
    /// Creates a new resolved predicate.
    ///
    /// # Arguments
    ///
    /// * `resolution` - all missing values, correspongind to [Value::ActionParam]
    pub fn into_resolved(
        &self,
        resolution: &[&ObjectHandle],
    ) -> Result<ResolvedPredicate, PredicateError> {
        if self
            .values
            .iter()
            .filter(|v| matches!(v, Value::ActionParam(_)))
            .count()
            != resolution.len()
        {
            return Err(PredicateError::WrongNumberOfResolutions);
        }

        if !self
            .values
            .iter()
            .filter_map(|v| match v {
                Value::Object(_) => None,
                Value::ActionParam(ap) => Some(ap),
            })
            .zip(resolution)
            .all(|(v, r)| v.r#type == r.r#type())
        {
            return Err(PredicateError::TypeMismatch);
        }

        let mut resolution = resolution.iter();
        Ok(ResolvedPredicate {
            name: self.name,
            arguments: self.arguments.clone(),
            values: self
                .values
                .iter()
                .map(|v| match v {
                    Value::Object(o) => o,
                    Value::ActionParam(_) => *resolution.next().unwrap(),
                })
                .map(Dupe::dupe)
                .collect(),
            unique_marker: self.unique_marker,
        })
    }
}

impl Evaluable for Predicate {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(self)
    }

    fn predicates(&self) -> Vec<Predicate> {
        vec![self.clone()]
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arguments == other.arguments && self.values == other.values
    }
}

/// A fully resolved predicate how it is stored in a state.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, Getters)]
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

impl PartialEq for ResolvedPredicate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arguments == other.arguments && self.values == other.values
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{action::ParameterHandle, entity::EntityStorage};

    use super::*;
    use core::assert;

    #[test]
    fn test_equality() {
        // Same predicates, even though the marker is different for each one
        let p = PredicateBuilder::new("foo").arguments([]).build();
        let p1 = PredicateBuilder::new("foo").arguments([]).build();
        assert!(p == p1);
        assert!(p.unique_marker != p1.unique_marker);

        // Different because of marker and name
        let p = PredicateBuilder::new("foo").arguments([]).build();
        let p1 = PredicateBuilder::new("bar").arguments([]).build();

        assert!(p != p1);

        // Same, because of the marker and all other args
        let mut entities = EntityStorage::default();
        let t = entities.get_or_create_type("t");
        let x = entities.get_or_create_object("x", &t);

        let p = PredicateBuilder::new("foo")
            .arguments([&t])
            .values([Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments([&t])
            .values([Value::object(&x)])
            .build()
            .unwrap();
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t1 = entities.get_or_create_type("t1");
        let y = entities.get_or_create_object("y", &t1);

        let p = PredicateBuilder::new("foo")
            .arguments([&t])
            .values([Value::object(&x)])
            .build()
            .unwrap();
        let mut p1 = PredicateBuilder::new("foo")
            .arguments([&t1])
            .values([Value::object(&y)])
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

        let p = PredicateBuilder::new("foo").arguments([&t1, &t2]);

        let p1 = p.values([Value::object(&o1), Value::object(&o1)]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let p1 = p.resolved_values([&o1, &o1]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));

        let ap1 = ActionParameter {
            parameter_handle: ParameterHandle { idx: 0 },
            r#type: t1.clone(),
        };
        let ap2 = ActionParameter {
            parameter_handle: ParameterHandle { idx: 1 },
            r#type: t2,
        };

        let p1 = p.values([Value::param(&ap1), Value::param(&ap1)]).build();
        assert!(matches!(p1, Err(PredicateError::TypeMismatch)));
        let p1 = p
            .values([Value::param(&ap1), Value::param(&ap2)])
            .build()
            .unwrap();

        let pr1 = p1.into_resolved(&[&o2, &o1]);
        assert!(matches!(pr1, Err(PredicateError::TypeMismatch)));

        let pr1 = p1.into_resolved(&[&o1, &o2, &o1]);
        assert!(matches!(pr1, Err(PredicateError::WrongNumberOfResolutions)));
    }

    #[test]
    fn test_builder_shortcut_equality() {
        let p1 = PredicateBuilder::new("foo")
            .arguments([])
            .values([])
            .build()
            .unwrap();
        let p2 = PredicateBuilder::new("foo").arguments([]).build();
        assert_eq!(p1, p2);

        let pr1 = PredicateBuilder::new("foo")
            .arguments([])
            .resolved_values([])
            .build()
            .unwrap();
        let pr2 = PredicateBuilder::new("foo").arguments([]).build_resolved();
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

        let pb1 = PredicateBuilder::new("foo").arguments([&t1, &t2]);
        let p1 = pb1
            .values([Value::object(&o1), Value::object(&o2)])
            .build()
            .unwrap();
        let pr1 = p1.into_resolved(&[]).unwrap();
        let pr2 = pb1.resolved_values([&o1, &o2]).build().unwrap();

        assert_eq!(pr1, pr2);
    }
}
