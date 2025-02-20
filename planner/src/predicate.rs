use crate::{
    evaluation::{Evaluable, EvaluationContext},
    object::ObjectHandle,
    r#type::TypeHandle,
    InternerSymbol, INTERNER, RANDOM,
};
use alloc::{vec, vec::Vec};
use core::fmt::Debug;
use rand::Rng;

/// A predicate how it is defined in the domain.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PredicateDeclaration {
    /// Name of the predicate.
    pub(crate) name: InternerSymbol,
    /// Arguments to the predicate.
    pub(crate) arguments: Vec<TypeHandle>,
}

impl PredicateDeclaration {
    pub fn new(name: &str, arguments: &[&TypeHandle]) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
            arguments: arguments.iter().map(|&v| v.clone()).collect(),
        }
    }

    pub fn as_specific(&self, values: &[Value]) -> Predicate {
        Predicate {
            declaration: self.clone(),
            values: values.to_vec(),
            unique_marker: RANDOM.lock().gen(),
        }
    }

    /// Creates a new resolved predicate,
    /// taking all parameter resolutions as argument
    pub fn as_resolved(&self, resolution: &[ObjectHandle]) -> ResolvedPredicate {
        ResolvedPredicate {
            declaration: self.clone(),
            values: resolution.to_vec(),
            unique_marker: RANDOM.lock().gen(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Object(ObjectHandle),
    ActionParam(ActionParameterRef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterHandle {
    pub(crate) idx: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ActionParameterRef {
    pub(crate) parameter_handle: ParameterHandle,
    pub(crate) r#type: TypeHandle,
}

/// A predicate how it is used in actions.
#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct Predicate {
    declaration: PredicateDeclaration,
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
    unique_marker: u32,
}

impl Predicate {
    pub fn name(&self) -> InternerSymbol {
        self.declaration.name
    }

    pub fn arguments(&self) -> &[TypeHandle] {
        &self.declaration.arguments
    }

    pub fn values(&self) -> Vec<Value> {
        self.values.to_vec()
    }

    pub fn unique_marker(&self) -> u32 {
        self.unique_marker
    }

    /// Creates a new resolved predicate.
    ///
    /// This method (unlike [PredicateDeclaration::as_resolved()])
    /// only need the resolved values for the
    /// [ArgumentValue::ActionParameter] values.
    pub fn as_resolved(&self, resolution: &[ObjectHandle]) -> ResolvedPredicate {
        let mut resolution = resolution.iter();
        ResolvedPredicate {
            declaration: self.declaration.clone(),
            values: self
                .values
                .iter()
                .map(|v| match v {
                    Value::Object(o) => *o,
                    Value::ActionParam(_) => *resolution.next().unwrap(),
                })
                .collect(),
            unique_marker: self.unique_marker,
        }
    }
}

impl Evaluable for Predicate {
    fn eval(&self, context: &impl EvaluationContext) -> bool {
        context.eval(self.clone())
    }

    fn predicates(&self) -> Vec<Predicate> {
        vec![self.clone()]
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.declaration == other.declaration && self.values == other.values
    }
}

/// A fully resolved predicate how it is stored in a state.
#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
pub struct ResolvedPredicate {
    declaration: PredicateDeclaration,
    values: Vec<ObjectHandle>,
    unique_marker: u32,
}

impl ResolvedPredicate {
    pub fn name(&self) -> InternerSymbol {
        self.declaration.name
    }

    pub fn arguments(&self) -> &[TypeHandle] {
        &self.declaration.arguments
    }

    pub fn values(&self) -> &[ObjectHandle] {
        &self.values
    }

    pub fn unique_marker(&self) -> u32 {
        self.unique_marker
    }
}

impl PartialEq for ResolvedPredicate {
    fn eq(&self, other: &Self) -> bool {
        self.declaration == other.declaration && self.values == other.values
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{object::ObjectCollection, r#type::TypeCollection};

    use super::*;
    use core::assert;

    #[test]
    fn test_equality() {
        // Same predicates, even though the marker is different for each one
        let p = PredicateDeclaration::new("foo", &[]).as_specific(&[]);
        let p1 = PredicateDeclaration::new("foo", &[]).as_specific(&[]);

        assert!(p == p1);
        assert!(p.unique_marker != p1.unique_marker);

        // Different because of marker and name
        let p = PredicateDeclaration::new("foo", &[]).as_specific(&[]);
        let p1 = PredicateDeclaration::new("bar", &[]).as_specific(&[]);

        assert!(p != p1);

        // Same, because of the marker and all other args
        let mut types = TypeCollection::default();
        let t = types.get_or_create("t");
        let mut objects = ObjectCollection::default();
        let x = objects.get_or_create("x", &t);

        let p = PredicateDeclaration::new("foo", &[&t]).as_specific(&[Value::Object(x)]);
        let mut p1 = PredicateDeclaration::new("foo", &[&t]).as_specific(&[Value::Object(x)]);
        p1.unique_marker = p.unique_marker;

        assert!(p == p1);

        // Different because of type
        let t1 = types.get_or_create("t1");
        let y = objects.get_or_create("y", &t1);
        let p = PredicateDeclaration::new("foo", &[&t]).as_specific(&[Value::Object(x)]);
        let mut p1 = PredicateDeclaration::new("foo", &[&t1]).as_specific(&[Value::Object(y)]);
        p1.unique_marker = p.unique_marker;

        assert!(p != p1);
    }
}
