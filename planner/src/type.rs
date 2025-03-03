use crate::{
    util::smart_handle::{Handleable, Idx, SmartHandle, Storage},
    InternerSymbol, INTERNER,
};
use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    vec,
    vec::Vec,
};
use core::{
    cell::{Ref, RefCell},
    ops::Deref,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    name: InternerSymbol,
}

impl Type {
    pub fn new(name: &str) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
        }
    }
}

impl Handleable for Type {}

pub type TypeHandle = SmartHandle<Type, TypeCollection>;

pub type SubTypeHandle = TypeHandle;
pub type SuperTypeHandle = TypeHandle;

type SubTypeIdx = Idx;
type SuperTypeIdx = Idx;

#[derive(Debug, Clone)]
pub enum TypeError {
    AlreadyHasSuperType(TypeHandle),
    CreatesCircularInheritance,
}

/// Type storage.
///
/// Contains information about the types used in the planning
/// and their relationship.
#[derive(Debug, Clone, Default)]
pub struct TypeCollection {
    types: Rc<RefCell<Vec<Type>>>,
    supertypes: Rc<RefCell<BTreeMap<SubTypeIdx, SuperTypeIdx>>>,
    subtypes: Rc<RefCell<BTreeMap<SuperTypeIdx, Vec<SubTypeIdx>>>>,
}

impl TypeCollection {
    pub fn get_or_create(&mut self, type_name: &str) -> TypeHandle {
        self.get_or_insert(Type::new(type_name))
    }

    fn get_or_insert(&mut self, r#type: Type) -> TypeHandle {
        if let Some(idx) = self.types.borrow().iter().enumerate().find_map(|(i, t)| {
            if *t == r#type {
                Some(i)
            } else {
                None
            }
        }) {
            return TypeHandle::from_raw(idx, Rc::new(self.clone()));
        }

        let idx = self.types.borrow().len();
        self.types.borrow_mut().push(r#type);
        TypeHandle::from_raw(idx, Rc::new(self.clone()))
    }

    pub fn create_inheritance(
        &mut self,
        sub_type: &SubTypeHandle,
        super_type: &SuperTypeHandle,
    ) -> Result<(), TypeError> {
        if let Some(tidx) = self.supertypes.borrow().get(&sub_type.idx) {
            return Err(TypeError::AlreadyHasSuperType(TypeHandle::new(
                *tidx,
                Rc::new(self.clone()),
            )));
        }

        if self.inherits(super_type, sub_type) {
            return Err(TypeError::CreatesCircularInheritance);
        }

        let mut supertypes = self.supertypes.borrow_mut();
        supertypes.insert(sub_type.idx, super_type.idx);

        self.subtypes
            .borrow_mut()
            .entry(super_type.idx)
            .and_modify(|e| e.push(sub_type.idx))
            .or_insert_with(|| vec![sub_type.idx]);

        Ok(())
    }

    pub fn get_parent(&self, r#type: &TypeHandle) -> Option<TypeHandle> {
        Ref::filter_map(self.supertypes.borrow(), |s| s.get(&r#type.idx))
            .ok()
            .map(|v| TypeHandle::new(*v, Rc::new(self.clone())))
    }

    /// Get sequence of all types that are ancestors of the current type, from most specific to most generic.
    /// The provided type is not included in the sequence.
    pub fn get_parents(&self, r#type: impl Deref<Target = TypeHandle>) -> Vec<TypeHandle> {
        self.get_parent(&r#type)
            .map(|mut parent| {
                let mut parents: VecDeque<TypeHandle> = VecDeque::new();
                parents.push_back(parent.clone());
                while let Some(ancestor) = self.get_parent(&parent) {
                    parents.push_back(ancestor.clone());
                    parent = ancestor;
                }

                parents.into()
            })
            .unwrap_or_default()
    }

    pub fn get_direct_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle> {
        Ref::filter_map(self.subtypes.borrow(), |s| s.get(&r#type.idx))
            .map(|subtypes| {
                subtypes
                    .iter()
                    .map(|idx| TypeHandle::new(*idx, Rc::new(self.clone())))
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn get_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle> {
        let mut ts: VecDeque<_> = self.get_direct_subtypes(r#type).into();
        let mut subtypes = VecDeque::new();
        while let Some(t) = ts.pop_front() {
            let tt = self.get_direct_subtypes(&t);
            subtypes.push_back(t);
            ts.extend(tt.into_iter());
        }
        subtypes.into()
    }

    pub fn inherits(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool {
        self.get_parents(sub_type).iter().any(|v| *v == *super_type)
    }

    pub fn inherits_or_eq(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool {
        sub_type == super_type || self.inherits(sub_type, super_type)
    }

    pub fn len(&self) -> usize {
        self.types.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.borrow().is_empty()
    }
}

impl Storage<Type> for TypeCollection {
    fn get<S: Storage<Type>>(&self, handle: &SmartHandle<Type, S>) -> Type {
        // Cannot panic, because the only way to create a handle is
        // through the TypeCollection
        self.types.borrow()[handle.idx.0]
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut types = TypeCollection::default();

        assert!(types.is_empty());

        let t1 = types.get_or_create("foo");
        let t2 = types.get_or_create("bar");
        assert_ne!(t1, t2);

        let t11 = types.get_or_create("foo");
        assert_eq!(t1, t11);

        assert!(!types.is_empty());
        assert_eq!(types.len(), 2);

        let tt1 = types.get(&t1);
        assert_eq!(tt1, Type::new("foo"));
    }

    #[test]
    fn test_inheritance() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("foo");
        let t2 = types.get_or_create("bar");
        let t3 = types.get_or_create("baz");
        let t4 = types.get_or_create("qux");

        let res = types.create_inheritance(&t1, &t2);
        assert!(res.is_ok());
        assert_eq!(types.get_parent(&t1).unwrap(), t2);

        let _ = types.create_inheritance(&t2, &t3);
        assert_eq!(
            types.get_parents(&t1).into_iter().collect::<Vec<_>>(),
            vec![&t2, &t3]
        );

        let res = types.create_inheritance(&t1, &t3);
        assert!(matches!(res, Err(TypeError::AlreadyHasSuperType(_))));
        if let Err(TypeError::AlreadyHasSuperType(t)) = res {
            assert_eq!(t, t2);
        }

        let _ = types.create_inheritance(&t3, &t4);

        assert!(types.inherits(&t1, &t2));
        assert!(types.inherits(&t2, &t3));
        assert!(types.inherits(&t3, &t4));

        assert!(types.inherits(&t1, &t3));
        assert!(types.inherits(&t2, &t4));

        let res = types.create_inheritance(&t4, &t1);
        assert!(matches!(res, Err(TypeError::CreatesCircularInheritance)));

        assert!(!types.inherits(&t3, &t1));
    }

    #[test]
    fn test_subtypes() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("t1");
        let t2 = types.get_or_create("t2");
        let t3 = types.get_or_create("t3");
        let t4 = types.get_or_create("t4");
        let t5 = types.get_or_create("t5");
        let t6 = types.get_or_create("t6");
        let t7 = types.get_or_create("t7");
        let t8 = types.get_or_create("t8");

        let _ = types.create_inheritance(&t2, &t1);
        let _ = types.create_inheritance(&t3, &t2);
        let _ = types.create_inheritance(&t4, &t3);
        let _ = types.create_inheritance(&t5, &t3);
        let _ = types.create_inheritance(&t6, &t5);
        let _ = types.create_inheritance(&t7, &t5);
        let _ = types.create_inheritance(&t8, &t7);

        let types = types;

        assert_eq!(types.get_subtypes(&t8), Vec::<&TypeHandle>::new());
        assert_eq!(types.get_subtypes(&t7), vec![&t8]);
        assert_eq!(types.get_subtypes(&t6), Vec::<&TypeHandle>::new());
        assert_eq!(types.get_subtypes(&t5), vec![&t6, &t7, &t8]);
        assert_eq!(types.get_subtypes(&t4), Vec::<&TypeHandle>::new());
        assert_eq!(types.get_subtypes(&t3), vec![&t4, &t5, &t6, &t7, &t8]);
        assert_eq!(types.get_subtypes(&t2), vec![&t3, &t4, &t5, &t6, &t7, &t8]);
        assert_eq!(
            types.get_subtypes(&t1),
            vec![&t2, &t3, &t4, &t5, &t6, &t7, &t8]
        );
    }
}
