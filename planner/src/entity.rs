use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    vec,
    vec::Vec,
};
use core::cell::{Ref, RefCell};

use crate::{
    util::smart_handle::{Handleable, Idx, SmartHandle, Storage},
    InternerSymbol, INTERNER,
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

type TypeIdx = Idx;
type SuperTypeIdx = Idx;
type SubTypeIdx = Idx;

pub type TypeHandle = SmartHandle<Type, EntityStorage>;

impl TypeHandle {
    pub fn direct_subtypes(&self) -> Vec<TypeHandle> {
        self.container().get_direct_subtypes(self)
    }

    pub fn subtypes(&self) -> Vec<TypeHandle> {
        self.container().get_subtypes(self)
    }

    pub fn inherits(&self, super_type: &SuperTypeHandle) -> bool {
        self.container().inherits(self, super_type)
    }

    pub fn inherits_or_eq(&self, super_type: &SuperTypeHandle) -> bool {
        self.container().inherits_or_eq(self, super_type)
    }

    pub fn get_objects_strict(&self) -> Vec<ObjectHandle> {
        self.container().get_by_type_strict(self)
    }

    pub fn get_objects(&self) -> Vec<ObjectHandle> {
        self.container().get_by_type(self)
    }
}

pub type SuperTypeHandle = TypeHandle;
pub type SubTypeHandle = TypeHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Object {
    name: InternerSymbol,
}

impl Object {
    pub fn new(name: &str) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
        }
    }
}

impl Handleable for Object {}

pub type ObjectHandle = SmartHandle<Object, EntityStorage>;

impl ObjectHandle {
    pub fn r#type(&self) -> TypeHandle {
        self.container().get_type(self)
    }
}

#[derive(Debug, Clone)]
pub enum TypeError {
    AlreadyHasSuperType(TypeHandle),
    CreatesCircularInheritance,
}

#[derive(Debug, Clone, Default)]
pub struct EntityStorage {
    types: Rc<RefCell<Vec<Type>>>,
    objects: Rc<RefCell<Vec<(Object, TypeIdx)>>>,
    supertypes: Rc<RefCell<BTreeMap<SubTypeIdx, SuperTypeIdx>>>,
    subtypes: Rc<RefCell<BTreeMap<SuperTypeIdx, Vec<SubTypeIdx>>>>,
}

impl EntityStorage {
    pub fn get_or_create_type(&mut self, type_name: &str) -> TypeHandle {
        self.get_or_insert_type(Type::new(type_name))
    }

    fn get_or_insert_type(&mut self, r#type: Type) -> TypeHandle {
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
        self.get_subtypes(super_type).contains(sub_type)
    }

    pub fn inherits_or_eq(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool {
        sub_type == super_type || self.inherits(sub_type, super_type)
    }

    pub fn get_or_create_object(&mut self, object_name: &str, r#type: &TypeHandle) -> ObjectHandle {
        self.get_or_insert_object(Object::new(object_name), r#type.idx)
    }

    fn get_or_insert_object(&mut self, object: Object, r#type: TypeIdx) -> ObjectHandle {
        if let Some(idx) = self
            .objects
            .borrow()
            .iter()
            .enumerate()
            .find_map(|(i, &(o, ti))| {
                if o == object && ti == r#type {
                    Some(i)
                } else {
                    None
                }
            })
        {
            return ObjectHandle::from_raw(idx, Rc::new(self.clone()));
        }

        let idx = self.objects.borrow().len();
        self.objects.borrow_mut().push((object, r#type));
        ObjectHandle::from_raw(idx, Rc::new(self.clone()))
    }

    pub fn get_by_type_strict(&self, r#type: &TypeHandle) -> Vec<ObjectHandle> {
        self.objects
            .borrow()
            .iter()
            .enumerate()
            .filter_map(|(i, &(_, ti))| {
                if ti == r#type.idx {
                    Some(ObjectHandle::from_raw(i, Rc::new(self.clone())))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_by_type(&self, r#type: &TypeHandle) -> Vec<ObjectHandle> {
        let mut res = self.get_by_type_strict(r#type);

        let types = r#type.container();

        for t in types.get_subtypes(r#type) {
            res.extend(self.get_by_type_strict(&t).into_iter());
        }

        res
    }

    pub fn get_type(&self, object: &ObjectHandle) -> TypeHandle {
        TypeHandle::new(self.objects.borrow()[object.idx.0].1, Rc::new(self.clone()))
    }
}

impl Storage<Type> for EntityStorage {
    fn get<S: Storage<Type>>(&self, handle: &SmartHandle<Type, S>) -> Type {
        // Cannot panic, because the only way to create a handle is
        // through the ObjectStorage, so they are all accounted for
        self.types.borrow()[handle.idx.0]
    }
}

impl Storage<Object> for EntityStorage {
    fn get<S: Storage<Object>>(&self, handle: &SmartHandle<Object, S>) -> Object {
        // Cannot panic, because the only way to create a handle is
        // through the ObjectStorage, so they are all accounted for
        self.objects.borrow()[handle.idx.0].0
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;

    #[test]
    fn test_type_inheritance() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("foo");
        let t2 = entities.get_or_create_type("bar");
        let t3 = entities.get_or_create_type("baz");
        let t4 = entities.get_or_create_type("qux");

        let res = entities.create_inheritance(&t1, &t2);
        assert!(res.is_ok());
        println!("{:?}", entities);

        let _ = entities.create_inheritance(&t2, &t3);
        let res = entities.create_inheritance(&t1, &t3);
        assert!(matches!(res, Err(TypeError::AlreadyHasSuperType(_))));
        if let Err(TypeError::AlreadyHasSuperType(t)) = res {
            assert_eq!(t, t2);
        }

        let _ = entities.create_inheritance(&t3, &t4);

        assert!(entities.inherits(&t1, &t2));
        assert!(entities.inherits(&t2, &t3));
        assert!(entities.inherits(&t3, &t4));

        assert!(entities.inherits(&t1, &t3));
        assert!(entities.inherits(&t2, &t4));

        let res = entities.create_inheritance(&t4, &t1);
        assert!(matches!(res, Err(TypeError::CreatesCircularInheritance)));

        assert!(!entities.inherits(&t3, &t1));
    }

    #[test]
    fn test_subentities() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("t1");
        let t2 = entities.get_or_create_type("t2");
        let t3 = entities.get_or_create_type("t3");
        let t4 = entities.get_or_create_type("t4");
        let t5 = entities.get_or_create_type("t5");
        let t6 = entities.get_or_create_type("t6");
        let t7 = entities.get_or_create_type("t7");
        let t8 = entities.get_or_create_type("t8");

        let _ = entities.create_inheritance(&t2, &t1);
        let _ = entities.create_inheritance(&t3, &t2);
        let _ = entities.create_inheritance(&t4, &t3);
        let _ = entities.create_inheritance(&t5, &t3);
        let _ = entities.create_inheritance(&t6, &t5);
        let _ = entities.create_inheritance(&t7, &t5);
        let _ = entities.create_inheritance(&t8, &t7);

        let entities = entities;

        assert_eq!(entities.get_subtypes(&t8), Vec::<&TypeHandle>::new());
        assert_eq!(entities.get_subtypes(&t7), vec![&t8]);
        assert_eq!(entities.get_subtypes(&t6), Vec::<&TypeHandle>::new());
        assert_eq!(entities.get_subtypes(&t5), vec![&t6, &t7, &t8]);
        assert_eq!(entities.get_subtypes(&t4), Vec::<&TypeHandle>::new());
        assert_eq!(entities.get_subtypes(&t3), vec![&t4, &t5, &t6, &t7, &t8]);
        assert_eq!(
            entities.get_subtypes(&t2),
            vec![&t3, &t4, &t5, &t6, &t7, &t8]
        );
        assert_eq!(
            entities.get_subtypes(&t1),
            vec![&t2, &t3, &t4, &t5, &t6, &t7, &t8]
        );
    }
}
