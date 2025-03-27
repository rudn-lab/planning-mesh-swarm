use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    vec,
    vec::Vec,
};
use core::{
    cell::{Ref, RefCell},
    fmt::Debug,
};
use gazebo::dupe::Dupe;

use crate::{
    util::{
        deep_clone::DeepClone,
        smart_handle::{Handleable, Idx, SmartHandle, Storage},
    },
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
        self.container().get_objects_by_type_strict(self)
    }

    pub fn get_objects(&self) -> Vec<ObjectHandle> {
        self.container().get_objects_by_type(self)
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
        self.container().get_type_for(self)
    }
}

#[derive(Debug, Clone)]
pub enum TypeError {
    AlreadyHasSuperType(TypeHandle),
    CreatesCircularInheritance,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EntityStorage {
    types: Rc<RefCell<Vec<Type>>>,
    objects: Rc<RefCell<Vec<(Object, TypeHandle)>>>,
    supertypes: Rc<RefCell<BTreeMap<SubTypeIdx, SuperTypeIdx>>>,
    subtypes: Rc<RefCell<BTreeMap<SuperTypeIdx, Vec<SubTypeIdx>>>>,
}

/// Trait that exposes methods related to types of [EntityStorage]
pub trait TypeStorage: Debug {
    fn get_or_create_type(&mut self, type_name: &str) -> TypeHandle;
    fn get_type(&self, type_name: &str) -> Option<TypeHandle>;
    fn create_inheritance(
        &mut self,
        sub_type: &SubTypeHandle,
        super_type: &SuperTypeHandle,
    ) -> Result<(), TypeError>;
    fn get_direct_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle>;
    fn get_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle>;
    fn inherits(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool;
    fn inherits_or_eq(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool;
    fn get_objects_by_type_strict(&self, r#type: &TypeHandle) -> Vec<ObjectHandle>;
    fn get_objects_by_type(&self, r#type: &TypeHandle) -> Vec<ObjectHandle>;
}

/// Trait that exposes methods related to objects of [EntityStorage]
pub trait ObjectStorage: Debug {
    fn get_or_create_object(&mut self, object_name: &str, r#type: &TypeHandle) -> ObjectHandle;
    /// Returns the object with this name and this type
    fn get_object_strict(&self, object_name: &str, r#type: &TypeHandle) -> Option<ObjectHandle>;
    /// Returns the object with this name for which the given type is a supertype
    fn get_object(&self, object_name: &str, r#type: &TypeHandle) -> Option<ObjectHandle>;
    fn get_type_for(&self, object: &ObjectHandle) -> TypeHandle;
}

impl TypeStorage for EntityStorage {
    fn get_or_create_type(&mut self, type_name: &str) -> TypeHandle {
        self.get_type(type_name).unwrap_or_else(|| {
            let idx = self.types.borrow().len();
            self.types.borrow_mut().push(Type::new(type_name));
            TypeHandle::from_raw(idx, self.clone())
        })
    }

    fn get_type(&self, type_name: &str) -> Option<TypeHandle> {
        let type_name = INTERNER.lock().get_or_intern(type_name);
        self.types.borrow().iter().enumerate().find_map(|(i, t)| {
            if t.name == type_name {
                Some(TypeHandle::from_raw(i, self.clone()))
            } else {
                None
            }
        })
    }

    fn create_inheritance(
        &mut self,
        sub_type: &SubTypeHandle,
        super_type: &SuperTypeHandle,
    ) -> Result<(), TypeError> {
        if let Some(idx) = self.supertypes.borrow().get(&sub_type.idx) {
            return Err(TypeError::AlreadyHasSuperType(TypeHandle::new(
                *idx,
                self.clone(),
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

    fn get_direct_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle> {
        Ref::filter_map(self.subtypes.borrow(), |s| s.get(&r#type.idx))
            .map(|subtypes| {
                subtypes
                    .iter()
                    .map(|idx| TypeHandle::new(*idx, self.clone()))
                    .collect()
            })
            .unwrap_or_default()
    }

    fn get_subtypes(&self, r#type: &TypeHandle) -> Vec<TypeHandle> {
        let mut ts: VecDeque<_> = self.get_direct_subtypes(r#type).into();
        let mut subtypes = VecDeque::new();
        while let Some(t) = ts.pop_front() {
            let tt = self.get_direct_subtypes(&t);
            subtypes.push_back(t);
            ts.extend(tt.into_iter());
        }
        subtypes.into()
    }

    fn inherits(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool {
        self.get_subtypes(super_type).contains(sub_type)
    }

    fn inherits_or_eq(&self, sub_type: &SubTypeHandle, super_type: &SuperTypeHandle) -> bool {
        sub_type == super_type || self.inherits(sub_type, super_type)
    }

    fn get_objects_by_type_strict(&self, r#type: &TypeHandle) -> Vec<ObjectHandle> {
        self.objects
            .borrow()
            .iter()
            .enumerate()
            .filter_map(|(i, (_, t))| {
                if t == r#type {
                    Some(ObjectHandle::from_raw(i, self.clone()))
                } else {
                    None
                }
            })
            .collect()
    }

    fn get_objects_by_type(&self, r#type: &TypeHandle) -> Vec<ObjectHandle> {
        let mut res = self.get_objects_by_type_strict(r#type);

        let types = r#type.container();

        for t in types.get_subtypes(r#type) {
            res.extend(self.get_objects_by_type_strict(&t).into_iter());
        }

        res
    }
}

impl ObjectStorage for EntityStorage {
    fn get_or_create_object(&mut self, object_name: &str, r#type: &TypeHandle) -> ObjectHandle {
        self.get_object_strict(object_name, r#type)
            .unwrap_or_else(|| {
                let idx = self.objects.borrow().len();
                self.objects
                    .borrow_mut()
                    .push((Object::new(object_name), r#type.dupe()));
                ObjectHandle::from_raw(idx, self.clone())
            })
    }

    fn get_object(&self, object_name: &str, r#type: &TypeHandle) -> Option<ObjectHandle> {
        let object_name = INTERNER.lock().get_or_intern(object_name);
        self.objects
            .borrow()
            .iter()
            .enumerate()
            .find_map(|(i, (o, t))| {
                if o.name == object_name && t.inherits_or_eq(r#type) {
                    #[cfg(test)]
                    {
                        println!("Hello");
                    }

                    Some(ObjectHandle::from_raw(i, self.clone()))
                } else {
                    None
                }
            })
    }

    fn get_object_strict(&self, object_name: &str, r#type: &TypeHandle) -> Option<ObjectHandle> {
        let object_name = INTERNER.lock().get_or_intern(object_name);
        self.objects
            .borrow()
            .iter()
            .enumerate()
            .find_map(|(i, (o, t))| {
                if o.name == object_name && t == r#type {
                    Some(ObjectHandle::from_raw(i, self.clone()))
                } else {
                    None
                }
            })
    }

    fn get_type_for(&self, object: &ObjectHandle) -> TypeHandle {
        self.objects.borrow()[object.idx.0].1.dupe()
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

impl DeepClone for EntityStorage {
    fn deep_clone(&self) -> Self {
        Self {
            types: self.types.deep_clone(),
            objects: self.objects.deep_clone(),
            supertypes: self.supertypes.deep_clone(),
            subtypes: self.subtypes.deep_clone(),
        }
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;

    #[test]
    fn test_creation() {
        let mut entities = EntityStorage::default();

        let t1 = entities.get_or_create_type("foo");
        let t2 = entities.get_or_create_type("bar");
        assert_ne!(t1, t2);

        let t11 = entities.get_or_create_type("foo");
        assert_eq!(t1, t11);

        let o1 = entities.get_or_create_object("a", &t1);
        let o2 = entities.get_or_create_object("b", &t2);
        assert_ne!(o1, o2);

        let o11 = entities.get_or_create_object("a", &t1);
        assert_eq!(o1, o11);

        let o3 = entities.get_or_create_object("a", &t2);
        assert_ne!(o3, o1);
        assert_ne!(o3, o2);

        assert_eq!(t1.inner(), Type::new("foo"));
        assert_eq!(o1.inner(), Object::new("a"));
    }

    #[test]
    fn test_wrappers() {
        let mut entities = EntityStorage::default();

        let t1 = entities.get_or_create_type("foo");
        let t2 = entities.get_or_create_type("bar");
        let t3 = entities.get_or_create_type("bar");
        let t4 = entities.get_or_create_type("qux");
        let _ = entities.create_inheritance(&t2, &t1);
        let _ = entities.create_inheritance(&t3, &t2);
        let _ = entities.create_inheritance(&t4, &t1);

        let _ = entities.get_or_create_object("o10", &t1);
        let _ = entities.get_or_create_object("o11", &t1);

        let _ = entities.get_or_create_object("o20", &t2);
        let _ = entities.get_or_create_object("o21", &t2);

        let _ = entities.get_or_create_object("o30", &t3);
        let _ = entities.get_or_create_object("o31", &t3);
        let _ = entities.get_or_create_object("o31", &t3);

        let _ = entities.get_or_create_object("o40", &t4);
        let _ = entities.get_or_create_object("o41", &t4);
        let _ = entities.get_or_create_object("o41", &t4);

        assert_eq!(t1.direct_subtypes(), entities.get_direct_subtypes(&t1));
        assert_eq!(t1.subtypes(), entities.get_subtypes(&t1));
        assert_eq!(t1.inherits(&t2), entities.inherits(&t1, &t2));
        assert_eq!(t2.inherits(&t1), entities.inherits(&t2, &t1));
        assert_eq!(t1.inherits_or_eq(&t2), entities.inherits_or_eq(&t1, &t2));
        assert_eq!(t1.inherits_or_eq(&t3), entities.inherits_or_eq(&t1, &t3));
        assert_eq!(
            t4.get_objects_strict(),
            entities.get_objects_by_type_strict(&t4)
        );
        assert_eq!(t1.get_objects(), entities.get_objects_by_type(&t1));
    }

    #[test]
    fn test_type_inheritance() {
        let mut entities = EntityStorage::default();
        let t1 = entities.get_or_create_type("foo");
        let t2 = entities.get_or_create_type("bar");
        let t3 = entities.get_or_create_type("baz");
        let t4 = entities.get_or_create_type("qux");

        let res = entities.create_inheritance(&t1, &t2);
        assert!(res.is_ok());

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
    fn test_subtypes() {
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

    #[test]
    fn test_objects() {
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

        let o10 = entities.get_or_create_object("o10", &t1);
        let o11 = entities.get_or_create_object("o11", &t1);
        let o12 = entities.get_or_create_object("o12", &t1);
        assert_eq!(
            entities.get_objects_by_type_strict(&t1),
            vec![o10, o11, o12]
        );

        let o6 = entities.get_or_create_object("o6", &t6);

        // Same name
        let o70 = entities.get_or_create_object("o7", &t7);
        let _ = entities.get_or_create_object("o7", &t7);
        let _ = entities.get_or_create_object("o7", &t7);
        assert_eq!(entities.get_objects_by_type_strict(&t7), vec![&o70]);

        let o80 = entities.get_or_create_object("o80", &t8);
        let o81 = entities.get_or_create_object("o81", &t8);
        let o82 = entities.get_or_create_object("o82", &t8);
        assert_eq!(entities.get_objects_by_type(&t8), vec![&o80, &o81, &o82]);

        assert_eq!(
            entities.get_objects_by_type(&t4),
            Vec::<ObjectHandle>::new()
        );
        assert_eq!(
            entities.get_objects_by_type(&t5),
            vec![&o6, &o70, &o80, &o81, &o82]
        );
        assert_eq!(
            entities.get_objects_by_type(&t3),
            vec![&o6, &o70, &o80, &o81, &o82]
        );
    }
}
