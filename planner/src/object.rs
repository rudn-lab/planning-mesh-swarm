use alloc::{collections::BTreeMap, vec, vec::Vec};

use slotmap::{new_key_type, SlotMap};

use crate::{
    r#type::{TypeCollection, TypeHandle},
    InternerSymbol, INTERNER,
};

new_key_type! {
    pub struct ObjectHandle;
}

/// Object storage.
///
/// Stores all currently defined objects.
/// Allows querying them by type.
#[derive(Debug, Clone, Default)]
pub struct ObjectCollection {
    objects: SlotMap<ObjectHandle, Object>,
    type_info: BTreeMap<TypeHandle, Vec<ObjectHandle>>,
}

impl ObjectCollection {
    pub fn get_or_create(&mut self, object_name: &str, r#type: TypeHandle) -> ObjectHandle {
        let h = self.get_or_insert_object(Object::new(object_name, r#type));
        self.type_info
            .entry(r#type)
            .and_modify(|objects| objects.push(h))
            .or_insert_with(|| vec![h]);
        h
    }

    fn get_or_insert_object(&mut self, object: Object) -> ObjectHandle {
        if let Some(h) = self
            .objects
            .iter()
            .find_map(|(h, o)| if *o == object { Some(h) } else { None })
        {
            return h;
        }
        self.objects.insert(object)
    }

    pub fn get(&self, key: ObjectHandle) -> Option<&Object> {
        self.objects.get(key)
    }

    pub fn get_by_type_strict(&self, r#type: &TypeHandle) -> Vec<ObjectHandle> {
        self.type_info.get(r#type).cloned().unwrap_or_default()
    }

    pub fn get_by_type(&self, r#type: &TypeHandle, types: &TypeCollection) -> Vec<ObjectHandle> {
        let mut res = self.get_by_type_strict(r#type);

        for t in types.get_subtypes(*r#type) {
            res.extend(self.get_by_type_strict(&t).iter());
        }

        res
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Object {
    pub(crate) name: InternerSymbol,
    pub(crate) r#type: TypeHandle,
}

impl Object {
    pub(crate) fn new(name: &str, r#type: TypeHandle) -> Self {
        Self {
            name: INTERNER.lock().get_or_intern(name),
            r#type,
        }
    }
}
