use crate::{InternerSymbol, INTERNER};
use alloc::{vec, vec::Vec};
use slotmap::{new_key_type, SecondaryMap, SlotMap};

new_key_type! {
    pub struct TypeHandle;
}

pub type SubTypeHandle = TypeHandle;
pub type SuperTypeHandle = TypeHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    types: SlotMap<TypeHandle, Type>,
    supertypes: SecondaryMap<SubTypeHandle, SuperTypeHandle>,
    subtypes: SecondaryMap<SuperTypeHandle, Vec<SubTypeHandle>>,
}

impl TypeCollection {
    pub fn get_or_create(&mut self, type_name: &str) -> TypeHandle {
        self.get_or_insert(Type::new(type_name))
    }

    fn get_or_insert(&mut self, r#type: Type) -> TypeHandle {
        if let Some(h) = self
            .types
            .iter()
            .find_map(|(h, t)| if *t == r#type { Some(h) } else { None })
        {
            return h;
        }
        self.types.insert(r#type)
    }

    pub fn get(&self, key: TypeHandle) -> Option<&Type> {
        self.types.get(key)
    }

    pub fn create_inheritance(
        &mut self,
        sub_type: SubTypeHandle,
        super_type: SuperTypeHandle,
    ) -> Result<(), TypeError> {
        if let Some(t) = self.supertypes.get(sub_type) {
            return Err(TypeError::AlreadyHasSuperType(*t));
        }

        if self.inherits(super_type, sub_type) {
            return Err(TypeError::CreatesCircularInheritance);
        }

        self.supertypes.insert(sub_type, super_type);
        // self.subtypes.insert(super_type, sub_type);
        self.subtypes.entry(super_type).map(|e| {
            e.and_modify(|subtypes| subtypes.push(sub_type))
                .or_insert_with(|| vec![sub_type])
        });

        Ok(())
    }

    pub fn get_parent(&self, r#type: TypeHandle) -> Option<TypeHandle> {
        self.supertypes.get(r#type).copied()
    }

    pub fn get_parents(&self, r#type: TypeHandle) -> Vec<TypeHandle> {
        let mut res: Vec<TypeHandle> = vec![];
        let mut t = r#type;
        while let Some(tt) = self.get_parent(t) {
            t = tt;
            res.push(t);
        }
        res
    }

    pub fn get_subtypes(&self, r#type: TypeHandle) -> Vec<TypeHandle> {
        let mut ts: Vec<TypeHandle> = self
            .subtypes
            .get(r#type)
            .cloned()
            .unwrap_or_default()
            .to_vec();

        let mut res = vec![];

        while let Some(t) = ts.pop() {
            res.push(t);
            let t = self.subtypes.get(t).cloned().unwrap_or_default();
            ts.extend(t.iter());
        }

        res
    }

    pub fn inherits(&self, sub_type: TypeHandle, super_type: TypeHandle) -> bool {
        self.get_parents(sub_type).contains(&super_type)
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

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

        let tt1 = types.get(t1);
        assert!(tt1.is_some());
        assert_eq!(*tt1.unwrap(), Type::new("foo"));
    }

    #[test]
    fn test_inheritance() {
        let mut types = TypeCollection::default();
        let t1 = types.get_or_create("foo");
        let t2 = types.get_or_create("bar");
        let t3 = types.get_or_create("baz");
        let t4 = types.get_or_create("qux");

        let res = types.create_inheritance(t1, t2);
        assert!(res.is_ok());
        assert_eq!(types.get_parent(t1).unwrap(), t2);

        let _ = types.create_inheritance(t2, t3);
        assert_eq!(types.get_parents(t1), vec![t2, t3]);

        let res = types.create_inheritance(t1, t3);
        assert!(matches!(res, Err(TypeError::AlreadyHasSuperType(_))));
        if let Err(TypeError::AlreadyHasSuperType(t)) = res {
            assert_eq!(t, t2);
        }

        let _ = types.create_inheritance(t3, t4);

        assert!(types.inherits(t1, t2));
        assert!(types.inherits(t2, t3));
        assert!(types.inherits(t3, t4));

        assert!(types.inherits(t1, t3));
        assert!(types.inherits(t2, t4));

        let res = types.create_inheritance(t4, t1);
        println!("{:?}", res);
        assert!(matches!(res, Err(TypeError::CreatesCircularInheritance)));

        assert!(!types.inherits(t3, t1));
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

        let _ = types.create_inheritance(t2, t1);
        let _ = types.create_inheritance(t3, t2);
        let _ = types.create_inheritance(t4, t3);
        let _ = types.create_inheritance(t5, t3);
        let _ = types.create_inheritance(t6, t5);
        let _ = types.create_inheritance(t7, t5);
        let _ = types.create_inheritance(t8, t7);

        let sorted = |mut v: Vec<_>| {
            v.sort();
            v
        };

        assert_eq!(sorted(types.get_subtypes(t8)), vec![]);
        assert_eq!(sorted(types.get_subtypes(t7)), vec![t8]);
        assert_eq!(sorted(types.get_subtypes(t6)), vec![]);
        assert_eq!(sorted(types.get_subtypes(t5)), vec![t6, t7, t8]);
        assert_eq!(sorted(types.get_subtypes(t4)), vec![]);
        assert_eq!(sorted(types.get_subtypes(t3)), vec![t4, t5, t6, t7, t8]);
        assert_eq!(sorted(types.get_subtypes(t2)), vec![t3, t4, t5, t6, t7, t8]);
        assert_eq!(
            sorted(types.get_subtypes(t1)),
            vec![t2, t3, t4, t5, t6, t7, t8]
        );
    }
}
