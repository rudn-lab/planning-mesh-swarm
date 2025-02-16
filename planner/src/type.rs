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
    /// Represents inheritance relationship from sub type to super type
    inheritance: SecondaryMap<SubTypeHandle, SuperTypeHandle>,
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
        if let Some(t) = self.inheritance.get(sub_type) {
            return Err(TypeError::AlreadyHasSuperType(*t));
        }

        if self.inherits(super_type, sub_type) {
            return Err(TypeError::CreatesCircularInheritance);
        }

        self.inheritance.insert(sub_type, super_type);

        Ok(())
    }

    pub fn get_parent(&self, r#type: TypeHandle) -> Option<TypeHandle> {
        self.inheritance.get(r#type).copied()
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
}
