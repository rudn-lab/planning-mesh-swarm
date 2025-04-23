use crate::{InternerSymbol, INTERNER};
use alloc::collections::BTreeMap;

pub trait Named {
    fn name(&self) -> InternerSymbol;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedStorage<T: Named + Clone>(BTreeMap<InternerSymbol, T>);

impl<T: Named + Clone> NamedStorage<T> {
    pub fn insert(&mut self, object: T) {
        let _ = self.0.insert(object.name(), object);
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.0.get(&INTERNER.lock().get_or_intern(name))
    }

    pub fn get_by_symbol(&self, symbol: &InternerSymbol) -> Option<&T> {
        self.0.get(symbol)
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.0.values()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.0.contains_key(&INTERNER.lock().get_or_intern(name))
    }
}

impl<T: Named + Clone> Default for NamedStorage<T> {
    fn default() -> Self {
        Self(BTreeMap::default())
    }
}
