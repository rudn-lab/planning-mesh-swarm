use alloc::rc::Rc;
use core::fmt::Debug;
use core::{hash::Hash, marker::PhantomData, ops::Deref};
use gazebo::dupe::Dupe;

pub trait Handleable: Clone + PartialEq {}

pub trait Storage<T: Handleable> {
    fn get<S: Storage<T>>(&self, handle: &SmartHandle<T, S>) -> T;
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Idx(pub(crate) usize);

pub struct SmartHandle<T: Handleable, S: Storage<T>> {
    pub(crate) idx: Idx,
    container: Rc<S>,
    _marker: PhantomData<T>,
}

impl<T: Handleable, S: Storage<T>> Clone for SmartHandle<T, S> {
    fn clone(&self) -> Self {
        Self {
            idx: self.idx.dupe(),
            container: Rc::clone(&self.container),
            _marker: PhantomData,
        }
    }
}

impl<T: Handleable, S: Storage<T>> Dupe for SmartHandle<T, S> {
    fn dupe(&self) -> Self {
        self.clone()
    }
}

/// This __has__ to be implemented manually.
/// Deriving causes stack overflow errors in tests
/// when it needs to write assertion failure to console.
/// I'm not kidding.
impl<T: Handleable, S: Storage<T>> Debug for SmartHandle<T, S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let handle_to = core::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "Handle<{}>({})", handle_to, self.idx.0)
    }
}

impl<T: Handleable, S: Storage<T>> SmartHandle<T, S> {
    pub(crate) fn new(idx: Idx, container: Rc<S>) -> Self {
        Self {
            idx,
            container,
            _marker: PhantomData,
        }
    }

    pub(crate) fn from_raw(idx: usize, container: Rc<S>) -> Self {
        Self {
            idx: Idx(idx),
            container,
            _marker: PhantomData,
        }
    }

    pub fn value(&self) -> T {
        self.container.get(self)
    }
}

impl<T: Handleable, S: Storage<T>> PartialEq for SmartHandle<T, S> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T: Handleable, S: Storage<T>> PartialEq<&SmartHandle<T, S>> for SmartHandle<T, S> {
    fn eq(&self, other: &&SmartHandle<T, S>) -> bool {
        *self == **other
    }
}

impl<T: Handleable, S: Storage<T>> PartialEq<SmartHandle<T, S>> for &SmartHandle<T, S> {
    fn eq(&self, other: &SmartHandle<T, S>) -> bool {
        **self == *other
    }
}

impl<T: Handleable, S: Storage<T>> Eq for SmartHandle<T, S> {}

/// Fields that have interior mutability aren't being used here
impl<T: Handleable, S: Storage<T>> PartialOrd for SmartHandle<T, S> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Fields that have interior mutability aren't being used here
impl<T: Handleable, S: Storage<T>> Ord for SmartHandle<T, S> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.idx.cmp(&other.idx)
    }
}

/// Fields that have interior mutability aren't being used here
impl<T: Handleable, S: Storage<T>> Hash for SmartHandle<T, S> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<T: Handleable, S: Storage<T>> SmartHandle<T, S> {
    pub fn inner(&self) -> T {
        self.container.get(self)
    }

    pub fn container(&self) -> impl Deref<Target = S> + '_ {
        Rc::clone(&self.container)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::cell::RefCell;

    impl Handleable for Thing {}

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Thing {
        name: String,
        val: usize,
    }

    #[derive(Clone)]
    struct Container<T: Handleable> {
        data: Rc<RefCell<Vec<T>>>,
    }

    impl<T: Handleable> Container<T> {
        fn new() -> Self {
            Self {
                data: Rc::new(RefCell::new(vec![])),
            }
        }

        fn add(&mut self, thing: T) -> SmartHandle<T, Self> {
            if let Some(idx) = self.data.borrow().iter().enumerate().find_map(|(i, v)| {
                if *v == thing {
                    Some(i)
                } else {
                    None
                }
            }) {
                return SmartHandle {
                    idx: Idx(idx),
                    container: Rc::new(self.clone()),
                    _marker: PhantomData,
                };
            }

            let idx = self.data.borrow().len();
            self.data.borrow_mut().push(thing);
            SmartHandle {
                idx: Idx(idx),
                container: Rc::new(self.clone()),
                _marker: PhantomData,
            }
        }
    }

    impl<T: Handleable> Storage<T> for Container<T> {
        fn get<S: Storage<T>>(&self, handle: &SmartHandle<T, S>) -> T {
            self.data.borrow()[handle.idx.0].clone()
        }
    }

    #[test]
    fn test_usage() {
        let a = Thing {
            name: "A".to_string(),
            val: 1,
        };
        let b = Thing {
            name: "B".to_string(),
            val: 2,
        };
        let c = Thing {
            name: "C".to_string(),
            val: 3,
        };
        let d = Thing {
            name: "D".to_string(),
            val: 4,
        };

        let mut cont = Container::new();

        let ha = cont.add(a.clone());
        assert_eq!(ha.inner(), a.clone());
        assert_eq!(cont.get(&ha), a.clone());
        assert_eq!(ha.container().get(&ha), a.clone());

        let hb = cont.add(b.clone());
        assert_eq!(hb.inner(), b.clone());
        assert_eq!(cont.get(&hb), b.clone());
        assert_eq!(hb.container().get(&hb), b.clone());

        let hc = cont.add(c.clone());
        assert_eq!(hc.inner(), c.clone());
        assert_eq!(cont.get(&hc), c.clone());
        assert_eq!(hc.container().get(&hc), c.clone());

        let mut correct = vec![a, b, c];
        assert_eq!(*cont.data.borrow(), correct);
        assert_eq!(*ha.container().data.borrow(), correct);
        assert_eq!(*hb.container().data.borrow(), correct);
        assert_eq!(*hc.container().data.borrow(), correct);

        let hd = cont.add(d.clone());
        assert_eq!(hd.inner(), d.clone());
        assert_eq!(cont.get(&hd), d.clone());
        assert_eq!(hd.container().get(&hd), d.clone());

        correct.push(d);
        assert_eq!(*cont.data.borrow(), correct);
        assert_eq!(*ha.container().data.borrow(), correct);
        assert_eq!(*hb.container().data.borrow(), correct);
        assert_eq!(*hc.container().data.borrow(), correct);
        assert_eq!(*hd.container().data.borrow(), correct);

        let _cont = cont;
        let _e = Thing {
            name: "E".to_string(),
            val: 5,
        };
        // No longer mutable, won't compile
        // _cont.add(_e);
    }
}
