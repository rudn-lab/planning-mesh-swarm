use alloc::rc::Rc;
use core::cell::RefCell;
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
    container: Rc<RefCell<S>>,
    _type: PhantomData<T>,
}

impl<T: Handleable, S: Storage<T>> SmartHandle<T, S> {
    pub fn value(&self) -> T {
        self.container.borrow().get(self)
    }
}

impl<T: Handleable, S: Storage<T>> Clone for SmartHandle<T, S> {
    fn clone(&self) -> Self {
        Self {
            idx: self.idx.dupe(),
            container: Rc::clone(&self.container),
            _type: PhantomData,
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
#[coverage(off)]
impl<T: Handleable, S: Storage<T>> Debug for SmartHandle<T, S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let handle_to = core::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "Handle<{}>({})", handle_to, self.idx.0)
    }
}

impl<T: Handleable, S: Storage<T>> SmartHandle<T, S> {
    pub(crate) fn new(idx: Idx, container: S) -> Self {
        Self {
            idx,
            container: Rc::new(RefCell::new(container)),
            _type: PhantomData,
        }
    }

    pub(crate) fn from_raw(idx: usize, container: S) -> Self {
        Self {
            idx: Idx(idx),
            container: Rc::new(RefCell::new(container)),
            _type: PhantomData,
        }
    }

    pub fn inner(&self) -> T {
        self.container.borrow().get(self)
    }

    pub fn container(&self) -> impl Deref<Target = S> + '_ {
        self.container.borrow()
    }
}

impl<T: Handleable, S: Storage<T>> PartialEq for SmartHandle<T, S> {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value() && self.idx == other.idx
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
