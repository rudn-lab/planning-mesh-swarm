use alloc::rc::Rc;
use core::cell::RefCell;

/// Special Clone trait that ensures
/// that even data in smart pointers is duplicated.
/// Use sparingly.
pub trait DeepClone {
    fn deep_clone(&self) -> Self;
}

impl<T: Clone> DeepClone for Rc<RefCell<T>> {
    fn deep_clone(&self) -> Self {
        Rc::new(RefCell::new(self.borrow().clone()))
    }
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use alloc::rc::Rc;
    use core::cell::RefCell;

    use crate::util::deep_clone::DeepClone;

    #[test]
    fn test_deep_clone() {
        let a = Rc::new(RefCell::new(Vec::from([1, 2, 3])));
        let b = a.clone();

        a.borrow_mut().push(4);

        assert!(b.borrow().contains(&4));

        let c = b.deep_clone();
        b.borrow_mut().push(5);

        assert!(!c.borrow().contains(&5));
    }
}
