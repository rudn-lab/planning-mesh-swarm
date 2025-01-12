use alloc::boxed::Box;
use alloc::rc::Rc;
use lazy_static;
use spin::Mutex;
use string_interner::{backend::BufferBackend, symbol::SymbolU16, StringInterner};

type Interner = StringInterner<BufferBackend<SymbolU16>>;
lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

pub trait Evaluable: Clone {
    fn eval(&self) -> bool;
}

impl<T: Evaluable> Evaluable for Box<T> {
    fn eval(&self) -> bool {
        (**self).eval()
    }
}

#[derive(Clone)]
pub struct Predicate {
    sym: SymbolU16,
    fun: Rc<dyn Fn() -> bool>,
}

impl Predicate {
    pub fn new(name: &str, fun: impl Fn() -> bool + 'static) -> Self {
        Self {
            sym: INTERNER.lock().get_or_intern(name),
            fun: Rc::new(fun),
        }
    }
}

impl core::fmt::Debug for Predicate {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Predicate {}",
            INTERNER.lock().resolve(self.sym).unwrap()
        )
    }
}

impl PartialEq for Predicate {
    fn eq(&self, other: &Self) -> bool {
        self.sym == other.sym
    }
}

impl Evaluable for Predicate {
    fn eval(&self) -> bool {
        (self.fun)()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::assert;

    #[test]
    fn test_eval() {
        let p = Predicate::new("truth", || true);

        assert!(p.eval());
    }

    #[test]
    fn test_name_equal() {
        let p = Predicate::new("truth", || true);
        let p2 = Predicate::new("truth", || false);

        assert!(p == p2);
    }
}
