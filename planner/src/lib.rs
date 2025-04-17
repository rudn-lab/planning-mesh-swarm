#![feature(lazy_type_alias)]
#![feature(coverage_attribute)]
#![cfg_attr(not(test), no_std)]

// #![feature(trace_macros)]
// trace_macros!(true);

extern crate alloc;
pub mod action;
pub mod calculus;
pub mod entity;
pub mod parser;
pub mod problem;
pub mod state;
pub mod truth_table;
mod util;

use core::sync::atomic::{AtomicUsize, Ordering};

pub use gazebo::dupe::Dupe;
use spin::Mutex;
use string_interner::{backend::BufferBackend, symbol::SymbolU16, StringInterner};

pub type InternerSymbol = SymbolU16;

type Interner = StringInterner<BufferBackend<InternerSymbol>>;
lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}

/// Is used to uniquely mark things.
#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Marker(usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);
impl Marker {
    fn new() -> Self {
        Marker(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

mod sealed {
    /// This is silly.
    /// https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
    #[allow(dead_code)]
    pub(crate) trait Sealed {}
}
