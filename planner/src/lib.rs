#![feature(coverage_attribute)]
#![cfg_attr(not(test), no_std)]

// #![feature(trace_macros)]
// trace_macros!(true);

extern crate alloc;
pub mod action;
pub mod calculus;
pub mod entity;
pub mod problem;
pub mod state;
pub mod truth_table;
mod util;

use rand::SeedableRng;
use rand_chacha::ChaCha8Rng;
use spin::Mutex;
use string_interner::{backend::BufferBackend, symbol::SymbolU16, StringInterner};

type InternerSymbol = SymbolU16;
type Interner = StringInterner<BufferBackend<InternerSymbol>>;
lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
    static ref RANDOM: Mutex<ChaCha8Rng> = Mutex::new(ChaCha8Rng::seed_from_u64(42));
}

mod sealed {
    /// This is silly.
    /// https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed
    #[allow(dead_code)]
    pub(crate) trait Sealed {}
}
