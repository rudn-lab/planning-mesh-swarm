#![feature(allocator_api)] // For nohash-hasher, https://github.com/rust-lang/rust/issues/32838
#![feature(iter_advance_by)]
#![cfg_attr(not(test), no_std)]

extern crate alloc;
pub mod evaluation;
pub mod expression;
pub mod predicate;
pub mod state;
pub mod truth_table;
pub mod r#type;

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
