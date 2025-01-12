#![feature(allocator_api)] // For nohash-hasher, https://github.com/rust-lang/rust/issues/32838
#![cfg_attr(not(test), no_std)]

extern crate alloc;
mod expression;
mod predicate;
mod state;
mod r#type;

use lazy_static;
use spin::Mutex;
use string_interner::{backend::BufferBackend, symbol::SymbolU16, StringInterner};

type InternerSymbol = SymbolU16;
type Interner = StringInterner<BufferBackend<InternerSymbol>>;
lazy_static::lazy_static! {
    static ref INTERNER: Mutex<Interner> = Mutex::new(Interner::new());
}
