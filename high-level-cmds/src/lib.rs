#![no_std]
#![allow(async_fn_in_trait)]

pub mod chassis;
pub mod network_kit;

use core::{future::Future, num::NonZeroU8};

/// Enum represents what high-level motion command to execute
#[derive(Copy, Clone, Debug)]
pub enum MotionCommand {
    /// Move forward by a specified number of tiles
    Forward(NonZeroU8),

    /// Move backward by a specified number of tiles
    Backward(NonZeroU8),

    /// Turn left by a specified number of quarter-turns.
    TurnLeft(NonZeroU8),

    /// Turn right by a specified number of quarter-turns.
    TurnRight(NonZeroU8),
}

/// This trait provides some basic utilities from the async runtime.
pub trait AsyncUtils {
    /// This function creates a task that will sleep for the given duration.
    fn sleep(&self, duration: core::time::Duration) -> impl Future<Output = ()> + Send;

    /// This function will write the given data to the log.
    /// Used for debugging.
    fn log(&self, data: &str) -> impl Future<Output = ()> + Send;
}
