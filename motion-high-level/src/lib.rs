#![no_std]

use core::num::NonZeroU8;

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
