use core::num::NonZeroU8;

#[allow(async_fn_in_trait)]
pub trait Chassis {
    async fn forward(&mut self, cells: NonZeroU8);
    async fn backward(&mut self, cells: NonZeroU8);
    async fn turn_left(&mut self, quarter_turns: NonZeroU8);
    async fn turn_right(&mut self, quarter_turns: NonZeroU8);

    /// Write a message to the internal log. This is used for debugging the robot.
    /// Note that this can block, for example if the simulation is paused.
    async fn log(&mut self, message: &str);
}
