use core::num::NonZeroU8;

#[allow(async_fn_in_trait)]
pub trait Chassis {
    async fn forward(&mut self, cells: NonZeroU8);
    async fn backward(&mut self, cells: NonZeroU8);
    async fn turn_left(&mut self, quarter_turns: NonZeroU8);
    async fn turn_right(&mut self, quarter_turns: NonZeroU8);
}
