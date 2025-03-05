use core::num::NonZeroU8;

use crate::AsyncUtils;

pub trait Chassis {
    async fn forward(&mut self, cells: NonZeroU8);
    async fn backward(&mut self, cells: NonZeroU8);
    async fn turn_left(&mut self, quarter_turns: NonZeroU8);
    async fn turn_right(&mut self, quarter_turns: NonZeroU8);

    fn utils(&self) -> impl AsyncUtils + Send;
}
