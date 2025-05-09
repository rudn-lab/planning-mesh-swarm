use core::num::NonZeroU8;

use crate::AsyncUtils;

pub trait Chassis {
    async fn forward(&mut self, cells: NonZeroU8);
    async fn backward(&mut self, cells: NonZeroU8);
    async fn turn_left_quarters(&mut self, quarter_turns: NonZeroU8);
    async fn turn_right_quarters(&mut self, quarter_turns: NonZeroU8);

    async fn turn_left(&mut self) {
        self.turn_left_quarters(NonZeroU8::new(1).unwrap()).await
    }

    async fn turn_right(&mut self) {
        self.turn_right_quarters(NonZeroU8::new(1).unwrap()).await
    }

    async fn forward_one(&mut self) {
        self.forward(NonZeroU8::new(1).unwrap()).await
    }

    async fn backward_one(&mut self) {
        self.backward(NonZeroU8::new(1).unwrap()).await
    }

    async fn turn_left_signed(&mut self, quarters: i8) {
        if quarters > 0 {
            self.turn_left_quarters(NonZeroU8::new(quarters as u8).unwrap())
                .await
        } else {
            self.turn_right_quarters(NonZeroU8::new(-quarters as u8).unwrap())
                .await
        }
    }

    async fn turn_right_signed(&mut self, quarters: i8) {
        if quarters > 0 {
            self.turn_right_quarters(NonZeroU8::new(quarters as u8).unwrap())
                .await
        } else {
            self.turn_left_quarters(NonZeroU8::new(-quarters as u8).unwrap())
                .await
        }
    }

    /// Get the color currently shown on the indicator LED.
    /// If the indicator LED is not available, this returns a constant value.
    async fn get_led_color(&self) -> [u8; 3];

    /// Set the color shown on the indicator LED.
    /// If the indicator LED is not available, this function does nothing.
    async fn set_led_color(&mut self, color: [u8; 3]);

    fn utils(&self) -> impl AsyncUtils + Send;
}
