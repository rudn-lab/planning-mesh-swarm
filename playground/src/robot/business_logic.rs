use std::num::NonZero;

use motion_high_level::chassis::Chassis;

pub async fn simple_business_logic(mut chassis: impl Chassis) {
    loop {
        chassis.forward(NonZero::new(2).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;
        chassis.forward(NonZero::new(1).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;
    }
}
