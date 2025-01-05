use std::num::NonZero;

use motion_high_level::chassis::Chassis;

pub(crate) async fn simple_business_logic(mut chassis: impl Chassis) {
    loop {
        chassis.log("Doing right turn").await;
        chassis.forward(NonZero::new(2).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;

        chassis.log("Going back to start").await;
        chassis.forward(NonZero::new(1).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;

        chassis.log("Waiting").await;
        async_std::task::sleep(std::time::Duration::from_secs(2)).await;
    }
}
