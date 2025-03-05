use std::{num::NonZero, time::Duration};

use high_level_cmds::{chassis::Chassis, AsyncUtils};

pub(crate) async fn simple_business_logic(mut chassis: impl Chassis + Send) {
    loop {
        chassis.utils().log("Doing right turn").await;
        chassis.forward(NonZero::new(2).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;

        chassis.utils().log("Going back to start").await;
        chassis.forward(NonZero::new(1).unwrap()).await;
        chassis.turn_right(NonZero::new(1).unwrap()).await;

        chassis.utils().log("Waiting").await;
        chassis.utils().sleep(Duration::from_secs(2)).await;
    }
}
