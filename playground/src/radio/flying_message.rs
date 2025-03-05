use std::time::Duration;

use bevy::prelude::*;

/// The entity with this component represents a message in flight.
#[derive(Component)]
pub(crate) struct FlyingMessage {
    /// The entity of the NIC that sent this message.
    pub(crate) sender_nic: Entity,

    /// The entity of the robot that the sender NIC is attached to.
    pub(crate) sender_robot: Entity,

    /// The robot that the message is destined for.
    pub(crate) receiver_robot: Entity,

    /// The contents of the message.
    pub(crate) message: String,

    /// When was this message sent, according to the [`Virtual`] timer.
    pub(crate) sent_when: Duration,
}
