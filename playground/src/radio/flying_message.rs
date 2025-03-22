use bevy::prelude::*;

use crate::{
    clock::{Simulation, SimulationInstant, SimulationTime},
    robot::motion_types::RobotState,
    CENTIMETER,
};

use super::virtual_nic::{MessageType, VirtualPeerId};

/// Describes how fast a message in flight is moving towards its destination.
/// In scene units per second.
/// Should be faster than robots' max speed,
/// otherwise the robots can outrun the message.
#[derive(Resource)]
pub(crate) struct FlyingMessageSpeed {
    pub(crate) speed: f32,
}

/// The entity with this component represents a message in flight.
#[derive(Component, Debug)]
pub(crate) struct FlyingMessage {
    /// The entity of the NIC that sent this message.
    pub(crate) sender_nic: Entity,

    /// The MAC address of the sender NIC.
    pub(crate) sender_peer_id: VirtualPeerId,

    /// The entity of the robot that the sender NIC is attached to.
    pub(crate) sender_robot: Entity,

    /// The robot that the message is destined for.
    pub(crate) receiver_robot: Entity,

    /// The contents of the message.
    pub(crate) message: MessageType,

    /// When was this message sent.
    pub(crate) sent_when: SimulationInstant,
}

pub(crate) struct FlyingMessagePlugin;

impl Plugin for FlyingMessagePlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<MessageCreatedEvent>();
        app.add_systems(Update, create_flying_message);
        app.add_systems(Update, update_flying_message);
        app.insert_resource(FlyingMessageSpeed { speed: 1000.0 });
    }
}

#[derive(Bundle)]
pub(crate) struct FlyingMessageBundle {
    message: FlyingMessage,
    transform: Transform,
    unpickable: PickingBehavior,
    name: Name,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
}

/// This event is dispatched whenever a robot sends a message to another robot.
#[derive(Event)]
pub(crate) struct MessageCreatedEvent {
    pub(crate) sender_nic: Entity,
    pub(crate) sender_peer_id: VirtualPeerId,
    pub(crate) sender_robot: Entity,
    pub(crate) receiver_robot: Entity,
    pub(crate) message: MessageType,
}

/// This system creates a new message entity whenever
/// an event is dispatched.
fn create_flying_message(
    mut commands: Commands,
    mut events: EventReader<MessageCreatedEvent>,
    time: Res<Time<Simulation>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,

    robots: Query<&GlobalTransform, With<RobotState>>,
) {
    for event in events.read() {
        let mesh = meshes.add(Circle::new(0.1 * CENTIMETER));
        let material = materials.add(ColorMaterial::from(Color::linear_rgb(1.0, 0.0, 1.0)));

        let message = FlyingMessage {
            sender_nic: event.sender_nic,
            sender_peer_id: event.sender_peer_id,
            sender_robot: event.sender_robot,
            receiver_robot: event.receiver_robot,
            message: event.message.clone(),
            sent_when: time.get_instant(),
        };

        let mut transform = robots.get(event.sender_robot).unwrap().compute_transform();
        transform.translation += Vec3::new(0.0, 0.0, 1.0);

        let bundle = FlyingMessageBundle {
            message,
            transform,
            unpickable: PickingBehavior::IGNORE,
            name: Name::new(format!(
                "Msg: {}->{}",
                event.sender_nic, event.receiver_robot
            )),
            mesh: Mesh2d(mesh),
            material: MeshMaterial2d(material),
        };

        commands.spawn(bundle);
    }
}

/// This system updates the entities of
/// messages in flight.
/// If a message has finished flying,
/// it is removed from the world,
/// and its contents are added to the robot.
fn update_flying_message(
    mut commands: Commands,
    time: Res<Time<Simulation>>,
    speed: Res<FlyingMessageSpeed>,
    mut messages: Query<(Entity, &FlyingMessage, &mut Transform)>,
    mut robots: Query<(&GlobalTransform, &mut RobotState)>,
) {
    for (msg_entity, message, mut transform) in messages.iter_mut() {
        let (destination, _) = robots.get(message.receiver_robot).unwrap();

        let vector = destination.translation() - transform.translation;
        let distance = vector.length();
        let normalized_vector = vector / distance;
        let delta_change = normalized_vector * speed.speed * time.delta_secs();

        transform.translation += delta_change;

        // If the message has reached its destination,
        // it is removed from the world,
        // and its contents are added to the robot.

        // Reached destination = distance between message and robot
        // is below what the message overcomes in the delta time.
        if distance < (speed.speed * time.delta_secs()) {
            robots
                .get_mut(message.receiver_robot)
                .unwrap()
                .1
                .queued_messages
                .push((message.sender_peer_id, message.message.clone()));

            commands.entity(msg_entity).despawn();
        }
    }
}
