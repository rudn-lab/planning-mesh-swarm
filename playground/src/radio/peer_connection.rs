use bevy::prelude::*;

use crate::robot::motion_types::RobotState;

use super::virtual_nic::VirtualPeerId;

/// If an entity with this component exists, then one robot's sender NIC is paired with a receiver.
#[derive(Component)]
pub(crate) struct PeerConnection {
    /// The entity of the sender NIC.
    pub(crate) source_nic: Entity,

    /// The entity of the robot. The NIC is a child of the robot.
    pub(crate) source_robot: Entity,

    /// What robot are we connecting to?
    pub(crate) destination: Entity,

    /// What is the destionation robot's MAC address?
    /// Useful if the destination robot disappears.
    pub(crate) destination_peer_id: VirtualPeerId,
}

#[derive(Bundle)]
pub(crate) struct PeerConnectionBundle {
    pub(crate) peer_connection: PeerConnection,
    pub(crate) transform: Transform,
    pub(crate) material: MeshMaterial2d<ColorMaterial>,
    pub(crate) mesh: Mesh2d,
    pub(crate) unpickable: PickingBehavior,
    pub(crate) name: Name,
}

impl PeerConnectionBundle {
    pub(crate) fn new(
        source_nic: Entity,
        source_robot: Entity,
        destination: Entity,
        destination_peer_id: VirtualPeerId,

        materials: &mut Assets<ColorMaterial>,
        meshes: &mut Assets<Mesh>,
    ) -> PeerConnectionBundle {
        PeerConnectionBundle {
            peer_connection: PeerConnection {
                source_nic,
                source_robot,
                destination,
                destination_peer_id,
            },
            transform: Transform::default(),
            material: MeshMaterial2d(materials.add(ColorMaterial::from(Color::WHITE))),
            mesh: Mesh2d::from(meshes.add(Rectangle::new(2.0, 1.0))),
            unpickable: PickingBehavior::IGNORE,
            name: Name::new(format!(
                "Peer Connection between {} and {}",
                source_robot, destination
            )),
        }
    }
}

pub(crate) struct PeerConnectionPlugin;

impl Plugin for PeerConnectionPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, peer_connection_update);
    }
}

fn peer_connection_update(
    mut connections: Query<(&mut PeerConnection, &mut Transform), Without<RobotState>>,
    robots: Query<&Transform, With<RobotState>>,
) {
    for (connection, mut transform) in connections.iter_mut() {
        let source_robot = robots.get(connection.source_robot).unwrap();
        let destination_robot = robots.get(connection.destination).unwrap();

        // The center of the line is in the middle of the two robots.
        transform.translation.x =
            (source_robot.translation.x + destination_robot.translation.x) / 2.0;
        transform.translation.y =
            (source_robot.translation.y + destination_robot.translation.y) / 2.0;
        transform.translation.z = -0.5;

        // The angle of the line is the angle between the two robots.
        let angle = (source_robot.translation.x - destination_robot.translation.x)
            .atan2(source_robot.translation.y - destination_robot.translation.y);
        transform.rotation = Quat::from_rotation_z(-angle);

        // The length of the line is the distance between the two robots.
        transform.scale.y = source_robot
            .translation
            .distance(destination_robot.translation);
    }
}
