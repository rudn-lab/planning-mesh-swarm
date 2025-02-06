use bevy::{prelude::*, sprite::Material2dPlugin};

use crate::robot::onclick_handling::SelectedRobot;

use super::antenna_reach_vis_mat::AntennaReachVisualizationMaterial;

/// This component is used as a child of the robot entity to indicate that it has a virtual network interface.
#[derive(Component)]
pub(crate) struct VirtualNetworkInterface {}

#[derive(Bundle)]
pub(crate) struct NicBundle {
    nic: VirtualNetworkInterface,
    material: MeshMaterial2d<AntennaReachVisualizationMaterial>,
    mesh: Mesh2d,
    transform: Transform,
    unpickable: PickingBehavior,
}

impl NicBundle {
    pub(crate) fn new(world: &mut World) -> Self {
        let material = world
            .resource_mut::<Assets<AntennaReachVisualizationMaterial>>()
            .add(AntennaReachVisualizationMaterial::default());
        let mesh = world
            .resource_mut::<Assets<Mesh>>()
            .add(Rectangle::new(10000.0, 10000.0));

        Self {
            nic: VirtualNetworkInterface {},
            material: MeshMaterial2d(material),
            mesh: Mesh2d(mesh),
            transform: Transform::default().with_translation(Vec3::NEG_Z * 0.5),
            unpickable: PickingBehavior::IGNORE,
        }
    }
}

pub(crate) struct AntennaRenderingPlugin;

impl Plugin for AntennaRenderingPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(Material2dPlugin::<AntennaReachVisualizationMaterial>::default())
            .add_systems(PostUpdate, update_antenna_reach_vis_material);
    }
}

fn update_antenna_reach_vis_material(
    mut materials: ResMut<Assets<AntennaReachVisualizationMaterial>>,
    mut query: Query<(
        &mut MeshMaterial2d<AntennaReachVisualizationMaterial>,
        &GlobalTransform,
        &Parent,
    )>,
    camera_query: Query<&OrthographicProjection, With<Camera2d>>,
    selected_robot: Res<SelectedRobot>,
    time: Res<Time>,
) {
    let camera = camera_query.single();
    for (mut material, transform, parent) in query.iter_mut() {
        if let Some(material) = materials.get_mut(&mut material.0) {
            material.center = transform.translation().xy();
            material.camera_scale = camera.scale;
            material.is_selected = if selected_robot.robot.is_some_and(|v| v == parent.get()) {
                1.0
            } else {
                0.0
            };
            material.time = time.elapsed_secs_wrapped();
        }
    }
}
