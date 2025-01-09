use bevy::{prelude::*, sprite::Material2dPlugin};

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
    )>,
) {
    for (mut material, transform) in query.iter_mut() {
        if let Some(material) = materials.get_mut(&mut material.0) {
            material.center = transform.translation().xy();
        }
    }
}
