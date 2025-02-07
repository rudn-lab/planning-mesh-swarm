use bevy::{prelude::*, sprite::Material2dPlugin};

use crate::{robot::onclick_handling::SelectedRobot, CENTIMETER};

use super::{antenna::AntennaReach, antenna_reach_vis_mat::AntennaReachVisualizationMaterial};

/// This component is used as a child of the robot entity to indicate that it has a virtual network interface.
#[derive(Component, Debug)]
pub(crate) struct VirtualNetworkInterface {
    pub(crate) reach: AntennaReach,
    pub(crate) color: Srgba,
}

#[derive(Bundle)]
pub(crate) struct NicBundle {
    nic: VirtualNetworkInterface,
    material: MeshMaterial2d<AntennaReachVisualizationMaterial>,
    mesh: Mesh2d,
    transform: Transform,
    unpickable: PickingBehavior,
}

impl NicBundle {
    pub(crate) fn new(world: &mut World, color: Srgba) -> Self {
        let material = world
            .resource_mut::<Assets<AntennaReachVisualizationMaterial>>()
            .add(AntennaReachVisualizationMaterial {
                shade_color: color.to_vec4(),
                ..Default::default()
            });
        let mesh = world
            .resource_mut::<Assets<Mesh>>()
            .add(Rectangle::new(10000.0, 10000.0));

        Self {
            nic: VirtualNetworkInterface {
                reach: AntennaReach::Circular {
                    max_reach: 10.0 * CENTIMETER,
                },
                color,
            },
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
            .add_systems(PostUpdate, update_antenna_reach_vis_material)
            .add_systems(PostUpdate, sort_nics_by_reach);
    }
}

fn update_antenna_reach_vis_material(
    mut materials: ResMut<Assets<AntennaReachVisualizationMaterial>>,
    mut query: Query<(
        &mut MeshMaterial2d<AntennaReachVisualizationMaterial>,
        &GlobalTransform,
        &VirtualNetworkInterface,
        &Parent,
    )>,
    camera_query: Query<&OrthographicProjection, With<Camera2d>>,
    selected_robot: Res<SelectedRobot>,
    time: Res<Time>,
) {
    let camera = camera_query.single();
    for (mut material, transform, nic, parent) in query.iter_mut() {
        if let Some(material) = materials.get_mut(&mut material.0) {
            match nic.reach {
                AntennaReach::Circular { max_reach } => {
                    material.radius = max_reach;
                }
            }

            material.center = transform.translation().xy();
            material.shade_color = [
                nic.color.red,
                nic.color.green,
                nic.color.blue,
                nic.color.alpha,
            ]
            .into();

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

/// This system sorts all the network interfaces by their reach,
/// so that the largest reach interface is shown with the greatest Z-depth.
fn sort_nics_by_reach(mut query: Query<(&mut Transform, &VirtualNetworkInterface)>) {
    let z_min = -1.0;
    let z_max = -0.1;

    let mut nics = query.iter_mut().collect::<Vec<_>>();
    nics.sort_by_key(|(_, nic)| {
        float_ord::FloatOrd(-match nic.reach {
            AntennaReach::Circular { max_reach } => max_reach,
        })
    });

    let z_step = (z_max - z_min) / nics.len() as f32;
    for (i, (ref mut transform, _)) in nics.iter_mut().enumerate() {
        transform.translation.z = z_min + i as f32 * z_step;
    }
}

/// This function produces a command
/// that will add a virtual network interface to the passed-in robot.
/// The NIC will have some reasonable defaults.
pub(crate) fn add_nic_to_robot(robot: Entity) -> impl Command {
    move |world: &mut World| {
        let nic = NicBundle::new(
            world,
            Srgba {
                alpha: 0.5,
                ..Srgba::WHITE
            },
        );

        let nic_entity = world.spawn(nic).id();
        world.commands().entity(robot).add_child(nic_entity);
    }
}
