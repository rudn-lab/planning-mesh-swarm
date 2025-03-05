use std::collections::VecDeque;

use bevy::{prelude::*, sprite::Material2dPlugin};

use crate::{robot::onclick_handling::SelectedRobot, CENTIMETER};

use super::{
    antenna::AntennaReach, antenna_reach_vis_mat::AntennaReachVisualizationMaterial,
    radio_reach_tooltip::HoveredRobot, virtual_nic::MessageType,
};

#[derive(Resource, Clone, Copy, Default)]
pub(crate) struct SelectedAuraVisualizationMode {
    pub(crate) mode: AuraVisualizationMode,
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, Default)]
pub(crate) enum AuraVisualizationMode {
    None,
    Selected,
    SelectedAndHovered,
    #[default]
    All,
}

impl AuraVisualizationMode {
    pub(crate) fn variants() -> &'static [AuraVisualizationMode] {
        &[
            AuraVisualizationMode::None,
            AuraVisualizationMode::Selected,
            AuraVisualizationMode::SelectedAndHovered,
            AuraVisualizationMode::All,
        ]
    }
}

impl ToString for AuraVisualizationMode {
    fn to_string(&self) -> String {
        match self {
            AuraVisualizationMode::None => "None",
            AuraVisualizationMode::Selected => "Only selected",
            AuraVisualizationMode::SelectedAndHovered => "Selected and hovered",
            AuraVisualizationMode::All => "All",
        }
        .to_string()
    }
}

/// This component is used as a child of the robot entity to indicate that it has a virtual network interface.
/// In the model, it's a sender interface:
/// every robot has a receiver interface implicitly.
#[derive(Component, Debug)]
pub(crate) struct VirtualNetworkInterface {
    /// The sequential number of this NIC.
    /// The first NIC on a robot is 0, the second is 1, etc.
    ///
    /// If any NICs are removed, these indexes are (treated as if they are) shifted backwards.
    pub(crate) index: usize,
    pub(crate) reach: AntennaReach,
    pub(crate) color: Srgba,

    /// If this NIC is paired with a receiver, this is the index of the robot whose receiver it is paired with.
    // pub(crate) paired_with: Option<VirtualPeerId>,

    /// If this NIC is paired with a receiver, this is the entity that represents that pairing.
    pub(crate) peer_connection: Option<Entity>,

    /// The messages that this NIC would like to send to its pair.
    pub(crate) pending_messages: VecDeque<MessageType>,
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
    pub(crate) fn new(world: &mut World, color: Srgba, index: usize) -> Self {
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
                index,
                // paired_with: None,
                peer_connection: None,
                pending_messages: VecDeque::new(),
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
    hovered_robot: Res<HoveredRobot>,
    mode: Res<SelectedAuraVisualizationMode>,
    time: Res<Time>,
) {
    let camera = camera_query.single();
    for (mut material, transform, nic, parent) in query.iter_mut() {
        if let Some(material) = materials.get_mut(&mut material.0) {
            let this_robot_is_selected = selected_robot.robot.is_some_and(|v| v == parent.get());
            let this_robot_is_hovered = hovered_robot.0.is_some_and(|v| v == parent.get());

            let should_hide_this = match mode.mode {
                AuraVisualizationMode::None => true,
                AuraVisualizationMode::Selected => !this_robot_is_selected,
                AuraVisualizationMode::SelectedAndHovered => {
                    !this_robot_is_selected && !this_robot_is_hovered
                }
                AuraVisualizationMode::All => false,
            };

            if should_hide_this {
                material.is_selected = 0.0;
                material.shade_color = Vec4::ZERO;
                continue;
            }

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
            material.is_selected = match mode.mode {
                AuraVisualizationMode::None => 0.0,
                AuraVisualizationMode::Selected => this_robot_is_selected.into(),
                AuraVisualizationMode::SelectedAndHovered => {
                    (this_robot_is_hovered || this_robot_is_selected).into()
                }
                AuraVisualizationMode::All => this_robot_is_selected.into(),
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
///
/// Provide the greatest index of all the NICs that are currently attached to the robot.
pub(crate) fn add_nic_to_robot(robot: Entity, max_nic_index: usize) -> impl Command {
    move |world: &mut World| {
        let nic = NicBundle::new(
            world,
            Srgba {
                alpha: 0.5,
                ..Srgba::WHITE
            },
            max_nic_index + 1,
        );

        let nic_entity = world.spawn(nic).id();
        world.commands().entity(robot).add_child(nic_entity);
    }
}
