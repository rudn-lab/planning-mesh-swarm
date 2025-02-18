use bevy::prelude::*;
use bevy_egui::EguiContexts;

use crate::robot::{
    motion_types::RobotState,
    onclick_handling::{SelectedRobot, SelectedRobotMarker},
};

use super::nic_components::VirtualNetworkInterface;

pub(crate) struct RadioReachTooltipPlugin;

impl Plugin for RadioReachTooltipPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, init_radio_reach);
        app.add_systems(Update, update_line_and_tooltip);
    }
}

/// This component marks the entity that draws a line between the currently selected and the currently hovered robot
#[derive(Component)]
struct RadioReachLine;

/// This resource holds the entity of the robot that is currently hovered
#[derive(Resource)]
pub(crate) struct HoveredRobot(pub(crate) Option<Entity>);

#[derive(Bundle)]
struct RadioReachLineBundle {
    line: RadioReachLine,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    name: Name,
}

impl RadioReachLineBundle {
    pub(crate) fn new(world: &mut World) -> Self {
        let mesh = world
            .resource_mut::<Assets<Mesh>>()
            .add(Rectangle::new(10.0, 1.0));
        let material = world
            .resource_mut::<Assets<ColorMaterial>>()
            .add(ColorMaterial::from(Color::WHITE));
        Self {
            line: RadioReachLine,
            mesh: Mesh2d(mesh),
            material: MeshMaterial2d(material),
            name: Name::new("Radio Reach Line"),
        }
    }
}

fn init_radio_reach(world: &mut World) {
    let bundle = RadioReachLineBundle::new(world);
    let mut commands = world.commands();

    commands.spawn(bundle);

    world.insert_resource(HoveredRobot(None));

    world.add_observer(on_hover_enter);
    world.add_observer(on_hover_exit);
}

fn on_hover_enter(
    trigger: Trigger<Pointer<Over>>,
    mut hovered_robot: ResMut<HoveredRobot>,
    all_robots: Query<Option<&SelectedRobotMarker>, (With<RobotState>, Without<RadioReachLine>)>,
) {
    // Ensure that the entity that was hovered is a robot, and is not the selected robot
    match all_robots.get(trigger.entity()) {
        // If the hovered entity *is* a robot, and *does* have the marker, then do nothing
        Ok(Some(_)) => return,
        // If the hovered entity *is not* a robot, do nothing
        Err(_) => return,

        // The hovered entity is a robot, but does not have the marker -- OK!
        _ => (),
    }

    hovered_robot.0 = Some(trigger.entity());
}

fn on_hover_exit(
    trigger: Trigger<Pointer<Out>>,
    mut hovered_robot: ResMut<HoveredRobot>,
    all_robots: Query<Option<&SelectedRobotMarker>, With<RobotState>>,
) {
    // Ensure that the entity that was hovered is a robot, and is not the selected robot
    match all_robots.get(trigger.entity()) {
        // If the hovered entity *is* a robot, and *does* have the marker, then do nothing
        Ok(Some(_)) => return,
        // If the hovered entity *is not* a robot, do nothing
        Err(_) => return,

        // The hovered entity is a robot, but does not have the marker -- OK!
        _ => (),
    }

    hovered_robot.0 = None;
}

fn update_line_and_tooltip(
    mut egui_contexts: EguiContexts,
    selected_robot: Res<SelectedRobot>,
    hovered_robot: Res<HoveredRobot>,
    mut radio_reach_line: Query<&mut Transform, (With<RadioReachLine>, Without<RobotState>)>,
    all_robots: Query<
        (Option<&SelectedRobotMarker>, &Transform),
        (With<RobotState>, Without<RadioReachLine>),
    >,
    nics: Query<(&Parent, &VirtualNetworkInterface)>,
) {
    let mut radio_reach_line_transform = radio_reach_line.single_mut();

    // If either the selected or hovered robot does not exist, hide the line.
    let (selected_entity, hovered_entity) = match (selected_robot.robot, hovered_robot.0) {
        (Some(selected), Some(hovered)) => (selected, hovered),
        _ => {
            radio_reach_line_transform.scale.y = 0.0;
            return;
        }
    };

    // Calculate the line between the selected and hovered robot
    let selected_robot_transform = all_robots.get(selected_entity).unwrap().1;
    let hovered_robot_transform = all_robots.get(hovered_entity).unwrap().1;

    let distance = selected_robot_transform
        .translation
        .distance(hovered_robot_transform.translation);

    // The angle of the line is the angle between the two origins
    let x_offset = hovered_robot_transform.translation.x - selected_robot_transform.translation.x;
    let y_offset = hovered_robot_transform.translation.y - selected_robot_transform.translation.y;
    let angle = f32::atan2(y_offset, x_offset) + std::f32::consts::FRAC_PI_2;

    // The line starts at the selected robot, and ends at the hovered robot
    // Because the line is a rectangle, its origin needs to be at the midpoint between the robots.
    radio_reach_line_transform.translation =
        (selected_robot_transform.translation + hovered_robot_transform.translation) / 2.0;

    // The height of the rectangle is the distance
    radio_reach_line_transform.scale.y = distance;

    // The rotation of the rectangle is the angle
    radio_reach_line_transform.rotation = Quat::from_rotation_z(angle);

    // Get the nics belonging to the currently selected robot
    let selected_robot_nics = nics
        .iter()
        .filter(|(parent, _)| parent.get() == selected_entity)
        .map(|(_, nic)| nic)
        .collect::<Vec<_>>();

    let my_position = selected_robot_transform.translation.xy();
    let their_position = hovered_robot_transform.translation.xy();
    // Show the tooltip at the position of the hovered robot
    // (which is implicitly the current position of the cursor)
    egui::show_tooltip(
        egui_contexts.ctx_mut(),
        egui::LayerId::background(),
        egui::Id::new("radio_reach_tooltip"),
        |ui| {
            if selected_robot_nics.is_empty() {
                ui.label("Selected robot has no NICs");
            } else {
                ui.label("Sender signal strength for target:");
            }

            for (idx, nic) in selected_robot_nics.iter().enumerate() {
                ui.horizontal(|ui| {
                    ui.label(format!("NIC {}: ", idx));
                    let signal = nic.reach.get_signal_strength(my_position, their_position);
                    ui.add(egui::ProgressBar::new(signal).show_percentage());
                });
            }
        },
    );
}
