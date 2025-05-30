use bevy::prelude::*;

use crate::robot::selection_reticle::ReticleBundle;

use super::{motion_types::RobotState, RobotId};

#[derive(Resource, Default)]
pub(crate) struct SelectedRobot {
    pub(crate) robot: Option<Entity>,
    pub(crate) selection_reticle: Option<Entity>,
}

#[derive(Component)]
pub(crate) struct SelectedRobotMarker;

#[derive(Event)]
pub(crate) enum SelectionChanged {
    /// The robot with the given entity is now selected.
    SelectedRobot(Entity),

    /// The robot with the given RobotId is now selected.
    SelectedRobotById(RobotId),

    /// No robots are selected now. The provided robot has just been deselected.
    DeselectedRobot(Entity),
}

/// This system runs whenever a robot is clicked.
pub(crate) fn on_click_robot(
    click: Trigger<Pointer<Click>>,
    selected_robot_query: Query<Entity, With<SelectedRobotMarker>>,
    all_robots: Query<Entity, With<RobotState>>,
    mut writer: EventWriter<SelectionChanged>,
) {
    // Ensure that the entity that was clicked is a robot
    if all_robots.get(click.entity()).is_err() {
        return;
    }

    // If there was no robot selected, then select me
    if selected_robot_query.is_empty() {
        writer.send(SelectionChanged::SelectedRobot(click.entity()));
    } else {
        let old_selected_robot = selected_robot_query.single();
        // If the robot that was just clicked is me, then deselect me
        if old_selected_robot == click.entity() {
            writer.send(SelectionChanged::DeselectedRobot(click.entity()));
        } else {
            // Otherwise, select me instead
            writer.send(SelectionChanged::SelectedRobot(click.entity()));
        }
    }
}

/// This system updates the SelectedRobot resource, the selection reticle, and the marker components.
pub(crate) fn on_selection_event(
    mut reader: EventReader<SelectionChanged>,
    mut commands: Commands,
    mut selected_robot: ResMut<SelectedRobot>,
    mut hovered_robot: ResMut<crate::radio::radio_reach_tooltip::HoveredRobot>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    all_robots: Query<(Entity, &RobotState)>,
) {
    for event in reader.read() {
        match event {
            SelectionChanged::SelectedRobot(entity) => {
                // The old robot, if any, needs to be unselected
                if let Some(old_selected_robot) = selected_robot.robot {
                    commands
                        .entity(old_selected_robot)
                        .remove::<SelectedRobotMarker>();
                }

                // The new robot is now selected
                commands.entity(*entity).insert(SelectedRobotMarker);
                selected_robot.robot = Some(*entity);

                // No robot is now hovered (because we're now hovering over the robot we just clicked)
                hovered_robot.0 = None;
            }
            SelectionChanged::SelectedRobotById(robot_id) => {
                // The old robot, if any, needs to be unselected
                if let Some(old_selected_robot) = selected_robot.robot {
                    commands
                        .entity(old_selected_robot)
                        .remove::<SelectedRobotMarker>();
                }

                // find the new robot
                let Some((entity, robot_state)) = all_robots
                    .iter()
                    .find(|(_, robot_state)| robot_state.id == *robot_id)
                else {
                    return;
                };

                // The new robot is now selected
                commands.entity(entity).insert(SelectedRobotMarker);
                selected_robot.robot = Some(entity);

                // No robot is now hovered (because we're now hovering over the robot we just clicked)
                hovered_robot.0 = None;
            }

            SelectionChanged::DeselectedRobot(entity) => {
                commands.entity(*entity).remove::<SelectedRobotMarker>();
                selected_robot.robot = None;

                // No robot is now hovered
                hovered_robot.0 = None;
            }
        }

        // If there is a selected robot now, then make it so that the selection reticle is parented to it.
        if let Some(robot) = selected_robot.robot {
            if let Some(selection_reticle) = selected_robot.selection_reticle {
                // Parent the selection reticle to the current selection
                commands.entity(selection_reticle).set_parent(robot);
            } else {
                // Create the selection reticle, and parent it
                let selection_reticle = commands
                    .spawn(ReticleBundle::new(&asset_server, &mut materials))
                    .id();
                commands.entity(robot).add_child(selection_reticle);
                selected_robot.selection_reticle = Some(selection_reticle);
            }
        } else {
            // No robot is selected, hide the selection reticle
            if let Some(selection_reticle) = selected_robot.selection_reticle {
                commands.entity(selection_reticle).despawn_recursive();
                selected_robot.selection_reticle = None;
            }
        }
    }
}
