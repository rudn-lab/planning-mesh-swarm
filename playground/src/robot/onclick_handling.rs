use bevy::prelude::*;

use crate::robot::selection_reticle::ReticleBundle;

#[derive(Resource, Default)]
pub struct SelectedRobot {
    pub robot: Option<Entity>,
    pub selection_reticle: Option<Entity>,
}

#[derive(Component)]
pub struct SelectedRobotMarker;

pub fn on_click_select(
    click: Trigger<Pointer<Click>>,
    mut selected_robot: ResMut<SelectedRobot>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut commands: Commands,
) {
    println!("click on entity {}", click.entity());
    // If none selected, select me
    // If already selected another, select me instead
    // If already selected me, deselect

    if selected_robot.robot.is_none() {
        // Give me the selection component
        commands.entity(click.entity()).insert(SelectedRobotMarker);
        selected_robot.robot = Some(click.entity());
    } else if selected_robot.robot != Some(click.entity()) {
        // Remove the selection component from me, and give it to the new one
        commands
            .entity(selected_robot.robot.unwrap())
            .remove::<SelectedRobotMarker>();
        commands.entity(click.entity()).insert(SelectedRobotMarker);
        selected_robot.robot = Some(click.entity());
    } else {
        // When deselecting me, remove the selection component from me.
        commands
            .entity(selected_robot.robot.unwrap())
            .remove::<SelectedRobotMarker>();
        selected_robot.robot = None;
    }

    // If the robot is selected, show the selection reticle parented to the robot
    // If the robot is not selected, hide the selection reticle
    if let Some(robot) = selected_robot.robot {
        if let Some(selection_reticle) = selected_robot.selection_reticle {
            // Parent the selection reticle to the robot
            println!("Reparenting");
            commands.entity(selection_reticle).set_parent(robot);
        } else {
            // Create the selection reticle, and parent it
            let selection_reticle = commands
                .spawn(ReticleBundle::new(asset_server, &mut materials))
                .insert(Name::new("Selection Reticle"))
                .set_parent(robot)
                .id();
            println!("Creating {selection_reticle:?}");
            selected_robot.selection_reticle = Some(selection_reticle);
        }
    } else {
        // No robot is selected, hide the selection reticle
        if let Some(selection_reticle) = selected_robot.selection_reticle {
            println!("Despawning");
            commands.entity(selection_reticle).remove_parent().despawn();
            selected_robot.selection_reticle = None;
        }
    }
}
