use bevy::prelude::*;
use bevy_egui::EguiContexts;
use egui::Color32;

use super::{
    motion_types::RobotState,
    onclick_handling::{SelectedRobotMarker, SelectionChanged},
};

pub struct InternalStatePlugin;

impl Plugin for InternalStatePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, visualize_internal_state);
    }
}

fn visualize_internal_state(
    mut ctxs: EguiContexts,
    selected_robot: Query<(Entity, &mut RobotState), With<SelectedRobotMarker>>,
    mut should_be_open: Local<bool>,
    mut did_initialize: Local<bool>,
    mut writer: EventWriter<SelectionChanged>,
) {
    // By default, the Local variables are set to false.
    // However, the egui window is open iff the variable is true; egui will set it to false to signal a closure.
    // So, we first initialize it to true.
    if !*did_initialize {
        *did_initialize = true;
        *should_be_open = true;
    }

    for (entity, robot_state) in selected_robot.iter().take(1) {
        let ctx = ctxs.ctx_mut();

        let window = egui::Window::new("Robot State")
            .open(&mut should_be_open)
            .show(ctx, |ui| {
                ui.columns(2, |col| {
                    let mut label = |text: &'static str, value: String| {
                        col[0].label(text);
                        col[1].label(format!("{}", value));
                    };

                    label("ID", robot_state.id.to_string());
                    label("Grid Position", format!("{:?}", robot_state.grid_pos));
                    label("Drive rate", robot_state.drive_rate.to_string());
                    label("Turn rate", robot_state.turn_rate.to_string());
                });

                ui.separator();
                ui.label("Log");

                egui::ScrollArea::vertical()
                    .animated(true)
                    .drag_to_scroll(true)
                    .stick_to_bottom(true)
                    .show(ui, |ui| {
                        for log in &robot_state.log {
                            ui.label(format!("{}", log));
                        }

                        if robot_state.log.is_empty() {
                            ui.colored_label(Color32::GRAY, "No messages yet...");
                        }
                    });
            });

        if let Some(window) = window {
            println!("Window hover: {:?}", window.response.hover_pos());
        }

        // After egui runs, if should_be_open gets updated to false, then we need to emit the deselect event.
        // We also reset the did_initialize variable:
        // on the next frame, we will reset to the initial state, but there will be no entity selected.
        // As a result, we will be ready for the next selection.
        if !*should_be_open {
            writer.send(SelectionChanged::DeselectedRobot(entity));
            *did_initialize = false;
        }
    }
}
