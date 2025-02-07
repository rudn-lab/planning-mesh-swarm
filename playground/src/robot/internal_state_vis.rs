use bevy::prelude::*;
use bevy_egui::EguiContexts;
use egui::{Color32, RichText};

use crate::{
    radio::{
        antenna::AntennaReach,
        nic_components::{add_nic_to_robot, VirtualNetworkInterface},
    },
    CENTIMETER,
};

use super::{
    motion_types::{RobotProps, RobotState},
    onclick_handling::{SelectedRobotMarker, SelectionChanged},
};

pub(crate) struct InternalStatePlugin;

impl Plugin for InternalStatePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, visualize_internal_state);
    }
}

fn visualize_internal_state(
    mut ctxs: EguiContexts,
    mut selected_robot: Query<
        (Entity, &mut RobotState, &mut RobotProps, &Children),
        With<SelectedRobotMarker>,
    >,
    robot_nics: Query<&VirtualNetworkInterface>,
    mut should_be_open: Local<bool>,
    mut did_initialize: Local<bool>,
    mut writer: EventWriter<SelectionChanged>,
    mut commands: Commands,
) {
    // By default, the Local variables are set to false.
    // However, the egui window is open iff the variable is true; egui will set it to false to signal a closure.
    // So, we first initialize it to true.
    if !*did_initialize {
        *did_initialize = true;
        *should_be_open = true;
    }

    for (entity, mut robot_state, mut props, children) in selected_robot.iter_mut().take(1) {
        let ctx = ctxs.ctx_mut();

        let this_robot_nics = children
            .iter()
            .filter_map(|child| robot_nics.get(*child).ok().map(|v| (*child, v)))
            .collect::<Vec<_>>();

        egui::Window::new("Robot State")
            .open(&mut should_be_open)
            .max_height(300.0)
            .show(ctx, |ui| {
                ui.columns(2, |col| {
                    let mut label = |text: &'static str, value: String| {
                        col[0].label(text);
                        col[1].label(format!("{}", value));
                    };

                    label("ID", robot_state.id.to_string());
                    label("Grid Position", format!("{:?}", robot_state.grid_pos));
                    // label("Drive rate", robot_state.drive_rate.to_string());
                    // label("Turn rate", robot_state.turn_rate.to_string());
                });

                ui.separator();
                // Drive and turn speeds
                {
                    ui.columns(2, |col| {
                        col[0].label("Drive speed, cm/s");
                        col[1].add(
                            egui::widgets::Slider::new(&mut props.drive_speed, 0.1..=500.0)
                                .show_value(true),
                        );

                        col[0].label("Turn speed, rad/s");
                        col[1].add(
                            egui::widgets::Slider::new(&mut props.turn_speed, 0.1..=15.0)
                                .show_value(true),
                        );
                    });
                }

                ui.separator();

                // Robot's NICs
                {
                    ui.horizontal(|ui| {
                        ui.label(format!("{} NICs", this_robot_nics.len()));

                        if ui.button("+").clicked() {
                            commands.queue(add_nic_to_robot(entity));
                        }
                    });

                    ui.push_id("robot_nics", |ui| {
                        egui::ScrollArea::vertical()
                            .animated(true)
                            .drag_to_scroll(true)
                            .stick_to_bottom(true)
                            .auto_shrink([false, true]) // Shrink vertically, but not horizontally
                            .show(ui, |ui| {
                                for (entity, nic) in this_robot_nics {
                                    nic_list_item(ui, nic, &mut commands, entity);
                                }
                            });
                    });
                }

                ui.separator();

                // Robot's log
                {
                    ui.columns(2, |col| {
                        col[0].label("Log");
                        let btn = col[1].button("Clear");
                        if btn.clicked() {
                            robot_state.log.clear();
                        }
                    });

                    egui::ScrollArea::vertical()
                        .animated(true)
                        .drag_to_scroll(true)
                        .stick_to_bottom(true)
                        .auto_shrink([false, true]) // Shrink vertically, but not horizontally
                        .show(ui, |ui| {
                            for log in &robot_state.log {
                                ui.label(RichText::new(log).monospace());
                            }

                            if robot_state.log.is_empty() {
                                ui.colored_label(Color32::GRAY, "No messages yet...");
                            }
                        });
                }
            });

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

fn nic_list_item(
    ui: &mut egui::Ui,
    nic: &VirtualNetworkInterface,
    commands: &mut Commands,
    nic_entity: Entity,
) {
    let mut color = [
        nic.color.red,
        nic.color.green,
        nic.color.blue,
        nic.color.alpha,
    ];

    ui.horizontal(|ui| {
        let resp = ui.color_edit_button_rgba_premultiplied(&mut color);
        if resp.changed() {
            let color = color;
            commands
                .entity(nic_entity)
                .entry::<VirtualNetworkInterface>()
                .and_modify(move |mut n| {
                    n.color.red = color[0];
                    n.color.green = color[1];
                    n.color.blue = color[2];
                    n.color.alpha = color[3];
                });
        }

        ui.label("Reach:");

        match nic.reach {
            AntennaReach::Circular { max_reach } => {
                let mut max_reach = max_reach / CENTIMETER;
                if ui
                    .add(
                        egui::DragValue::new(&mut max_reach)
                            .speed(0.1)
                            .fixed_decimals(2),
                    )
                    .changed()
                {
                    let max_reach = max_reach * CENTIMETER;
                    commands
                        .entity(nic_entity)
                        .entry::<VirtualNetworkInterface>()
                        .and_modify(move |mut n| {
                            n.reach = AntennaReach::Circular { max_reach };
                        });
                }

                ui.label("cm");

                if ui.button("-").clicked() {
                    commands.entity(nic_entity).despawn_recursive();
                }
            }
        }
    });
}
