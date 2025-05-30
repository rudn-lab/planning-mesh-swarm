use bevy::{
    app::{Plugin, Update},
    ecs::{entity::Entity, event::EventWriter, schedule::IntoSystemConfigs, system::ResMut},
    prelude::Query,
    time::Time,
};
use bevy_egui::EguiContexts;
use egui::{ComboBox, Widget};

use crate::{
    clock::{Simulation, SimulationTime},
    radio::nic_components::{AuraVisualizationMode, SelectedAuraVisualizationMode},
    robot::{motion_types::RobotState, onclick_handling::SelectionChanged},
};

pub(crate) struct Ui;

impl Plugin for Ui {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Update, (left_panel, crate::event_log::bottom_panel).chain());
    }
}

fn left_panel(
    mut contexts: EguiContexts,
    robots: Query<(Entity, &RobotState)>,
    mut ev_writer: EventWriter<SelectionChanged>,
    mut ev_ghost_robot_spawn: EventWriter<crate::robot::robot_spawn_menu::InitGhostRobot>,

    mut aura_mode: ResMut<SelectedAuraVisualizationMode>,
    mut time: ResMut<Time<Simulation>>,
) {
    let ctx = contexts.ctx_mut();
    let mut robots = robots.iter().collect::<Vec<_>>();
    robots.sort_by_key(|v| v.1.id);

    egui::SidePanel::left("left_panel")
        .resizable(true)
        .show(ctx, |ui| {
            ui.label(format!("{} robots", robots.len()));

            for (entity, robot) in robots {
                ui.horizontal(|ui| {
                    let resp = ui.link(format!("Robot {}", robot.id));
                    if resp.clicked() {
                        // TODO: reveal the robot
                        ev_writer.send(SelectionChanged::SelectedRobot(entity));
                    }

                    ui.label(format!("{:?}", robot.grid_pos));
                });
            }

            if ui.button("New Robot").clicked() {
                ev_ghost_robot_spawn.send(crate::robot::robot_spawn_menu::InitGhostRobot);
            }

            ui.separator();

            ui.label("Aura mode:");
            ComboBox::new("aura_mode_combo", "")
                .selected_text(aura_mode.mode.to_string())
                .show_ui(ui, |ui| {
                    for mode in AuraVisualizationMode::variants() {
                        ui.selectable_value(&mut aura_mode.mode, *mode, mode.to_string());
                    }
                });

            ui.separator();

            ui.label("Simulation speed:");

            let mut speed = time.context_mut().speed;
            let resp = egui::Slider::new(&mut speed, 0.01..=100.0)
                .logarithmic(true)
                .show_value(true)
                .ui(ui);
            if resp.changed() {
                time.set_speed(speed);
            }

            ui.allocate_rect(ui.available_rect_before_wrap(), egui::Sense::hover());
        });
}
