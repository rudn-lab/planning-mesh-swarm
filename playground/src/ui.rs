use bevy::{
    app::{Plugin, Update},
    ecs::{entity::Entity, event::EventWriter},
    prelude::Query,
};
use bevy_egui::EguiContexts;

use crate::robot::{motion_types::RobotState, onclick_handling::SelectionChanged};

pub(crate) struct Ui;

impl Plugin for Ui {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Update, left_panel);
        // app.add_systems(Update, top_panel);
    }
}

fn left_panel(
    mut contexts: EguiContexts,
    robots: Query<(Entity, &RobotState)>,
    mut ev_writer: EventWriter<SelectionChanged>,
    mut ev_ghost_robot_spawn: EventWriter<crate::robot::robot_spawn_menu::InitGhostRobot>,
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
            ui.allocate_rect(ui.available_rect_before_wrap(), egui::Sense::hover());
        });
}
