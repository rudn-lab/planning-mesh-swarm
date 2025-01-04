use bevy::{
    app::{Plugin, Update},
    prelude::{Camera2d, Local, OrthographicProjection, Query, Res, With},
};
use bevy_egui::EguiContexts;
use bevy_pancam::PanCam;

use crate::pause_controller::PauseState;

pub struct Ui;

impl Plugin for Ui {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.add_systems(Update, left_panel);
        app.add_systems(Update, top_panel);
    }
}

fn left_panel(mut contexts: EguiContexts, mut is_last_selected: Local<bool>) {
    let ctx = contexts.ctx_mut();
    egui::SidePanel::left("left_panel")
        .resizable(true)
        .show(ctx, |ui| {
            ui.label("Left resizeable panel");
            if ui
                .add(egui::widgets::Button::new("A button").selected(!*is_last_selected))
                .clicked()
            {
                *is_last_selected = false;
            }
            if ui
                .add(egui::widgets::Button::new("Another button").selected(*is_last_selected))
                .clicked()
            {
                *is_last_selected = true;
            }

            ui.allocate_rect(ui.available_rect_before_wrap(), egui::Sense::hover());
        });
}

fn top_panel(
    mut contexts: EguiContexts,
    camera: Query<(&PanCam, &OrthographicProjection), With<Camera2d>>,
    pause_state: Res<PauseState>,
) {
    let ctx = contexts.ctx_mut();
    let camera = camera.single();

    egui::TopBottomPanel::top("top_panel")
        .resizable(true)
        .show(ctx, |ui| {
            ui.label("Top resizeable panel");
            ui.label(format!("{:?}", camera.1));
            ui.label(format!("Paused: {:?}", pause_state.paused));
            ui.allocate_rect(ui.available_rect_before_wrap(), egui::Sense::hover());
        });
}
