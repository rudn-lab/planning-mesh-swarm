use bevy::diagnostic::DiagnosticsStore;
use bevy::diagnostic::FrameTimeDiagnosticsPlugin;
use bevy::prelude::*;
use bevy_egui::EguiContexts;

// Global resource for FPS window visibility flag
#[derive(Resource)]
pub(crate) struct FpsTextVisible(pub(crate) bool);

fn fps_text_update_system(
    diagnostics: Res<DiagnosticsStore>,
    state: Res<FpsTextVisible>,
    mut contexts: EguiContexts,
) {
    if !state.0 {
        return;
    }
    egui::Window::new("FPS")
        .fixed_size(egui::Vec2::new(150.0, 50.0))
        .show(contexts.ctx_mut(), |ui| {
            let fps = diagnostics
                .get(&FrameTimeDiagnosticsPlugin::FPS)
                .and_then(|fps| fps.smoothed());
            let frame_time = diagnostics
                .get(&FrameTimeDiagnosticsPlugin::FRAME_TIME)
                .and_then(|frame_time| frame_time.value());
            ui.columns(2, |ui| {
                ui[0].label("FPS:");
                if let Some(value) = fps {
                    let color = if value >= 60.0 {
                        // Above 60 FPS, use green color
                        egui::Color32::GREEN
                    } else if value >= 30.0 {
                        // Between 30-60 FPS, gradually transition from yellow to green
                        egui::Color32::from_rgb(
                            ((1.0 - (value - 30.0) / (60.0 - 30.0)) as f32 * 255.0) as u8,
                            255,
                            0,
                        )
                    } else if value >= 15.0 {
                        // Between 15-30 FPS, gradually transition from red to yellow
                        egui::Color32::from_rgb(
                            255,
                            (((value - 15.0) / (30.0 - 15.0)) as f32 * 255.0) as u8,
                            0,
                        )
                    } else {
                        // Below 15 FPS, use red color
                        egui::Color32::RED
                    };
                    ui[1].colored_label(color, format!("{:4.1}", value));
                } else {
                    // display "N/A" if we can't get a FPS measurement
                    ui[1].colored_label(egui::Color32::WHITE, " N/A");
                }

                ui[0].label("Frame time:");
                if let Some(value) = frame_time {
                    ui[1].colored_label(egui::Color32::WHITE, format!("{:4.2} ms", value));
                } else {
                    ui[1].colored_label(egui::Color32::WHITE, " N/A");
                }
            })
        });
}

/// Toggle the FPS counter when pressing F12
fn fps_counter_showhide(mut state: ResMut<FpsTextVisible>, kbd: Res<ButtonInput<KeyCode>>) {
    if kbd.just_pressed(KeyCode::F12) {
        state.0 = !state.0;
    }
}

pub(crate) struct FpsMonitorPlugin;

impl Plugin for FpsMonitorPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(FrameTimeDiagnosticsPlugin);
        app.insert_resource(FpsTextVisible(true));
        app.add_systems(Update, (fps_text_update_system, fps_counter_showhide));
    }
}
