use bevy::{
    asset::AssetMetaCheck, input::common_conditions::input_toggle_active, prelude::*,
    window::WindowResolution, winit::WinitSettings,
};
use bevy_egui::EguiPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::PanCamPlugin;
use bevy_rand::{plugin::EntropyPlugin, prelude::WyRand};
use bevy_tweening::TweeningPlugin;

use crate::{
    animator::SimAnimatorPlugin, clock::SimulationClockPlugin,
    radio::flying_message::FlyingMessagePlugin, robot::robot_spawn_menu::GhostRobotPlugin,
};

/// Plugin that contains most of the plugins we're using
pub(super) struct MyPlugins;

impl Plugin for MyPlugins {
    fn build(&self, app: &mut bevy::app::App) {
        app.add_plugins(
            DefaultPlugins
                .build()
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        fit_canvas_to_parent: true,
                        resolution: WindowResolution::new(1280.0, 720.0)
                            .with_scale_factor_override(1.0),
                        ..default()
                    }),
                    ..default()
                })
                .set(AssetPlugin {
                    meta_check: AssetMetaCheck::Never,
                    watch_for_changes_override: Some(true),
                    ..default()
                }),
        )
        .add_plugins(EguiPlugin)
        .add_plugins(EntropyPlugin::<WyRand>::default())
        .add_plugins(TweeningPlugin)
        .add_plugins(MeshPickingPlugin)
        .add_plugins(bevy_stl::StlPlugin)
        .insert_resource(WinitSettings::game())
        .add_plugins(PanCamPlugin::default())
        .add_plugins(
            WorldInspectorPlugin::default().run_if(input_toggle_active(false, KeyCode::Escape)),
        )
        .add_plugins(bevy_arrows_plugin::BevyArrowsPlugin)
        .add_plugins(crate::ui::Ui)
        .add_plugins(GhostRobotPlugin)
        .add_plugins(SimulationClockPlugin)
        .add_plugins(SimAnimatorPlugin::<Transform>::default())
        .add_plugins(FlyingMessagePlugin);
    }
}
