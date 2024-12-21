pub mod bg_grid;
pub mod bg_grid_mat;
pub mod fps_monitor;
pub mod robot;
pub mod ui;

use core::f32;

use bevy::{
    asset::AssetMetaCheck, input::common_conditions::input_toggle_active, prelude::*,
    window::WindowResolution, winit::WinitSettings,
};
use bevy_egui::EguiPlugin;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_pancam::{DirectionKeys, PanCam, PanCamPlugin};
use bg_grid::Grid;
use fps_monitor::FpsMonitorPlugin;
use robot::{RobotBehaviorPlugin, RobotBundle};
use ui::Ui;

/// The number of game units in 1 millimeter
pub const MILLIMETER: f32 = 10.0;

/// The number of game units in 1 centimeter
pub const CENTIMETER: f32 = MILLIMETER * 10.0;

// The width/height of a single grid cell, in game units
pub const CELL_SIZE: f32 = CENTIMETER;

fn main() {
    App::new()
        .insert_resource(WinitSettings::desktop_app())
        .add_plugins(
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
                    ..default()
                }),
        )
        .add_plugins(EguiPlugin)
        .insert_resource(WinitSettings::game())
        .add_plugins(Ui)
        .add_plugins(PanCamPlugin::default())
        .add_plugins(
            WorldInspectorPlugin::default().run_if(input_toggle_active(false, KeyCode::Escape)),
        )
        .add_systems(Startup, setup_system)
        .add_plugins({
            let grid_size = 100f32 * CENTIMETER;
            Grid {
                mesh_size: Vec2::new(grid_size, grid_size),
                num_cells: Vec2::new(grid_size / CELL_SIZE, grid_size / CELL_SIZE),
                line_thickness: 0.05f32,
                line_color: Color::BLACK,
                bg_color: Color::NONE,
            }
        })
        .add_plugins(RobotBehaviorPlugin)
        .add_plugins(FpsMonitorPlugin)
        .run();
}

fn setup_system(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    log::info!("Preparing shapes");
    const X_EXTENT: f32 = 900.;

    //commands.spawn(Camera2d);
    commands.spawn(PanCam {
        grab_buttons: vec![MouseButton::Right, MouseButton::Middle, MouseButton::Left],
        move_keys: DirectionKeys::arrows_and_wasd(),
        speed: 200.0,
        enabled: true,
        zoom_to_cursor: true,
        min_scale: 1.0,
        max_scale: 10.0,
        ..Default::default()
    });

    commands.spawn(RobotBundle::new(
        Vec2::new(3.0, 2.0),
        &mut meshes,
        &mut materials,
    ));

    let shapes = [
        meshes.add(Circle::new(5.0)),
        meshes.add(CircularSector::new(5.0, 1.0)),
        meshes.add(CircularSegment::new(5.0, 1.25)),
        meshes.add(Ellipse::new(2.5, 5.0)),
        meshes.add(Annulus::new(2.5, 5.0)),
        meshes.add(Capsule2d::new(2.5, 5.0)),
        meshes.add(Rhombus::new(7.5, 10.0)),
        meshes.add(Rectangle::new(5.0, 10.0)),
        meshes.add(RegularPolygon::new(5.0, 6)),
        meshes.add(Triangle2d::new(
            Vec2::Y * 5.0,
            Vec2::new(-5.0, -5.0),
            Vec2::new(5.0, -5.0),
        )),
    ];
    let num_shapes = shapes.len();

    for (i, shape) in shapes.into_iter().enumerate() {
        // Distribute colors evenly across the rainbow.
        let color = Color::hsl(360. * i as f32 / num_shapes as f32, 0.95, 0.7);

        commands.spawn((
            Mesh2d(shape),
            MeshMaterial2d(materials.add(color)),
            Transform::from_xyz(
                // Distribute shapes from -X_EXTENT/2 to +X_EXTENT/2.
                -X_EXTENT / 2. + i as f32 / (num_shapes - 1) as f32 * X_EXTENT,
                0.0,
                0.0,
            ),
        ));
    }
}
