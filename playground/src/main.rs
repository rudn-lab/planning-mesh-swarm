pub(crate) mod animator;
pub(crate) mod bg_grid;
pub(crate) mod bg_grid_mat;
pub(crate) mod clock;
pub(crate) mod fps_monitor;
mod pause_controller;
mod plugins;
pub(crate) mod radio;
pub(crate) mod robot;
pub(crate) mod ui;

use core::f32;

use bevy::{prelude::*, winit::WinitSettings};
use bevy_pancam::{DirectionKeys, PanCam};
use bg_grid::Grid;
use fps_monitor::FpsMonitorPlugin;
use pause_controller::PausePlugin;
use plugins::MyPlugins;
use radio::{
    nic_components::{AntennaRenderingPlugin, NicBundle},
    RadioPlugin,
};
use robot::{onclick_handling::on_click_robot, RobotBehaviorPlugin, RobotBundle};

/// The number of game units in 1 millimeter
pub(crate) const MILLIMETER: f32 = 10.0;

/// The number of game units in 1 centimeter
pub(crate) const CENTIMETER: f32 = MILLIMETER * 10.0;

// The width/height of a single grid cell, in game units
pub(crate) const CELL_SIZE: f32 = CENTIMETER;

fn main() {
    App::new()
        .insert_resource(WinitSettings::desktop_app())
        .add_plugins(MyPlugins)
        .add_systems(Startup, setup_system)
        .add_plugins({
            let grid_size = 100f32 * CENTIMETER;
            Grid {
                mesh_size: Vec2::new(grid_size, grid_size),
                num_cells: Vec2::new(grid_size / CELL_SIZE, grid_size / CELL_SIZE),
                line_thickness_min: 0.75,
                line_thickness_max: 2.0,
                spotlight_radius: 500.0,
                line_color: Color::BLACK,
                bg_color: Color::NONE,
            }
        })
        .add_plugins(RobotBehaviorPlugin)
        .add_plugins(FpsMonitorPlugin)
        .add_plugins(PausePlugin)
        .add_plugins(RadioPlugin)
        .run();
}

fn setup_system(world: &mut World) {
    log::info!("Preparing shapes");

    //commands.spawn(Camera2d);
    world.commands().spawn(PanCam {
        grab_buttons: vec![MouseButton::Right, MouseButton::Middle, MouseButton::Left],
        move_keys: DirectionKeys::arrows_and_wasd(),
        speed: 200.0,
        enabled: true,
        zoom_to_cursor: true,
        min_scale: 1.0,
        max_scale: 10.0,
        ..Default::default()
    });

    let nic = NicBundle::new(world, Srgba::GREEN, 0);
    let robot_nic_child = world.commands().spawn(nic).id();

    let robot = RobotBundle::new(0, (3, 2), world, 1.0, 0.5);
    world.commands().spawn(robot).add_child(robot_nic_child);

    let robot = RobotBundle::new(1, (-3, 2), world, 0.5, 1.0);
    world.commands().spawn(robot);

    world.add_observer(on_click_robot);
}
