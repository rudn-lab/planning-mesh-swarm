use bevy::{prelude::*, window::PrimaryWindow};
use bevy_egui::EguiContexts;

use crate::{
    bg_grid::{grid_pos_to_world, snap_world_to_grid},
    CELL_SIZE,
};

use super::{
    motion_types::{RobotOrientation, RobotState},
    ROBOT_WIDTH,
};

pub(crate) struct GhostRobotPlugin;

impl Plugin for GhostRobotPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, init_ghost_robot);
        app.add_systems(Update, update_ghost_robot);
    }
}

#[derive(Component)]
struct GhostRobot {
    orientation: RobotOrientation,
    grid_pos: (i32, i32),
}

#[derive(Bundle)]
struct GhostRobotBundle {
    ghost_robot: GhostRobot,
    location: Transform,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    name: Name,
}

impl GhostRobotBundle {
    fn new(grid_pos: (i32, i32), world: &mut World) -> Self {
        Self {
            ghost_robot: GhostRobot {
                orientation: RobotOrientation::Up,
                grid_pos,
            },
            mesh: Mesh2d(world.resource_mut::<Assets<Mesh>>().add(Triangle2d::new(
                Vec2::Y * ROBOT_WIDTH / 2.0,
                Vec2::new(-ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
                Vec2::new(ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
            ))),
            material: MeshMaterial2d(
                world
                    .resource_mut::<Assets<ColorMaterial>>()
                    .add(ColorMaterial::from(Color::srgba(0.0, 1.0, 1.0, 0.5))),
            ),
            location: Transform::from_translation(
                (Vec2::new(grid_pos.0 as f32, grid_pos.1 as f32) * CELL_SIZE).extend(0.0),
            ),
            name: Name::new("Ghost Robot"),
        }
    }
}

fn init_ghost_robot(world: &mut World) {
    let bundle = GhostRobotBundle::new((0, 0), world);
    world.commands().spawn(bundle);
}

fn update_ghost_robot(
    all_robots: Query<&RobotState>,
    camera: Query<(&Camera, &GlobalTransform), With<Camera2d>>,
    window: Query<&Window, With<PrimaryWindow>>,
    rotate_button_clicked: Res<ButtonInput<KeyCode>>,
    mut ghost_robot: Query<(&mut GhostRobot, &mut Transform)>,
) {
    let Ok((mut ghost_robot, mut transform)) = ghost_robot.get_single_mut() else {
        return;
    };

    if rotate_button_clicked.just_pressed(KeyCode::KeyR) {
        ghost_robot.orientation = ghost_robot.orientation.plus_quarter_turns(1);
    }

    let (camera, camera_transform) = camera.single();
    let window = window.single();
    let cursor_pos = window.cursor_position().map(|cursor_pos| {
        camera
            .viewport_to_world_2d(camera_transform, cursor_pos)
            .expect("Failed to convert cursor position to world space")
    });

    if let Some(cursor_pos) = cursor_pos {
        ghost_robot.grid_pos = snap_world_to_grid(cursor_pos);
    }

    transform.translation = grid_pos_to_world(ghost_robot.grid_pos).extend(0.0);
    transform.rotation = ghost_robot.orientation.to_quaternion();
}
