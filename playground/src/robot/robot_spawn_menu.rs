use bevy::{ecs::system::SystemId, prelude::*, window::PrimaryWindow};
use bevy_egui::EguiContexts;

use crate::{
    bg_grid::{grid_pos_to_world, snap_world_to_grid},
    CELL_SIZE,
};

use super::{
    motion_types::{RobotOrientation, RobotState},
    RobotBundle, ROBOT_WIDTH,
};

pub(crate) struct GhostRobotPlugin;

impl Plugin for GhostRobotPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, init_ghost_robot);
        app.add_systems(Update, update_ghost_robot);
        app.add_systems(Update, ghost_robot_config_menu);
        app.add_systems(Update, materialize_ghost_robot);
        app.add_event::<MaterializeGhostRobot>();
        app.add_event::<InitGhostRobot>();
    }
}

/// This event is sent when the ghost robot needs to be transformed into a real robot.
/// Any props have already been sent by this point, so the event needs no data.
///
/// If there is no ghost robot, then this event is ignored.
#[derive(Event, Default, Clone, Copy)]
struct MaterializeGhostRobot;

/// This event is sent when we need to add a ghost robot to the world.
/// If there is already a ghost robot, then this event is ignored.
#[derive(Event, Default, Clone, Copy)]
pub(crate) struct InitGhostRobot;

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

fn init_ghost_robot(
    existing_ghost_robot: Query<Entity, With<GhostRobot>>,
    mut events: EventReader<InitGhostRobot>,
    mut commands: Commands,
    mut exec_system: Local<Option<SystemId>>,
) {
    if events.read().next().is_some() {
        log::info!("Received InitGhostRobot event");
        // Check whether there is already a ghost robot;
        // if so, ignore this event
        if existing_ghost_robot.iter().next().is_some() {
            log::info!("There is already a ghost robot, ignoring InitGhostRobot event");
            return;
        }

        // There is not already a ghost robot, so spawn one
        if exec_system.is_none() {
            *exec_system = Some(commands.register_system(perform_init_ghost_robot));
        }
        commands.run_system(exec_system.unwrap());
    }
}

fn perform_init_ghost_robot(world: &mut World) {
    let bundle = GhostRobotBundle::new((0, 0), world);
    world.commands().spawn(bundle);
}

fn ghost_robot_config_menu(
    mut contexts: EguiContexts,
    mut ghost_robot: Query<(Entity, &mut GhostRobot, &mut Transform)>,
    mut commands: Commands,
) {
    let Ok((entity, ghost_robot, _transform)) = ghost_robot.get_single_mut() else {
        return;
    };

    let mut stay_open = true;
    egui::Window::new("New Robot Config")
        .open(&mut stay_open)
        .show(contexts.ctx_mut(), |ui| {
            ui.horizontal(|ui| {
                ui.label("Orientation:");
                ui.label(format!("{:?}", ghost_robot.orientation))
            });
            ui.horizontal(|ui| {
                ui.label("Grid Position:");
                ui.label(format!("{:?}", ghost_robot.grid_pos))
            });

            ui.separator();

            ui.small("Esc or close to cancel, Enter to confirm");
            ui.small("R to rotate");
        })
        .unwrap();

    if !stay_open {
        commands.entity(entity).despawn_recursive();
    }
}

fn materialize_ghost_robot(
    mut commands: Commands,
    ghost_robot: Query<Entity, With<GhostRobot>>,
    mut ev_reader: EventReader<MaterializeGhostRobot>,
    mut inner_system_id: Local<Option<SystemId>>,
) {
    let Some(_) = ghost_robot.iter().next() else {
        return;
    };

    let Some(_event) = ev_reader.read().next() else {
        return;
    };

    if inner_system_id.is_none() {
        *inner_system_id = Some(commands.register_system(materialize_ghost_robot_inner));
    }
    commands.run_system(inner_system_id.unwrap());
}

fn materialize_ghost_robot_inner(world: &mut World) {
    let mut all_robots = world.query::<&RobotState>();

    let max_id = all_robots
        .iter(world)
        .map(|robot| robot.id)
        .max()
        .unwrap_or(1);

    let mut query = world.query::<(Entity, &mut GhostRobot)>();
    let (entity, ghost_robot) = query.single(world);

    let bundle = RobotBundle::new(max_id + 1, ghost_robot.grid_pos, world, 0.1, 0.2);

    let commands = &mut world.commands();
    commands.spawn(bundle);
    commands.entity(entity).despawn_recursive();
}

fn update_ghost_robot(
    mut commands: Commands,
    camera: Query<(&Camera, &GlobalTransform), With<Camera2d>>,
    window: Query<&Window, With<PrimaryWindow>>,
    keyboard: Res<ButtonInput<KeyCode>>,
    mut ghost_robot: Query<(Entity, &mut GhostRobot, &mut Transform)>,
    mut ev_writer: EventWriter<MaterializeGhostRobot>,
) {
    let Ok((ghost_entity, mut ghost_robot, mut transform)) = ghost_robot.get_single_mut() else {
        return;
    };

    if keyboard.just_pressed(KeyCode::Escape) {
        // Cancel creating the robot by despawning the ghost robot
        commands.entity(ghost_entity).despawn_recursive();
    }

    if keyboard.just_pressed(KeyCode::KeyR) {
        ghost_robot.orientation = ghost_robot.orientation.plus_quarter_turns(1);
    }

    if keyboard.just_pressed(KeyCode::Enter) {
        ev_writer.send_default();
        return;
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
