use bevy::{prelude::*, tasks::IoTaskPool};
use bevy_tweening::{Animator, TweenCompleted};
use motion_anim::get_tween;
use motion_types::{BusyRobot, IdleRobot, RobotOrientation, RobotState};
use virtual_chassis::VirtualChassis;

use crate::CELL_SIZE;

mod async_queue_wrapper;
mod business_logic;
mod motion_anim;
mod motion_types;
pub mod virtual_chassis;

#[derive(Bundle)]
pub struct RobotBundle {
    name: Name,
    location: Transform,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    robot_state: RobotState,
    idle_robot: IdleRobot,
}

pub const ROBOT_WIDTH: f32 = 0.9 * (CELL_SIZE);

impl RobotBundle {
    pub fn new(
        id: u64,
        grid_pos: (i32, i32),
        meshes: &mut Assets<Mesh>,
        materials: &mut Assets<ColorMaterial>,
        drive_rate: f32,
        turn_rate: f32,
    ) -> Self {
        let (from_robot_sender, from_robot_receiver) = async_channel::unbounded();
        let (to_robot_sender, to_robot_receiver) = async_channel::unbounded();

        let chassis = VirtualChassis {
            tx: from_robot_sender,
            rx: to_robot_receiver,
        };

        let my_logic = IoTaskPool::get().spawn(business_logic::simple_business_logic(chassis));

        Self {
            name: Name::new(format!("Robot {}", id)),
            location: Transform::from_translation(
                (Vec2::new(grid_pos.0 as f32, grid_pos.1 as f32) * CELL_SIZE).extend(0.0),
            ),
            mesh: Mesh2d(meshes.add(Triangle2d::new(
                Vec2::Y * ROBOT_WIDTH / 2.0,
                Vec2::new(-ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
                Vec2::new(ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
            ))),
            material: MeshMaterial2d(materials.add(ColorMaterial::from(Color::hsl(0.0, 1.0, 1.0)))),
            robot_state: RobotState {
                id,
                grid_pos,
                orientation: RobotOrientation::Up,

                from_chassis: from_robot_receiver,
                into_chassis: to_robot_sender,
                task: my_logic,

                drive_rate,
                turn_rate,
            },
            idle_robot: IdleRobot,
        }
    }
}

fn setup_system(mut commands: Commands) {}

fn update_idle_robots(
    mut robot_query: Query<(Entity, &mut RobotState), With<IdleRobot>>,
    mut commands: Commands,
) {
    // For every robot that is idle, check whether they have sent us any commands
    // If they have, move them to the busy state
    // and start the animation
    for (entity, robot_state) in robot_query.iter_mut() {
        if let Ok(message) = robot_state.from_chassis.try_recv() {
            println!("Received message: {message:?}");
            commands.entity(entity).remove::<IdleRobot>();
            commands
                .entity(entity)
                .insert(BusyRobot { command: message });

            let tween = get_tween(message, &robot_state).with_completed_event(robot_state.id);
            commands.entity(entity).insert(Animator::new(tween));
        }
    }
}

fn update_busy_robots(
    mut robot_query: Query<(
        Entity,
        &mut Transform,
        &mut RobotState,
        &BusyRobot,
        &Animator<Transform>,
    )>,
    mut commands: Commands,
    mut event_reader: EventReader<TweenCompleted>,
) {
    let mut robot_ids_to_update = vec![];

    for event in event_reader.read() {
        robot_ids_to_update.push(event.user_data);
    }

    if robot_ids_to_update.is_empty() {
        return;
    }

    for (entity, mut transform, mut robot_state, busy_robot, _animator) in robot_query.iter_mut() {
        if robot_ids_to_update.contains(&robot_state.id) {
            // The animation has completed, so the robot is now in its new position
            // Finalize the animation by moving the robot to its final position
            robot_state.update(busy_robot.command);
            transform.translation = robot_state.get_translation();
            transform.rotation = robot_state.get_rotation();

            // Remove the old components, mark the robot as idle
            // and send a message to the runner
            commands.entity(entity).remove::<Animator<Transform>>();
            commands.entity(entity).remove::<BusyRobot>();
            commands.entity(entity).insert(IdleRobot);

            println!("Finished executing robot command: {:?}", busy_robot.command);

            robot_state.into_chassis.try_send(()).unwrap();
        }
    }
}

pub struct RobotBehaviorPlugin;

impl Plugin for RobotBehaviorPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_system);
        app.add_systems(FixedUpdate, update_idle_robots);
        app.add_systems(FixedUpdate, update_busy_robots);
    }
}