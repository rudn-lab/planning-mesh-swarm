use std::{num::NonZero, time::Duration};

use async_channel::{Receiver, Sender};
use bevy::{
    prelude::*,
    tasks::{IoTaskPool, Task},
    utils::HashMap,
};
use bevy_tweening::{lens::TransformPositionLens, Animator, AnimatorState, Tween, TweenCompleted};
use motion_high_level::MotionCommand;

use crate::{
    robot_motion_anim::get_tween,
    robot_motion_types::{BusyRobot, IdleRobot, RobotOrientation},
    CELL_SIZE, CENTIMETER,
};

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
    ) -> Self {
        Self {
            name: Name::new(format!("Robot {}", id)),
            location: Transform::from_translation(
                (Vec2::new(grid_pos.0 as f32, grid_pos.1 as f32) * CELL_SIZE).extend(0.0),
            ),
            mesh: Mesh2d(meshes.add(Triangle2d::new(
                Vec2::Y * ROBOT_WIDTH / 2.0,
                Vec2::new(-ROBOT_WIDTH / 2.0, -ROBOT_WIDTH / 2.0),
                Vec2::new(ROBOT_WIDTH / 2.0, -ROBOT_WIDTH / 2.0),
            ))),
            material: MeshMaterial2d(materials.add(ColorMaterial::from(Color::hsl(0.0, 1.0, 1.0)))),
            robot_state: RobotState {
                id,
                grid_pos,
                orientation: RobotOrientation::Up,
            },
            idle_robot: IdleRobot,
        }
    }
}

#[derive(Component)]
pub struct RobotState {
    pub id: u64,
    pub grid_pos: (i32, i32),
    pub orientation: RobotOrientation,
}

impl RobotState {
    pub fn get_translation(&self) -> Vec3 {
        Vec3::new(
            self.grid_pos.0 as f32 * CELL_SIZE,
            self.grid_pos.1 as f32 * CELL_SIZE,
            0.0,
        )
    }

    pub fn get_rotation(&self) -> Quat {
        match self.orientation {
            RobotOrientation::Up => Quat::IDENTITY,
            RobotOrientation::Down => Quat::from_rotation_z(std::f32::consts::PI),
            RobotOrientation::Left => Quat::from_rotation_z(std::f32::consts::FRAC_PI_2),
            RobotOrientation::Right => Quat::from_rotation_z(-std::f32::consts::FRAC_PI_2),
        }
    }

    pub fn update(&mut self, command: MotionCommand) {
        match command {
            MotionCommand::Forward(n) => {
                let n = n.get() as i32;
                self.grid_pos.0 += self.orientation.to_numeric_vector(n).0;
                self.grid_pos.1 += self.orientation.to_numeric_vector(n).1;
            }
            MotionCommand::Backward(n) => {
                let n = n.get() as i32;
                self.grid_pos.0 += self.orientation.to_numeric_vector(n).0;
                self.grid_pos.1 += self.orientation.to_numeric_vector(n).1;
            }
            MotionCommand::TurnLeft(n) => {
                self.orientation = self.orientation.plus_quarter_turns(-(n.get() as i32));
            }
            MotionCommand::TurnRight(n) => {
                self.orientation = self.orientation.plus_quarter_turns(n.get() as i32);
            }
        }
    }
}

#[derive(Resource)]
pub struct RobotTaskHandle {
    pub receiver: Receiver<WithRobotId<MotionCommand>>,
    pub sender: Sender<WithRobotId<()>>,
    pub handle: Task<()>,
}

#[derive(Debug)]
pub struct WithRobotId<T> {
    pub robot_id: u64,
    pub value: T,
}

fn setup_system(mut commands: Commands) {
    let (send_tx, send_rx) = async_channel::unbounded();
    let (recv_tx, recv_rx) = async_channel::unbounded();
    let handle = IoTaskPool::get().spawn(robot_async_reactor(send_rx, recv_tx));

    commands.insert_resource(RobotTaskHandle {
        receiver: recv_rx,
        sender: send_tx,
        handle,
    });
}

async fn robot_async_reactor(
    mut from_environment: Receiver<WithRobotId<()>>,
    mut into_environment: Sender<WithRobotId<MotionCommand>>,
) {
    // TODO: just one robot doing a simple loop for now
    let robot_id = 0;
    loop {
        use MotionCommand as M;
        let cmds = [
            M::Forward(NonZero::new(1).unwrap()),
            M::TurnRight(NonZero::new(1).unwrap()),
        ];

        for cmd in cmds {
            into_environment
                .send(WithRobotId {
                    robot_id,
                    value: cmd,
                })
                .await
                .unwrap();
            from_environment.recv().await.unwrap();

            // This delay is necessary so that the new command does not arrive during the same frame
            // as the previous command.
            // If this happens, then there is a race condition:
            // first, update_busy_robots will wake the robot up
            // and add a command to delete BusyRobot and add IdleRobot to the entity.
            // Then, on the same frame (before the command is processed),
            // update_idle_robots will run, receive the event referencing robot 1234,
            // and try to find robot 1234 from the query,
            // but the query only includes robots with IdleRobot, while robot 1234 (still) has BusyRobot,
            // so it will not find robot 1234 and crash.
            async_std::task::sleep(Duration::from_secs(1)).await;
        }
    }
}

fn update_idle_robots(
    mut robot_query: Query<(Entity, &mut RobotState), With<IdleRobot>>,
    handle: ResMut<RobotTaskHandle>,
    mut commands: Commands,
) {
    if handle.handle.is_finished() {
        panic!("Robot task finished, but was supposed to run forever");
    }
    loop {
        let new_event = match handle.receiver.try_recv() {
            Ok(event) => event,
            Err(async_channel::TryRecvError::Empty) => return,
            Err(async_channel::TryRecvError::Closed) => {
                panic!("Robot task closed the channel, but was supposed to run forever");
            }
        };

        println!("Got command from robot: {:#?}", new_event);

        let (entity, robot_state) = robot_query
            .iter_mut()
            .find(|(_, s)| s.id == new_event.robot_id)
            .unwrap();

        commands.entity(entity).remove::<IdleRobot>();
        commands.entity(entity).insert(BusyRobot {
            command: new_event.value,
        });

        let tween = get_tween(new_event.value, &robot_state).with_completed_event(robot_state.id);
        commands.entity(entity).insert(Animator::new(tween));
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
    handle: ResMut<RobotTaskHandle>,
    mut commands: Commands,
    mut event_reader: EventReader<TweenCompleted>,
) {
    if handle.handle.is_finished() {
        panic!("Robot task finished, but was supposed to run forever");
    }

    let mut robot_ids_to_update = vec![];

    for event in event_reader.read() {
        robot_ids_to_update.push(event.user_data);
    }

    if robot_ids_to_update.is_empty() {
        return;
    }

    for (entity, mut transform, mut robot_state, busy_robot, animator) in robot_query.iter_mut() {
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

            println!(
                "Finished executing robot command: {:#?}",
                busy_robot.command
            );

            handle
                .sender
                .try_send(WithRobotId {
                    robot_id: robot_state.id,
                    value: (),
                })
                .unwrap();
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
