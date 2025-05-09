use bevy::{prelude::*, tasks::IoTaskPool};
use bevy_tweening::TweenCompleted;
use internal_state_vis::InternalStatePlugin;
use motion_anim::{get_tween, update_robot_tweens_after_props_change};
use motion_types::{BusyRobot, IdleRobot, RobotOrientation, RobotProps, RobotState, SleepingRobot};
use onclick_handling::{on_selection_event, SelectedRobot, SelectionChanged};
use radio_world_communication::update_robot_radios;
use virtual_chassis::VirtualChassis;

use crate::{
    animator::SimAnimator,
    clock::{Simulation, SimulationTime},
    pause_controller::PauseState,
    radio::virtual_nic::new_virtual_network_kit,
    CELL_SIZE,
};

mod async_queue_wrapper;
mod business_logic;
mod internal_state_vis;
pub(crate) mod motion_anim;
pub(crate) mod motion_types;
pub(crate) mod onclick_handling;
mod radio_world_communication;
pub(crate) mod robot_spawn_menu;
mod selection_reticle;
pub(crate) mod virtual_chassis;

#[derive(Bundle)]
pub(crate) struct RobotBundle {
    name: Name,
    location: Transform,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    robot_state: RobotState,
    robot_props: RobotProps,
    idle_robot: IdleRobot,
}

pub(crate) const ROBOT_WIDTH: f32 = 0.9 * (CELL_SIZE);

impl RobotBundle {
    pub(crate) fn new(
        id: u64,
        grid_pos: (i32, i32),
        world: &mut World,
        drive_speed: f32,
        turn_speed: f32,
    ) -> Self {
        let (from_robot_sender, from_robot_receiver) = async_channel::unbounded();

        let chassis = VirtualChassis {
            tx: from_robot_sender,
        };

        let my_logic = IoTaskPool::get().spawn(business_logic::simple_business_logic(chassis));

        let (tx, from_radio) = async_channel::bounded(100);
        const MAX_SENDER_COUNT: usize = 4;
        let my_radio = IoTaskPool::get().spawn(routing::sample_routing(new_virtual_network_kit::<
            MAX_SENDER_COUNT,
        >(tx)));

        Self {
            name: Name::new(format!("Robot {}", id)),
            location: Transform::from_translation(
                (Vec2::new(grid_pos.0 as f32, grid_pos.1 as f32) * CELL_SIZE).extend(0.0),
            ),
            mesh: Mesh2d(world.resource_mut::<Assets<Mesh>>().add(Triangle2d::new(
                Vec2::Y * ROBOT_WIDTH / 2.0,
                Vec2::new(-ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
                Vec2::new(ROBOT_WIDTH / 4.0, -ROBOT_WIDTH / 2.0),
            ))),
            material: MeshMaterial2d(
                world
                    .resource_mut::<Assets<ColorMaterial>>()
                    .add(ColorMaterial::from(Color::hsl(0.0, 1.0, 1.0))),
            ),

            robot_state: RobotState {
                id,
                grid_pos,
                orientation: RobotOrientation::Up,

                from_chassis: from_robot_receiver,
                business_task: my_logic,

                from_radio,
                radio_task: my_radio,
                msg_receivers: vec![],
                queued_messages: vec![],

                log: vec![],
                radio_sleep_state: None,
                led_color: [255, 255, 255],
            },
            robot_props: RobotProps {
                drive_speed,
                turn_speed,
            },
            idle_robot: IdleRobot,
        }
    }
}

fn update_robot_colors(
    robot_query: Query<(&RobotState, &MeshMaterial2d<ColorMaterial>)>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    // For every robot, set its associated color material
    // to match the robot state.
    for (state, material) in robot_query.iter() {
        if let Some(mat) = materials.get_mut(material.id()) {
            let [r, g, b] = state.led_color;
            let r = r as f32 / 255.0;
            let g = g as f32 / 255.0;
            let b = b as f32 / 255.0;
            let intensity = 7.5;
            let r = r * intensity;
            let g = g * intensity;
            let b = b * intensity;
            mat.color = Srgba::new(r, g, b, 1.0).into();
        }
    }
}

fn update_idle_robots(
    mut robot_query: Query<(Entity, &RobotProps, &mut RobotState), With<IdleRobot>>,
    mut commands: Commands,
    pause_state: Res<PauseState>,
    time: Res<Time<Simulation>>,
) {
    // If we are paused, do nothing
    // This will keep the robots from beginning to move,
    // but not from thinking;
    // however, if any command is sent (including logging),
    // then the robot will be blocked until it's unpaused.
    if pause_state.paused {
        return;
    }

    // For every robot that is idle, check whether they have sent us any commands
    // If they have sent motion commands, move them to the busy state
    // and start the animation
    for (entity, props, mut robot_state) in robot_query.iter_mut() {
        if let Ok(message) = robot_state.from_chassis.try_recv() {
            match message {
                virtual_chassis::VirtualChassisCommand::Motion(motion_command) => {
                    commands.entity(entity).remove::<IdleRobot>();
                    commands.entity(entity).insert(BusyRobot {
                        command: motion_command.0,
                        on_complete: Some(motion_command.1),
                    });

                    let tween = get_tween(motion_command.0, &props, &robot_state);
                    commands.entity(entity).insert(SimAnimator::new(tween));
                }
                virtual_chassis::VirtualChassisCommand::Log(message) => {
                    // Add the log entry to the robot state's log
                    robot_state.log.push((time.get_instant(), message.0));

                    // Immediately acknowledge the message
                    // (the acknowledgement could only be delayed if we had been paused when the message was sent)
                    message.1.send(()).unwrap();
                }
                virtual_chassis::VirtualChassisCommand::Sleep((duration, on_complete)) => {
                    commands.entity(entity).insert(SleepingRobot {
                        duration,
                        started_at: time.get_instant(),
                        on_complete: Some(on_complete),
                    });
                }
                virtual_chassis::VirtualChassisCommand::LedColorSet((color, tx)) => {
                    robot_state.led_color = color;
                    tx.send(()).expect("failed to send ack for LedColorSet");
                }
                virtual_chassis::VirtualChassisCommand::LedColorGet(sender) => {
                    sender
                        .send(robot_state.led_color)
                        .expect("failed to send result for LedColorGet");
                }
            }
        }
    }
}

fn update_busy_robots(
    mut robot_query: Query<(
        Entity,
        &mut Transform,
        &mut RobotState,
        &mut BusyRobot,
        &SimAnimator<Transform>,
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

    for (entity, mut transform, mut robot_state, mut busy_robot, _animator) in
        robot_query.iter_mut()
    {
        if robot_ids_to_update.contains(&robot_state.id) {
            // The animation has completed, so the robot is now in its new position
            // Finalize the animation by moving the robot to its final position
            robot_state.update(busy_robot.command);
            transform.translation = robot_state.get_translation();
            transform.rotation = robot_state.get_rotation();

            // Remove the old components, mark the robot as idle
            // and send a message to the runner
            commands.entity(entity).remove::<SimAnimator<Transform>>();
            commands.entity(entity).remove::<BusyRobot>();
            commands.entity(entity).insert(IdleRobot);

            // println!("Finished executing robot command: {:?}", busy_robot.command);

            busy_robot.on_complete.take().map(|v| v.send(()).unwrap());
        }
    }
}

fn update_sleeping_robots(
    mut robot_query: Query<(Entity, Mut<SleepingRobot>)>,
    mut commands: Commands,
    time: Res<Time<Simulation>>,
) {
    // For every robot that is sleeping,
    // check whether the sleep duration has elapsed
    for (entity, mut sleeping_robot) in robot_query.iter_mut() {
        if time.get_instant() - sleeping_robot.started_at > sleeping_robot.duration {
            commands.entity(entity).remove::<SleepingRobot>();
            sleeping_robot
                .on_complete
                .take()
                .map(|v| v.send(()).unwrap());
        }
    }
}

pub(crate) struct RobotBehaviorPlugin;

impl Plugin for RobotBehaviorPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, update_robot_colors);
        app.add_systems(FixedUpdate, update_idle_robots);
        app.add_systems(FixedUpdate, update_busy_robots);
        app.add_systems(FixedUpdate, update_sleeping_robots);
        app.add_systems(FixedUpdate, update_robot_radios);
        app.add_systems(FixedUpdate, on_selection_event);
        app.add_systems(FixedUpdate, update_robot_tweens_after_props_change);
        app.add_plugins(InternalStatePlugin);
        app.init_resource::<SelectedRobot>();
        app.add_event::<SelectionChanged>();
    }
}
