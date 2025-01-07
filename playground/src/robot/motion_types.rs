use async_channel::{Receiver, Sender};
use bevy::{prelude::*, tasks::Task};
use high_level_cmds::MotionCommand;

use crate::CELL_SIZE;

use super::virtual_chassis::VirtualChassisCommand;

#[derive(Copy, Clone, Debug)]
pub(crate) enum RobotOrientation {
    Up,
    Down,
    Left,
    Right,
}

impl RobotOrientation {
    pub(crate) fn to_vector(&self) -> Vec2 {
        match self {
            RobotOrientation::Up => Vec2::Y,
            RobotOrientation::Down => -Vec2::Y,
            RobotOrientation::Left => -Vec2::X,
            RobotOrientation::Right => Vec2::X,
        }
    }

    pub(crate) fn to_radians(&self) -> f32 {
        match self {
            RobotOrientation::Up => 0.0,
            RobotOrientation::Down => std::f32::consts::PI,
            RobotOrientation::Left => std::f32::consts::FRAC_PI_2,
            RobotOrientation::Right => -std::f32::consts::FRAC_PI_2,
        }
    }

    pub(crate) fn one_right(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Left,
            RobotOrientation::Left => RobotOrientation::Up,
        }
    }
    pub(crate) fn one_left(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Left,
            RobotOrientation::Left => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Up,
        }
    }
    pub(crate) fn opposite(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Up,
            RobotOrientation::Left => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Left,
        }
    }

    pub(crate) fn plus_quarter_turns(&self, count: i32) -> Self {
        let count = count.rem_euclid(4);
        match count {
            0 => *self,
            1 => self.one_right(),
            2 => self.opposite(),
            3 => self.one_left(),
            _ => unreachable!(),
        }
    }

    pub(crate) fn to_numeric_vector(&self, n: i32) -> (i32, i32) {
        match self {
            RobotOrientation::Up => (0, n),
            RobotOrientation::Down => (0, -n),
            RobotOrientation::Left => (-n, 0),
            RobotOrientation::Right => (n, 0),
        }
    }
}

/// Component that marks robots that are not currently moving.
#[derive(Component)]
pub(crate) struct IdleRobot;

/// Component that marks robots that are currently performing an animation in response to a motion command.
#[derive(Component)]
pub(crate) struct BusyRobot {
    pub(crate) command: MotionCommand,
}

/// Values that are inherent to a robot's anatomy.
#[derive(Component, Debug)]
pub(crate) struct RobotProps {
    /// The speed of the robot when moving in a line; in centimeters per second.
    pub(crate) drive_speed: f32,

    /// The rotation speed of the robot; in radians per second.
    pub(crate) turn_speed: f32,
}

/// Values representing the robot's current position in the grid.
/// Updated after the motion's animation completes;
/// while the animation is running, the values represent its state at the start of the animation.
#[derive(Component, Debug)]
pub(crate) struct RobotState {
    pub(crate) id: u64,
    pub(crate) grid_pos: (i32, i32),
    pub(crate) orientation: RobotOrientation,

    pub(crate) from_chassis: Receiver<VirtualChassisCommand>,
    pub(crate) into_chassis: Sender<()>,

    /// Task that is running the robot's business logic.
    /// We don't need to touch it directly, but we need to prevent dropping it.
    #[allow(dead_code)]
    pub(crate) task: Task<()>,

    /// Log messages
    pub(crate) log: Vec<String>,
}

impl RobotState {
    pub(crate) fn get_translation(&self) -> Vec3 {
        Vec3::new(
            self.grid_pos.0 as f32 * CELL_SIZE,
            self.grid_pos.1 as f32 * CELL_SIZE,
            0.0,
        )
    }

    pub(crate) fn get_rotation(&self) -> Quat {
        match self.orientation {
            RobotOrientation::Up => Quat::IDENTITY,
            RobotOrientation::Down => Quat::from_rotation_z(std::f32::consts::PI),
            RobotOrientation::Left => Quat::from_rotation_z(std::f32::consts::FRAC_PI_2),
            RobotOrientation::Right => Quat::from_rotation_z(-std::f32::consts::FRAC_PI_2),
        }
    }

    pub(crate) fn update(&mut self, command: MotionCommand) {
        match command {
            MotionCommand::Forward(n) => {
                let n = n.get() as i32;
                self.grid_pos.0 += self.orientation.to_numeric_vector(n).0;
                self.grid_pos.1 += self.orientation.to_numeric_vector(n).1;
            }
            MotionCommand::Backward(n) => {
                let n = n.get() as i32;
                self.grid_pos.0 -= self.orientation.to_numeric_vector(n).0;
                self.grid_pos.1 -= self.orientation.to_numeric_vector(n).1;
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
