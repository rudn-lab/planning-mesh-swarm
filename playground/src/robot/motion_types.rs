use async_channel::{Receiver, Sender};
use bevy::{prelude::*, tasks::Task};
use motion_high_level::MotionCommand;

use crate::CELL_SIZE;

use super::virtual_chassis::VirtualChassisCommand;

#[derive(Copy, Clone, Debug)]
pub enum RobotOrientation {
    Up,
    Down,
    Left,
    Right,
}

impl RobotOrientation {
    pub fn to_vector(&self) -> Vec2 {
        match self {
            RobotOrientation::Up => Vec2::Y,
            RobotOrientation::Down => -Vec2::Y,
            RobotOrientation::Left => -Vec2::X,
            RobotOrientation::Right => Vec2::X,
        }
    }

    pub fn to_radians(&self) -> f32 {
        match self {
            RobotOrientation::Up => 0.0,
            RobotOrientation::Down => std::f32::consts::PI,
            RobotOrientation::Left => std::f32::consts::FRAC_PI_2,
            RobotOrientation::Right => -std::f32::consts::FRAC_PI_2,
        }
    }

    pub fn one_right(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Left,
            RobotOrientation::Left => RobotOrientation::Up,
        }
    }
    pub fn one_left(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Left,
            RobotOrientation::Left => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Up,
        }
    }
    pub fn opposite(&self) -> Self {
        match self {
            RobotOrientation::Up => RobotOrientation::Down,
            RobotOrientation::Down => RobotOrientation::Up,
            RobotOrientation::Left => RobotOrientation::Right,
            RobotOrientation::Right => RobotOrientation::Left,
        }
    }

    pub fn plus_quarter_turns(&self, count: i32) -> Self {
        let count = count.rem_euclid(4);
        match count {
            0 => *self,
            1 => self.one_right(),
            2 => self.opposite(),
            3 => self.one_left(),
            _ => unreachable!(),
        }
    }

    pub fn to_numeric_vector(&self, n: i32) -> (i32, i32) {
        match self {
            RobotOrientation::Up => (0, n),
            RobotOrientation::Down => (0, -n),
            RobotOrientation::Left => (-n, 0),
            RobotOrientation::Right => (n, 0),
        }
    }
}

#[derive(Component)]
pub struct IdleRobot;

#[derive(Component)]
pub struct BusyRobot {
    pub command: MotionCommand,
}

#[derive(Component, Debug)]
pub struct RobotState {
    pub id: u64,
    pub grid_pos: (i32, i32),
    pub orientation: RobotOrientation,

    pub from_chassis: Receiver<VirtualChassisCommand>,
    pub into_chassis: Sender<()>,
    pub task: Task<()>,

    /// Number of seconds per single tile
    pub drive_rate: f32,

    /// Number of seconds per quarter turn
    pub turn_rate: f32,

    /// Log messages
    pub log: Vec<String>,
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
