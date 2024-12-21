use bevy::prelude::*;
use motion_high_level::MotionCommand;

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
