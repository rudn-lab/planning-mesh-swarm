use async_channel::Receiver;
use bevy::{prelude::*, tasks::Task};
use high_level_cmds::MotionCommand;

use crate::{
    bg_grid::grid_pos_to_world,
    clock::SimulationInstant,
    radio::virtual_nic::{MessageType, VirtualPeerId, VirtualRadioRequest},
};

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

    pub(crate) fn to_quaternion(&self) -> Quat {
        match self {
            RobotOrientation::Up => Quat::IDENTITY,
            RobotOrientation::Left => Quat::from_rotation_z(std::f32::consts::FRAC_PI_2),
            RobotOrientation::Down => Quat::from_rotation_z(std::f32::consts::PI),
            RobotOrientation::Right => Quat::from_rotation_z(-std::f32::consts::FRAC_PI_2),
        }
    }
}

/// Component that marks robots that are not currently moving.
#[derive(Component)]
pub(crate) struct IdleRobot;

/// Component that marks robots that are currently waiting on a sleep to elapse.
#[derive(Component)]
pub(crate) struct SleepingRobot {
    pub(crate) duration: core::time::Duration,
    pub(crate) started_at: SimulationInstant,
    pub(crate) on_complete: Option<oneshot::Sender<()>>,
}

/// Component that marks robots that are currently performing an animation in response to a motion command.
#[derive(Component)]
pub(crate) struct BusyRobot {
    pub(crate) command: MotionCommand,
    pub(crate) on_complete: Option<oneshot::Sender<()>>,
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

    pub(crate) from_radio: Receiver<VirtualRadioRequest>,

    /// Task that is running the robot's business logic.
    /// We don't need to touch it directly, but we need to prevent dropping it.
    #[allow(dead_code)]
    pub(crate) business_task: Task<()>,

    /// Task that is running the robot's radio logic.
    /// As above, it just needs to be kept alive.
    #[allow(dead_code)]
    pub(crate) radio_task: Task<()>,

    /// Log messages
    pub(crate) log: Vec<(SimulationInstant, String)>,

    /// If the radio is sleeping, this represents when the sleep elapses.
    pub(crate) radio_sleep_state: Option<(SimulationInstant, oneshot::Sender<()>)>,

    /// Channels that are waiting for a message to arrive to this robot's receiver NIC.
    pub(crate) msg_receivers: Vec<oneshot::Sender<(VirtualPeerId, MessageType)>>,

    /// Messages that have been received by the receiver NIC,
    /// but not yet seen by the radio logic.
    pub(crate) queued_messages: Vec<(VirtualPeerId, MessageType)>,

    /// What color is the robot displaying?
    /// Available as an RGB triple.
    pub(crate) led_color: [u8; 3],
}

impl RobotState {
    pub(crate) fn get_translation(&self) -> Vec3 {
        grid_pos_to_world(self.grid_pos).extend(0.0)
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
