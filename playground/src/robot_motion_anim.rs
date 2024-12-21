use std::time::Duration;

use bevy::prelude::{EaseFunction, Transform};
use bevy_tweening::{
    lens::{TransformPositionLens, TransformRotateZLens},
    Tween,
};
use motion_high_level::MotionCommand;

use crate::{robot::RobotState, CELL_SIZE};

pub fn get_tween(cmd: MotionCommand, state: &RobotState) -> Tween<Transform> {
    match cmd {
        MotionCommand::Forward(n) => get_forward_back_tween(n.get() as i32, state),
        MotionCommand::Backward(n) => get_forward_back_tween(-(n.get() as i32), state),
        MotionCommand::TurnLeft(n) => get_turn_tween(-(n.get() as i32), state),
        MotionCommand::TurnRight(n) => get_turn_tween(n.get() as i32, state),
    }
}

/// The tween representing a movement forward (or back, if negative).
fn get_forward_back_tween(cells_moved_forward: i32, state: &RobotState) -> Tween<Transform> {
    // how many seconds per tile
    let move_rate = 1.0;
    let forward_back_tween = Tween::new(
        EaseFunction::QuadraticInOut,
        Duration::from_secs_f32(cells_moved_forward as f32 * move_rate),
        TransformPositionLens {
            start: state.get_translation(),
            end: state.get_translation()
                + state.orientation.to_vector().extend(0.0)
                    * cells_moved_forward as f32
                    * CELL_SIZE,
        },
    );

    forward_back_tween
}

/// The tween representing a rotation clockwise (or counterclockwise, if negative).
fn get_turn_tween(turns_cw: i32, state: &RobotState) -> Tween<Transform> {
    // how many seconds per quarter turn
    let turn_rate = 0.5;
    let turn_tween = Tween::new(
        EaseFunction::QuadraticInOut,
        Duration::from_secs_f32(turns_cw as f32 * turn_rate),
        TransformRotateZLens {
            start: state.orientation.to_radians(),
            end: state.orientation.to_radians() - turns_cw as f32 * std::f32::consts::FRAC_PI_2,
        },
    );

    turn_tween
}
