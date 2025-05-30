use std::time::Duration;

use bevy::prelude::*;
use bevy_tweening::{
    lens::{TransformPositionLens, TransformRotateZLens},
    Tween, Tweenable,
};
use high_level_cmds::MotionCommand;

use crate::{animator::SimAnimator, robot::RobotState, CELL_SIZE};

use super::motion_types::{BusyRobot, RobotProps};

pub(crate) fn get_tween(
    cmd: MotionCommand,
    props: &RobotProps,
    state: &RobotState,
) -> Tween<Transform> {
    match cmd {
        MotionCommand::Forward(n) => get_forward_back_tween(n.get() as i32, props, state),
        MotionCommand::Backward(n) => get_forward_back_tween(-(n.get() as i32), props, state),
        MotionCommand::TurnLeft(n) => get_turn_tween(-(n.get() as i32), props, state),
        MotionCommand::TurnRight(n) => get_turn_tween(n.get() as i32, props, state),
    }
    // When the tween is complete, send an event to the robot to wake up.
    .with_completed_event(state.id.0)
}

/// The shortest possible duration for a tween. Should be almost instant.
/// Used for rounding up shorter durations (like zero: this causes issues for the animation).
const MIN_DURATION: Duration = Duration::from_millis(1);

/// The tween representing a movement forward (or back, if negative).
fn get_forward_back_tween(
    cells_moved_forward: i32,
    props: &RobotProps,
    state: &RobotState,
) -> Tween<Transform> {
    let distance = cells_moved_forward.abs() as f32 * CELL_SIZE;
    let time = Duration::from_secs_f32(distance / props.drive_speed);

    let forward_back_tween = Tween::new(
        EaseFunction::QuadraticInOut,
        time.max(MIN_DURATION),
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
fn get_turn_tween(turns_cw: i32, props: &RobotProps, state: &RobotState) -> Tween<Transform> {
    let radians_moved = turns_cw.abs() as f32 * std::f32::consts::FRAC_PI_2;
    let time = Duration::from_secs_f32(radians_moved / props.turn_speed);

    let turn_tween = Tween::new(
        EaseFunction::QuadraticInOut,
        time.max(MIN_DURATION),
        TransformRotateZLens {
            start: state.orientation.to_radians(),
            end: state.orientation.to_radians() - turns_cw as f32 * std::f32::consts::FRAC_PI_2,
        },
    );

    turn_tween
}

/// This system checks for robots whose config has changed.
/// If it has, the robot's in-progress tween is updated.
///
/// NOTE: currently the system runs once every frame when the robot's config popup is opened,
/// because rendering the popup's speed sliders causes a mutable access,
/// even if the robot's config has not changed.
/// Only a single robot may be selected at a time,
/// and updating the tween every frame like this does not cause any discontinuities.
/// However, this is a potential candidate for optimization.
pub(crate) fn update_robot_tweens_after_props_change(
    mut changed_robots: Query<
        (
            &mut SimAnimator<Transform>,
            &RobotProps,
            &RobotState,
            &BusyRobot,
        ),
        Changed<RobotProps>,
    >,
) {
    for (mut animator, props, state, busy) in changed_robots.iter_mut() {
        // Generate a new tween for the same motion as the robot was doing.
        let mut new_tween = get_tween(busy.command, props, state);

        // Get the old tween's progress, and apply it to the new tween
        let old_progress = animator.tweenable.progress();
        new_tween.set_progress(old_progress);

        // Replace the old tween with the new tween
        animator.set_tweenable(new_tween);
    }
}
