use std::{
    ops::{Add, Sub},
    time::Duration,
};

use bevy::{
    app::{First, Plugin},
    ecs::{
        schedule::IntoSystemConfigs,
        system::{Res, ResMut},
    },
    time::{Time, TimeSystem, Virtual},
};

/// This clock represents the time inside the simulation.
/// It is paused and unpaused when the spacebar is pressed.
pub(crate) struct Simulation {
    pub(crate) paused: bool,
    pub(crate) speed: f32,
}

impl Default for Simulation {
    fn default() -> Self {
        Self {
            paused: false,
            speed: 1.0,
        }
    }
}

/// This represents a moment in simulation time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SimulationInstant(core::time::Duration);

impl SimulationInstant {
    pub(crate) fn into_inner(self) -> core::time::Duration {
        self.0
    }
}
impl Sub for SimulationInstant {
    type Output = core::time::Duration;
    fn sub(self, other: Self) -> Self::Output {
        self.0 - other.0
    }
}

impl Add<core::time::Duration> for SimulationInstant {
    type Output = Self;
    fn add(self, other: core::time::Duration) -> Self::Output {
        Self(self.0 + other)
    }
}

pub(crate) struct SimulationClockPlugin;

impl Plugin for SimulationClockPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.insert_resource(Time::new_with(Simulation::default()));

        app.add_systems(First, update_simulation_time.after(TimeSystem));
    }
}

fn update_simulation_time(base: Res<Time<Virtual>>, mut my_clock: ResMut<Time<Simulation>>) {
    let speed = my_clock.context().speed;
    let paused = my_clock.context().paused;

    if !paused {
        my_clock.advance_by(base.delta().mul_f32(speed));
    } else {
        my_clock.advance_by(Duration::default());
    }
}

pub(crate) trait SimulationTime {
    fn set_paused(&mut self, paused: bool);
    fn set_speed(&mut self, speed: f32);
    fn get_instant(&self) -> SimulationInstant;
}

impl SimulationTime for Time<Simulation> {
    fn set_paused(&mut self, paused: bool) {
        self.context_mut().paused = paused;
    }

    fn set_speed(&mut self, speed: f32) {
        self.context_mut().speed = speed;
    }

    fn get_instant(&self) -> SimulationInstant {
        SimulationInstant(self.elapsed())
    }
}
