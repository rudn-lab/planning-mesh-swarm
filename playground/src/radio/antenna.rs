use bevy::math::Vec2;

/// This enum describes the shape of an antenna's coverage.
#[derive(Copy, Clone, Debug)]
pub(crate) enum AntennaReach {
    /// The antenna has a rotationally symmetric coverage
    /// (it is a circle).
    /// The signal reach follows a smooth cubic curve
    /// of the form:
    /// S(d) = max(0, r*r - d*d)^3
    Circular {
        /// The maximum reach allowed, in centimeters. Regardless of the parameters,
        /// any point beyond this radius has a zero signal strength.
        max_reach: f32,
    },
}

impl AntennaReach {
    /// If this antenna is a signal emitter, then this function
    /// returns the strength of the received signal at the target position.
    /// The signal strength is a number from 0 to 1,
    /// where 0 is no signal and 1 is maximum signal strength.
    pub(crate) fn get_signal_strength(&self, my_position: Vec2, target_position: Vec2) -> f32 {
        match self {
            AntennaReach::Circular { max_reach } => {
                let distance = (target_position - my_position).length();
                if distance > *max_reach {
                    0.0
                } else {
                    let relative_distance = distance / *max_reach;
                    let value = f32::max(0.0, 1.0 - f32::powi(relative_distance, 2));
                    value.powi(3)
                }
            }
        }
    }
}
