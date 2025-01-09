/// This enum describes the shape of an antenna's coverage.
pub(crate) enum AntennaReach {
    /// The antenna has a rotationally symmetric coverage
    /// (it is a circle).
    /// The signal strength is given by the value of the logistic function
    /// with the argument being the distance from the center of the circle.
    Circular {
        /// The maximum reach allowed, in centimeters. Regardless of the logistic parameters,
        /// any point beyond this radius has a zero signal strength.
        max_reach: f32,

        /// In the logistic function y=1/(1+exp(-k*(x-x0))), this is the x0 parameter.
        /// This shifts the function left and right.
        x0: f32,

        /// In the logistic function y=1/(1+exp(-k*(x-x0))), this is the k parameter.
        /// This changes the slope.
        ///
        /// If it is positive, the function slopes upwards,
        /// so the signal strength gets higher as you move further away from the source:
        /// this is probably not what you want.
        ///
        /// If it is exactly zero, then the signal strength is uniformly equal to half the maximum,
        /// within `max_reach`.
        k: f32,
    },
}
