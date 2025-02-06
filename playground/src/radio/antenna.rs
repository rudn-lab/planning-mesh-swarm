/// This enum describes the shape of an antenna's coverage.
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
