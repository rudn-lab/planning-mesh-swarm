/// Represents a message that can be sent between robots.
pub trait MessageKind: core::fmt::Debug + Clone + Send + Sync + 'static {
    /// Construct a message that does nothing but ping the peer.
    fn ping() -> Self;
}
