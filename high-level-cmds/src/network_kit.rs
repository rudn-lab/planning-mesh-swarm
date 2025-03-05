use core::marker::PhantomData;

use crate::AsyncUtils;

/// Received Signal Strength indicator: a number that represents how good the signal is.
/// Higher is better.
pub struct RSSI(pub u8);

impl RSSI {
    pub fn from_float(f: f32) -> Self {
        Self((f.clamp(0.0, 1.0) * 255.0) as u8)
    }
}

/// This struct represents a robot's network kit.
pub struct NetworkKit<PeerId, MessageType, Recv, Send, AsyncUtilsImpl, const SENDERS: usize>
where
    Recv: ReceiverNic<PeerId, MessageType>,
    Send: TransmitterNic<PeerId, MessageType>,
    AsyncUtilsImpl: AsyncUtils,
{
    /// The NIC for receiving messages.
    pub receiver: Recv,

    /// The NICs for sending messages to other peers.
    /// Should be at least one.
    pub senders: [Send; SENDERS],

    /// The async utils implementation.
    pub utils: AsyncUtilsImpl,

    pub _peer_id_type: PhantomData<PeerId>,
    pub _message_type: PhantomData<MessageType>,
}

impl<PeerId, MessageType, Recv, Send, AsyncUtilsImpl, const SENDERS: usize>
    NetworkKit<PeerId, MessageType, Recv, Send, AsyncUtilsImpl, SENDERS>
where
    Recv: ReceiverNic<PeerId, MessageType>,
    Send: TransmitterNic<PeerId, MessageType>,
    AsyncUtilsImpl: AsyncUtils,
{
    /// Perform a liveliness check on each sender and return which are alive.
    pub async fn live_senders(&mut self) -> [bool; SENDERS] {
        let mut res = [false; SENDERS];
        for i in 0..SENDERS {
            res[i] = self.senders[i].ping().await.is_ok();
        }
        res
    }
}

/// The NIC that receives messages from many different peers.
/// Physically, this is an AP.
pub trait ReceiverNic<PeerId, MessageType> {
    type Error: core::fmt::Debug;
    /// Receive a single message that has been sent to this receiver.
    /// Note that the NIC driver may have a limited capacity for messages,
    /// so this should be called often to avoid dropping messages.
    ///
    /// Asynchronously blocks until a message is received.
    async fn get(&mut self) -> Result<(PeerId, MessageType), Self::Error>;

    /// Get this receiver's ID.
    /// Other peers will see me by this ID.
    async fn get_id(&mut self) -> Result<PeerId, Self::Error>;
}

/// The NIC that can send messages to a single other receiver, which needs to be paired first.
/// Physically, this is a Wi-Fi STA connected to an AP on the peer receiver.
pub trait TransmitterNic<PeerId, MessageType> {
    type Error: core::fmt::Debug;

    /// Check that the transmitter is alive.
    /// If it is, then most other commands are expected to succeed.
    ///
    /// It might be stuck or missing.
    async fn ping(&mut self) -> Result<(), Self::Error>;

    /// Is the transmitter currently paired with a receiver?
    /// If so, returns its peer ID.
    ///
    /// A transmitter may become unpaired if [`unpair`] is called,
    /// or if the peers go too far out of range to trigger it.
    /// However, depending on how exactly the NIC works, going out of range can also cause a reconnect loop.
    async fn get_peer(&mut self) -> Result<Option<PeerId>, Self::Error>;

    /// Get information about the current peer connection.
    /// If paired and in range, returns the peer's ID and RSSI.
    /// If paired and not in range, returns the peer's ID and an RSSI of zero.
    /// If not paired, will return an error.
    async fn get_connection_info(&mut self) -> Result<ConnectionInfo<PeerId>, Self::Error>;

    /// Scan the air for receivers that we can pair with.
    /// Note that this may not work properly if the transmitter is already paired.
    ///
    /// Writes the newly found peers to the given array, and returns how many were found.
    /// If no peers were found, returns 0 and does not modify the array.
    /// If found more peers than the array can hold, returns a number bigger than the array's length.
    ///
    /// If an error occurs, the state of the array is not defined.
    async fn scan(&mut self, peers: &mut [PeerId]) -> Result<usize, Self::Error>;

    /// Try to pair with the given peer.
    ///
    /// If already paired, returns an error.
    /// If the peer is unavailable, returns an error.
    /// If success is returned, then the pairing was successful,
    /// and (at least for the moment) the transmitter can send messages to the peer.
    async fn pair(&mut self, peer: PeerId) -> Result<(), Self::Error>;

    /// Try to unpair from the current peer.
    /// If not paired, succeeds immediately.
    async fn unpair(&mut self) -> Result<(), Self::Error>;

    /// Send a message to the current peer.
    /// If not paired, returns an error.
    async fn send(&mut self, message: MessageType) -> Result<(), Self::Error>;
}

pub struct ConnectionInfo<PeerId> {
    pub peer_id: PeerId,
    pub rssi: RSSI,
}
